Commands

====================

py-smart-operator-check
-----------------------
Check, if smart-operator-mode is loaded resp. available.

Give some hints, if not.

py-autopair-check
-----------------
Check, if autopair-mode is available.

Give some hints, if not.

py-choose-shell-by-shebang
--------------------------
Choose shell by looking at #! on the first line.

If SHEBANG is non-nil, returns the shebang as string,
otherwise the Python resp. Jython shell command name. 

py-choose-shell-by-path
-----------------------
Select Python executable according to version desplayed in path, current buffer-file is selected from.

Returns versioned string, nil if nothing appropriate found 

py-which-python
---------------
Returns version of Python of current environment, a number. 

py-python-current-environment
-----------------------------
Returns path of current Python installation. 

py-choose-shell
---------------
Return an appropriate executable as a string.

Returns nil, if no executable found.

This does the following:
 - look for an interpreter with `py-choose-shell-by-shebang'
 - examine imports using `py--choose-shell-by-import'
 - look if Path/To/File indicates a Python version
 - if not successful, return default value of `py-shell-name'

When interactivly called, messages the shell name, Emacs would in the given circtumstances.

With C-u 4 is called `py-switch-shell' see docu there.

py-install-directory-check
--------------------------
Do some sanity check for `py-install-directory'.

Returns `t' if successful. 

py-guess-py-install-directory
-----------------------------
Takes value of user directory aka $HOME
if `(locate-library "python-mode")' is not succesful.

Used only, if `py-install-directory' is empty. 

py-load-pymacs
--------------
Load Pymacs as delivered with python-mode.el.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.ca

py-set-load-path
----------------
Include needed subdirs of python-mode directory. 

py-count-lines
--------------
Count lines in accessible part until current line.

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=7115

py-toggle-highlight-indentation
-------------------------------
If `highlight-indentation-p' should be on or off. 

py-highlight-indentation-off
----------------------------
If `highlight-indentation-p' should be on or off. 

py-highlight-indentation-on
---------------------------
If `highlight-indentation-p' should be on or off. 

py-toggle-smart-indentation
---------------------------
If `py-smart-indentation' should be on or off.

Returns value of `py-smart-indentation' switched to. 

py-smart-indentation-on
-----------------------
Make sure, `py-smart-indentation' is on.

Returns value of `py-smart-indentation'. 

py-smart-indentation-off
------------------------
Make sure, `py-smart-indentation' is off.

Returns value of `py-smart-indentation'. 

py-toggle-sexp-function
-----------------------
Opens customization 

py-toggle-autopair-mode
-----------------------
If `py-autopair-mode' should be on or off.

  Returns value of `py-autopair-mode' switched to. 

py-autopair-mode-on
-------------------
Make sure, py-autopair-mode' is on.

Returns value of `py-autopair-mode'. 

py-autopair-mode-off
--------------------
Make sure, py-autopair-mode' is off.

Returns value of `py-autopair-mode'. 

toggle-py-smart-operator-mode-p
-------------------------------
If `py-smart-operator-mode-p' should be on or off.

  Returns value of `py-smart-operator-mode-p' switched to. 

py-smart-operator-mode-p-on
---------------------------
Make sure, py-smart-operator-mode-p' is on.

Returns value of `py-smart-operator-mode-p'. 

py-smart-operator-mode-p-off
----------------------------
Make sure, py-smart-operator-mode-p' is off.

Returns value of `py-smart-operator-mode-p'. 

toggle-py-switch-buffers-on-execute-p
-------------------------------------
If `py-switch-buffers-on-execute-p' should be on or off.

  Returns value of `py-switch-buffers-on-execute-p' switched to. 

py-switch-buffers-on-execute-p-on
---------------------------------
Make sure, `py-py-switch-buffers-on-execute-p' is on.

Returns value of `py-switch-buffers-on-execute-p'. 

py-switch-buffers-on-execute-p-off
----------------------------------
Make sure, `py-switch-buffers-on-execute-p' is off.

Returns value of `py-switch-buffers-on-execute-p'. 

toggle-py-split-window-on-execute
---------------------------------
If `py-split-window-on-execute' should be on or off.

  Returns value of `py-split-window-on-execute' switched to. 

py-split-window-on-execute-on
-----------------------------
Make sure, `py-py-split-window-on-execute' is on.

Returns value of `py-split-window-on-execute'. 

py-split-window-on-execute-off
------------------------------
Make sure, `py-split-window-on-execute' is off.

Returns value of `py-split-window-on-execute'. 

toggle-py-fontify-shell-buffer-p
--------------------------------
If `py-fontify-shell-buffer-p' should be on or off.

  Returns value of `py-fontify-shell-buffer-p' switched to. 

py-fontify-shell-buffer-p-on
----------------------------
Make sure, `py-py-fontify-shell-buffer-p' is on.

Returns value of `py-fontify-shell-buffer-p'. 

py-fontify-shell-buffer-p-off
-----------------------------
Make sure, `py-fontify-shell-buffer-p' is off.

Returns value of `py-fontify-shell-buffer-p'. 

toggle-python-mode-v5-behavior-p
--------------------------------
If `python-mode-v5-behavior-p' should be on or off.

  Returns value of `python-mode-v5-behavior-p' switched to. 

python-mode-v5-behavior-p-on
----------------------------
Make sure, `python-mode-v5-behavior-p' is on.

Returns value of `python-mode-v5-behavior-p'. 

python-mode-v5-behavior-p-off
-----------------------------
Make sure, `python-mode-v5-behavior-p' is off.

Returns value of `python-mode-v5-behavior-p'. 

toggle-py-jump-on-exception
---------------------------
If `py-jump-on-exception' should be on or off.

  Returns value of `py-jump-on-exception' switched to. 

py-jump-on-exception-on
-----------------------
Make sure, py-jump-on-exception' is on.

Returns value of `py-jump-on-exception'. 

py-jump-on-exception-off
------------------------
Make sure, `py-jump-on-exception' is off.

Returns value of `py-jump-on-exception'. 

toggle-py-use-current-dir-when-execute-p
----------------------------------------
If `py-use-current-dir-when-execute-p' should be on or off.

  Returns value of `py-use-current-dir-when-execute-p' switched to. 

py-use-current-dir-when-execute-p-on
------------------------------------
Make sure, py-use-current-dir-when-execute-p' is on.

Returns value of `py-use-current-dir-when-execute-p'. 

py-use-current-dir-when-execute-p-off
-------------------------------------
Make sure, `py-use-current-dir-when-execute-p' is off.

Returns value of `py-use-current-dir-when-execute-p'. 

toggle-py-electric-comment-p
----------------------------
If `py-electric-comment-p' should be on or off.

  Returns value of `py-electric-comment-p' switched to. 

py-electric-comment-p-on
------------------------
Make sure, py-electric-comment-p' is on.

Returns value of `py-electric-comment-p'. 

py-electric-comment-p-off
-------------------------
Make sure, `py-electric-comment-p' is off.

Returns value of `py-electric-comment-p'. 

toggle-py-underscore-word-syntax-p
----------------------------------
If `py-underscore-word-syntax-p' should be on or off.

  Returns value of `py-underscore-word-syntax-p' switched to. 

py-underscore-word-syntax-p-on
------------------------------
Make sure, py-underscore-word-syntax-p' is on.

Returns value of `py-underscore-word-syntax-p'. 

py-underscore-word-syntax-p-off
-------------------------------
Make sure, `py-underscore-word-syntax-p' is off.

Returns value of `py-underscore-word-syntax-p'. 

py-insert-default-shebang
-------------------------
Insert in buffer shebang of installed default Python. 

py-indent-line-outmost
----------------------
Indent the current line to the outmost reasonable indent.

With optional C-u an indent with length `py-indent-offset' is inserted unconditionally 

py-indent-line
--------------
Indent the current line according to Python rules.

When called interactivly with C-u, ignore dedenting rules for block closing statements
(e.g. return, raise, break, continue, pass)

An optional C-u followed by a numeric argument neither 1 nor 4 will switch off `py-smart-indentation' for this execution. This permits to correct allowed but unwanted indents.
Similar to `toggle-py-smart-indentation' resp. `py-smart-indentation-off' followed by TAB.

This function is normally used by `indent-line-function' resp.
TAB.

When bound to TAB, C-q TAB inserts a TAB.

OUTMOST-ONLY stops circling possible indent.

When `py-tab-shifts-region-p' is `t', not just the current line,
but the region is shiftet that way.

If `py-tab-indents-region-p' is `t' and first TAB doesn't shift
--as indent is at outmost reasonable--, indent-region is called.

C-q TAB inserts a literal TAB-character.

py-newline-and-indent
---------------------
Add a newline and indent to outmost reasonable indent.
When indent is set back manually, this is honoured in following lines. 

py-newline-and-dedent
---------------------
Add a newline and indent to one level below current.
Returns column. 

py-toggle-indent-tabs-mode
--------------------------
Toggle `indent-tabs-mode'.

Returns value of `indent-tabs-mode' switched to. 

py-indent-tabs-mode
-------------------
With positive ARG switch `indent-tabs-mode' on.

With negative ARG switch `indent-tabs-mode' off.
Returns value of `indent-tabs-mode' switched to. 

py-indent-tabs-mode-on
----------------------
Switch `indent-tabs-mode' on. 

py-indent-tabs-mode-off
-----------------------
Switch `indent-tabs-mode' off. 

py-guess-indent-offset
----------------------
Guess `py-indent-offset'.

Set local value of `py-indent-offset', return it

Might change local value of `py-indent-offset' only when called
downwards from beginning of block followed by a statement. Otherwise default-value is returned.

py-indent-and-forward
---------------------
Indent current line according to mode, move one line forward. 

py-indent-region
----------------
Reindent a region of Python code.

In case first line accepts an indent, keep the remaining
lines relative.
Otherwise lines in region get outmost indent,
same with optional argument

In order to shift a chunk of code, where the first line is okay, start with second line.


py-backward-declarations
------------------------
Got to the beginning of assigments resp. statements in current level which don't open blocks.


py-forward-declarations
-----------------------
Got to the end of assigments resp. statements in current level which don't open blocks. 

py-declarations
---------------
Copy and mark assigments resp. statements in current level which don't open blocks or start with a keyword.

See also `py-statements', which is more general, taking also simple statements starting with a keyword. 

py-kill-declarations
--------------------
Delete variables declared in current level.

Store deleted variables in kill-ring 

py--bounds-of-statements
------------------------
Bounds of consecutive multitude of statements around point.

Indented same level, which don't open blocks. 

py-backward-statements
----------------------
Got to the beginning of statements in current level which don't open blocks. 

py-forward-statements
---------------------
Got to the end of statements in current level which don't open blocks. 

py-statements
-------------
Copy and mark simple statements in current level which don't open blocks.

More general than py-declarations, which would stop at keywords like a print-statement. 

py-kill-statements
------------------
Delete statements declared in current level.

Store deleted statements in kill-ring 

py-insert-super
---------------
Insert a function "super()" from current environment.

As example given in Python v3.1 documentation » The Python Standard Library »

class C(B):
    def method(self, arg):
        super().method(arg) # This does the same thing as:
                               # super(C, self).method(arg)

Returns the string inserted. 

py-comment-region
-----------------
Like `comment-region' but uses double hash (`#') comment starter.

py-delete-comments-in-def-or-class
----------------------------------
Delete all commented lines in def-or-class at point

py-delete-comments-in-class
---------------------------
Delete all commented lines in class at point

py-delete-comments-in-block
---------------------------
Delete all commented lines in block at point

py-delete-comments-in-region
----------------------------
Delete all commented lines in region. 

py-backward-block
-----------------
Go to beginning of block.

If already at beginning, go one block backward.
Returns beginning of block if successful, nil otherwise



py-backward-block-or-clause
---------------------------
Go to beginning of block-or-clause.

If already at beginning, go one block-or-clause backward.
Returns beginning of block-or-clause if successful, nil otherwise



py-backward-class
-----------------
Go to beginning of class.

If already at beginning, go one class backward.
Returns beginning of class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too. 

py-backward-clause
------------------
Go to beginning of clause.

If already at beginning, go one clause backward.
Returns beginning of clause if successful, nil otherwise



py-backward-def
---------------
Go to beginning of def.

If already at beginning, go one def backward.
Returns beginning of def if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too. 

py-backward-def-or-class
------------------------
Go to beginning of def-or-class.

If already at beginning, go one def-or-class backward.
Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too. 

py-backward-if-block
--------------------
Go to beginning of if-block.

If already at beginning, go one if-block backward.
Returns beginning of if-block if successful, nil otherwise



py-backward-elif-block
----------------------
Go to beginning of elif-block.

If already at beginning, go one elif-block backward.
Returns beginning of elif-block if successful, nil otherwise



py-backward-else-block
----------------------
Go to beginning of else-block.

If already at beginning, go one else-block backward.
Returns beginning of else-block if successful, nil otherwise



py-backward-for-block
---------------------
Go to beginning of for-block.

If already at beginning, go one for-block backward.
Returns beginning of for-block if successful, nil otherwise



py-backward-except-block
------------------------
Go to beginning of except-block.

If already at beginning, go one except-block backward.
Returns beginning of except-block if successful, nil otherwise



py-backward-try-block
---------------------
Go to beginning of try-block.

If already at beginning, go one try-block backward.
Returns beginning of try-block if successful, nil otherwise



py-backward-line
----------------
Go to beginning of line.

If already at beginning, go one line backward.
Returns beginning of line if successful, nil otherwise



py-backward-minor-block
-----------------------
Go to beginning of minor-block.

If already at beginning, go one minor-block backward.
Returns beginning of minor-block if successful, nil otherwise



py-backward-block-bol
---------------------
Go to beginning of block, go to BOL.

If already at beginning, go one block backward.
Returns beginning of block if successful, nil otherwise



py-backward-block-or-clause-bol
-------------------------------
Go to beginning of block-or-clause, go to BOL.

If already at beginning, go one block-or-clause backward.
Returns beginning of block-or-clause if successful, nil otherwise



py-backward-class-bol
---------------------
Go to beginning of class, go to BOL.

If already at beginning, go one class backward.
Returns beginning of class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too. 

py-backward-clause-bol
----------------------
Go to beginning of clause, go to BOL.

If already at beginning, go one clause backward.
Returns beginning of clause if successful, nil otherwise



py-backward-def-bol
-------------------
Go to beginning of def, go to BOL.

If already at beginning, go one def backward.
Returns beginning of def if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too. 

py-backward-def-or-class-bol
----------------------------
Go to beginning of def-or-class, go to BOL.

If already at beginning, go one def-or-class backward.
Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too. 

py-backward-elif-block-bol
--------------------------
Go to beginning of elif-block, go to BOL.

If already at beginning, go one elif-block backward.
Returns beginning of elif-block if successful, nil otherwise



py-backward-else-block-bol
--------------------------
Go to beginning of else-block, go to BOL.

If already at beginning, go one else-block backward.
Returns beginning of else-block if successful, nil otherwise



py-backward-except-block-bol
----------------------------
Go to beginning of except-block, go to BOL.

If already at beginning, go one except-block backward.
Returns beginning of except-block if successful, nil otherwise



py-backward-for-block-bol
-------------------------
Go to beginning of for-block, go to BOL.

If already at beginning, go one for-block backward.
Returns beginning of for-block if successful, nil otherwise



py-backward-if-block-bol
------------------------
Go to beginning of if-block, go to BOL.

If already at beginning, go one if-block backward.
Returns beginning of if-block if successful, nil otherwise



py-backward-minor-block-bol
---------------------------
Go to beginning of minor-block, go to BOL.

If already at beginning, go one minor-block backward.
Returns beginning of minor-block if successful, nil otherwise



py-backward-statement-bol
-------------------------
Goto beginning of line where statement starts.
  Returns position reached, if successful, nil otherwise.

See also `py-up-statement': up from current definition to next beginning of statement above. 

py-backward-try-block-bol
-------------------------
Go to beginning of try-block, go to BOL.

If already at beginning, go one try-block backward.
Returns beginning of try-block if successful, nil otherwise



py-forward-block
----------------
Go to end of block.

Returns end of block if successful, nil otherwise

py-forward-block-bol
--------------------
Goto beginning of line following end of block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-block': down from current definition to next beginning of block below. 

py-forward-block-or-clause
--------------------------
Go to end of block-or-clause.

Returns end of block-or-clause if successful, nil otherwise

py-forward-block-or-clause-bol
------------------------------
Goto beginning of line following end of block-or-clause.
  Returns position reached, if successful, nil otherwise.

See also `py-down-block-or-clause': down from current definition to next beginning of block-or-clause below. 

py-forward-class
----------------
Go to end of class.

Returns end of class if successful, nil otherwise

py-forward-class-bol
--------------------
Goto beginning of line following end of class.
  Returns position reached, if successful, nil otherwise.

See also `py-down-class': down from current definition to next beginning of class below. 

py-forward-clause
-----------------
Go to end of clause.

Returns end of clause if successful, nil otherwise

py-forward-clause-bol
---------------------
Goto beginning of line following end of clause.
  Returns position reached, if successful, nil otherwise.

See also `py-down-clause': down from current definition to next beginning of clause below. 

py-forward-def
--------------
Go to end of def.

Returns end of def if successful, nil otherwise

py-forward-def-bol
------------------
Goto beginning of line following end of def.
  Returns position reached, if successful, nil otherwise.

See also `py-down-def': down from current definition to next beginning of def below. 

py-forward-def-or-class
-----------------------
Go to end of def-or-class.

Returns end of def-or-class if successful, nil otherwise

py-forward-def-or-class-bol
---------------------------
Goto beginning of line following end of def-or-class.
  Returns position reached, if successful, nil otherwise.

See also `py-down-def-or-class': down from current definition to next beginning of def-or-class below. 

py-forward-if-block
-------------------
Go to end of if-block.

Returns end of if-block if successful, nil otherwise

py-forward-if-block-bol
-----------------------
Goto beginning of line following end of if-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-if-block': down from current definition to next beginning of if-block below. 

py-forward-elif-block
---------------------
Go to end of elif-block.

Returns end of elif-block if successful, nil otherwise

py-forward-elif-block-bol
-------------------------
Goto beginning of line following end of elif-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-elif-block': down from current definition to next beginning of elif-block below. 

py-forward-else-block
---------------------
Go to end of else-block.

Returns end of else-block if successful, nil otherwise

py-forward-else-block-bol
-------------------------
Goto beginning of line following end of else-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-else-block': down from current definition to next beginning of else-block below. 

py-forward-for-block
--------------------
Go to end of for-block.

Returns end of for-block if successful, nil otherwise

py-forward-for-block-bol
------------------------
Goto beginning of line following end of for-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-for-block': down from current definition to next beginning of for-block below. 

py-forward-except-block
-----------------------
Go to end of except-block.

Returns end of except-block if successful, nil otherwise

py-forward-except-block-bol
---------------------------
Goto beginning of line following end of except-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-except-block': down from current definition to next beginning of except-block below. 

py-forward-try-block
--------------------
Go to end of try-block.

Returns end of try-block if successful, nil otherwise

py-forward-try-block-bol
------------------------
Goto beginning of line following end of try-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-try-block': down from current definition to next beginning of try-block below. 

py-forward-line
---------------
Goes to end of line after forward move.

Travels right-margin comments. 

py-forward-line-bol
-------------------
Goto beginning of line following end of line.
  Returns position reached, if successful, nil otherwise.

See also `py-down-line': down from current definition to next beginning of line below. 

py-forward-minor-block
----------------------
Go to end of minor-block.

Returns end of minor-block if successful, nil otherwise

py-forward-minor-block-bol
--------------------------
Goto beginning of line following end of minor-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-minor-block': down from current definition to next beginning of minor-block below. 

py-backward-expression
----------------------
Go to the beginning of a python expression.

If already at the beginning or before a expression, go to next expression in buffer upwards

py-forward-expression
---------------------
Go to the end of a compound python expression.

Operators are ignored. 

py-beginning-of-line
--------------------
Go to beginning-of-line, return position.

If already at beginning-of-line and not at BOB, go to beginning of previous line. 

py-end-of-line
--------------
Go to end-of-line, return position.

If already at end-of-line and not at EOB, go to end of next line. 

py-backward-statement
---------------------
Go to the initial line of a simple statement.

For beginning of compound statement use py-backward-block.
For beginning of clause py-backward-clause.

py-forward-statement
--------------------
Go to the last char of current statement.

Optional argument REPEAT, the number of loops done already, is checked for py-max-specpdl-size error. Avoid eternal loops due to missing string delimters etc. 

py-forward-statement-bol
------------------------
Go to the beginning-of-line following current statement.

py-backward-decorator
---------------------
Go to the beginning of a decorator.

Returns position if succesful 

py-forward-decorator
--------------------
Go to the end of a decorator.

Returns position if succesful 

py-go-to-beginning-of-comment
-----------------------------
Go to the beginning of current line's comment, if any.

From a programm use macro `py-backward-comment' instead 

py-leave-comment-or-string-backward
-----------------------------------
If inside a comment or string, leave it backward. 

py-beginning-of-list-pps
------------------------
Go to the beginning of a list.
Optional ARG indicates a start-position for `parse-partial-sexp'.
Return beginning position, nil if not inside.

py-forward-into-nomenclature
----------------------------
Move forward to end of a nomenclature symbol.

With C-u (programmatically, optional argument ARG), do it that many times.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores.

py-backward-into-nomenclature
-----------------------------
Move backward to beginning of a nomenclature symbol.

With optional ARG, move that many times.  If ARG is negative, move
forward.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores.

py-beginning-of-block-current-column
------------------------------------
Reach next beginning of block upwards which starts at current column.

Return position

py-backward-section
-------------------
Go to next section start upward in buffer.

Return position if successful

py-forward-section
------------------
Go to next section end downward in buffer.

Return position if successful

py-kill-block
-------------
Delete `block' at point.

Stores data in kill ring

py-kill-block-or-clause
-----------------------
Delete `block-or-clause' at point.

Stores data in kill ring

py-kill-class
-------------
Delete `class' at point.

Stores data in kill ring

py-kill-clause
--------------
Delete `clause' at point.

Stores data in kill ring

py-kill-def
-----------
Delete `def' at point.

Stores data in kill ring

py-kill-def-or-class
--------------------
Delete `def-or-class' at point.

Stores data in kill ring

py-kill-elif-block
------------------
Delete `elif-block' at point.

Stores data in kill ring

py-kill-else-block
------------------
Delete `else-block' at point.

Stores data in kill ring

py-kill-except-block
--------------------
Delete `except-block' at point.

Stores data in kill ring

py-kill-expression
------------------
Delete `expression' at point.

Stores data in kill ring

py-kill-for-block
-----------------
Delete `for-block' at point.

Stores data in kill ring

py-kill-if-block
----------------
Delete `if-block' at point.

Stores data in kill ring

py-kill-minor-block
-------------------
Delete `minor-block' at point.

Stores data in kill ring

py-kill-partial-expression
--------------------------
Delete `partial-expression' at point.

Stores data in kill ring

py-kill-section
---------------
Delete `section' at point.

Stores data in kill ring

py-kill-statement
-----------------
Delete `statement' at point.

Stores data in kill ring

py-kill-top-level
-----------------
Delete `top-level' at point.

Stores data in kill ring

py-kill-try-block
-----------------
Delete `try-block' at point.

Stores data in kill ring

py-kill-block-bol
-----------------
Delete block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-clause-bol
------------------
Delete clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-block-or-clause-bol
---------------------------
Delete block-or-clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-def-bol
---------------
Delete def bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-class-bol
-----------------
Delete class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-def-or-class-bol
------------------------
Delete def-or-class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-if-block-bol
--------------------
Delete if-block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-try-block-bol
---------------------
Delete try-block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-minor-block-bol
-----------------------
Delete minor-block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-for-block-bol
---------------------
Delete for-block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-top-level-bol
---------------------
Delete top-level bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-statement-bol
---------------------
Delete statement bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-mark-block
-------------
Mark block at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-block-or-clause
-----------------------
Mark block-or-clause at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-class
-------------
Mark class at point.

With C-u or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of marked area, a cons. 

py-mark-clause
--------------
Mark clause at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-comment
---------------
Mark comment at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-def
-----------
Mark def at point.

With C-u or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of marked area, a cons. 

py-mark-def-or-class
--------------------
Mark def-or-class at point.

With C-u or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of marked area, a cons. 

py-mark-except-block
--------------------
Mark except-block at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-expression
------------------
Mark expression at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-if-block
----------------
Mark if-block at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-line
------------
Mark line at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-minor-block
-------------------
Mark minor-block at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-paragraph
-----------------
Mark paragraph at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-partial-expression
--------------------------
Mark partial-expression at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-section
---------------
Mark section at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-statement
-----------------
Mark statement at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-top-level
-----------------
Mark top-level at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-try-block
-----------------
Mark try-block at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-block-bol
-----------------
Mark block, take beginning of line positions. 

Returns beginning and end positions of region, a cons. 

py-mark-block-or-clause-bol
---------------------------
Mark block-or-clause, take beginning of line positions. 

Returns beginning and end positions of region, a cons. 

py-mark-class-bol
-----------------
Mark class, take beginning of line positions. 

With C-u or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of region, a cons. 

py-mark-clause-bol
------------------
Mark clause, take beginning of line positions. 

Returns beginning and end positions of region, a cons. 

py-mark-comment-bol
-------------------
Mark comment, take beginning of line positions. 

Returns beginning and end positions of region, a cons. 

py-mark-def-bol
---------------
Mark def, take beginning of line positions. 

With C-u or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of region, a cons. 

py-mark-def-or-class-bol
------------------------
Mark def-or-class, take beginning of line positions. 

With C-u or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of region, a cons. 

py-mark-except-block-bol
------------------------
Mark except-block, take beginning of line positions. 

Returns beginning and end positions of region, a cons. 

py-mark-expression-bol
----------------------
Mark expression, take beginning of line positions. 

Returns beginning and end positions of region, a cons. 

py-mark-if-block-bol
--------------------
Mark if-block, take beginning of line positions. 

Returns beginning and end positions of region, a cons. 

py-mark-line-bol
----------------
Mark line, take beginning of line positions. 

Returns beginning and end positions of region, a cons. 

py-mark-minor-block-bol
-----------------------
Mark minor-block, take beginning of line positions. 

Returns beginning and end positions of region, a cons. 

py-mark-paragraph-bol
---------------------
Mark paragraph, take beginning of line positions. 

Returns beginning and end positions of region, a cons. 

py-mark-partial-expression-bol
------------------------------
Mark partial-expression, take beginning of line positions. 

Returns beginning and end positions of region, a cons. 

py-mark-statement-bol
---------------------
Mark statement, take beginning of line positions. 

Returns beginning and end positions of region, a cons. 

py-mark-top-level-bol
---------------------
Mark top-level, take beginning of line positions. 

Returns beginning and end positions of region, a cons. 

py-mark-try-block-bol
---------------------
Mark try-block, take beginning of line positions. 

Returns beginning and end positions of region, a cons. 

py-copy-block
-------------
Copy block at point.

Store data in kill ring, so it might yanked back. 

py-copy-block-or-clause
-----------------------
Copy block-or-clause at point.

Store data in kill ring, so it might yanked back. 

py-copy-class
-------------
Copy class at point.

Store data in kill ring, so it might yanked back. 

py-copy-clause
--------------
Copy clause at point.

Store data in kill ring, so it might yanked back. 

py-copy-def
-----------
Copy def at point.

Store data in kill ring, so it might yanked back. 

py-copy-def-or-class
--------------------
Copy def-or-class at point.

Store data in kill ring, so it might yanked back. 

py-copy-expression
------------------
Copy expression at point.

Store data in kill ring, so it might yanked back. 

py-copy-line
------------
Copy line at point.

Store data in kill ring, so it might yanked back. 

py-copy-minor-block
-------------------
Copy minor-block at point.

Store data in kill ring, so it might yanked back. 

py-copy-paragraph
-----------------
Copy paragraph at point.

Store data in kill ring, so it might yanked back. 

py-copy-partial-expression
--------------------------
Copy partial-expression at point.

Store data in kill ring, so it might yanked back. 

py-copy-statement
-----------------
Copy statement at point.

Store data in kill ring, so it might yanked back. 

py-copy-top-level
-----------------
Copy top-level at point.

Store data in kill ring, so it might yanked back. 

py-copy-block-bol
-----------------
Delete block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-copy-block-or-clause-bol
---------------------------
Delete block-or-clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-copy-class-bol
-----------------
Delete class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-copy-clause-bol
------------------
Delete clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-copy-def-bol
---------------
Delete def bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-copy-def-or-class-bol
------------------------
Delete def-or-class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-copy-expression-bol
----------------------
Delete expression bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-copy-line-bol
----------------
Delete line bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-copy-minor-block-bol
-----------------------
Delete minor-block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-copy-paragraph-bol
---------------------
Delete paragraph bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-copy-partial-expression-bol
------------------------------
Delete partial-expression bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-copy-statement-bol
---------------------
Delete statement bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-copy-top-level-bol
---------------------
Delete top-level bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-delete-block
---------------
Delete BLOCK at point.

Don't store data in kill ring. 

py-delete-block-or-clause
-------------------------
Delete BLOCK-OR-CLAUSE at point.

Don't store data in kill ring. 

py-delete-class
---------------
Delete CLASS at point.

Don't store data in kill ring. 
With C-u or `py-mark-decorators' set to `t', `decorators' are included.

py-delete-clause
----------------
Delete CLAUSE at point.

Don't store data in kill ring. 

py-delete-def
-------------
Delete DEF at point.

Don't store data in kill ring. 
With C-u or `py-mark-decorators' set to `t', `decorators' are included.

py-delete-def-or-class
----------------------
Delete DEF-OR-CLASS at point.

Don't store data in kill ring. 
With C-u or `py-mark-decorators' set to `t', `decorators' are included.

py-delete-expression
--------------------
Delete EXPRESSION at point.

Don't store data in kill ring. 

py-delete-line
--------------
Delete LINE at point.

Don't store data in kill ring. 

py-delete-minor-block
---------------------
Delete MINOR-BLOCK at point.

Don't store data in kill ring. 

py-delete-paragraph
-------------------
Delete PARAGRAPH at point.

Don't store data in kill ring. 

py-delete-partial-expression
----------------------------
Delete PARTIAL-EXPRESSION at point.

Don't store data in kill ring. 

py-delete-statement
-------------------
Delete STATEMENT at point.

Don't store data in kill ring. 

py-delete-top-level
-------------------
Delete TOP-LEVEL at point.

Don't store data in kill ring. 

py-delete-block-bol
-------------------
Delete BLOCK at point until beginning-of-line.

Don't store data in kill ring. 

py-delete-block-or-clause-bol
-----------------------------
Delete BLOCK-OR-CLAUSE at point until beginning-of-line.

Don't store data in kill ring. 

py-delete-class-bol
-------------------
Delete CLASS at point until beginning-of-line.

Don't store data in kill ring. 
With C-u or `py-mark-decorators' set to `t', `decorators' are included.

py-delete-clause-bol
--------------------
Delete CLAUSE at point until beginning-of-line.

Don't store data in kill ring. 

py-delete-def-bol
-----------------
Delete DEF at point until beginning-of-line.

Don't store data in kill ring. 
With C-u or `py-mark-decorators' set to `t', `decorators' are included.

py-delete-def-or-class-bol
--------------------------
Delete DEF-OR-CLASS at point until beginning-of-line.

Don't store data in kill ring. 
With C-u or `py-mark-decorators' set to `t', `decorators' are included.

py-delete-elif-block-bol
------------------------
Delete ELIF-BLOCK at point until beginning-of-line.

Don't store data in kill ring. 

py-delete-else-block-bol
------------------------
Delete ELSE-BLOCK at point until beginning-of-line.

Don't store data in kill ring. 

py-delete-except-block-bol
--------------------------
Delete EXCEPT-BLOCK at point until beginning-of-line.

Don't store data in kill ring. 

py-delete-for-block-bol
-----------------------
Delete FOR-BLOCK at point until beginning-of-line.

Don't store data in kill ring. 

py-delete-if-block-bol
----------------------
Delete IF-BLOCK at point until beginning-of-line.

Don't store data in kill ring. 

py-delete-minor-block-bol
-------------------------
Delete MINOR-BLOCK at point until beginning-of-line.

Don't store data in kill ring. 

py-delete-paragraph-bol
-----------------------
Delete PARAGRAPH at point until beginning-of-line.

Don't store data in kill ring. 

py-delete-statement-bol
-----------------------
Delete STATEMENT at point until beginning-of-line.

Don't store data in kill ring. 

py-delete-try-block-bol
-----------------------
Delete TRY-BLOCK at point until beginning-of-line.

Don't store data in kill ring. 

py-switch-to-python
-------------------
Switch to Python process buffer.

py-send-file
------------
Send FILE-NAME to Python PROCESS.
If TEMP-FILE-NAME is passed then that file is used for processing
instead, while internally the shell will continue to use
FILE-NAME.

toggle-force-local-shell
------------------------
If locally indicated Python shell should be taken and
enforced upon sessions execute commands.

Toggles boolean `py-force-local-shell-p' along with `py-force-py-shell-name-p'
Returns value of `toggle-force-local-shell' switched to.

When on, kind of an option 'follow', local shell sets `py-shell-name', enforces its use afterwards.

See also commands
`py-force-local-shell-on'
`py-force-local-shell-off'
 

py-force-local-shell-on
-----------------------
Make sure, `py-force-local-shell-p' is on.

Returns value of `py-force-local-shell-p'.

Kind of an option 'follow', local shell sets `py-shell-name', enforces its use afterwards 

py-force-local-shell-off
------------------------
Restore `py-shell-name' default value and `behaviour'. 

toggle-force-py-shell-name-p
----------------------------
If customized default `py-shell-name' should be enforced upon execution.

If `py-force-py-shell-name-p' should be on or off.
Returns value of `py-force-py-shell-name-p' switched to.

See also commands
force-py-shell-name-p-on
force-py-shell-name-p-off

Caveat: Completion might not work that way.


force-py-shell-name-p-on
------------------------
Switches `py-force-py-shell-name-p' on.

Customized default `py-shell-name' will be enforced upon execution.
Returns value of `py-force-py-shell-name-p'.

Caveat: Completion might not work that way.


force-py-shell-name-p-off
-------------------------
Make sure, `py-force-py-shell-name-p' is off.

Function to use by executes will be guessed from environment.
Returns value of `py-force-py-shell-name-p'. 

py-toggle-split-windows-on-execute
----------------------------------
If `py-split-window-on-execute' should be on or off.

  Returns value of `py-split-window-on-execute' switched to. 

py-split-windows-on-execute-on
------------------------------
Make sure, `py-split-window-on-execute' is on.

Returns value of `py-split-window-on-execute'. 

py-split-windows-on-execute-off
-------------------------------
Make sure, `py-split-window-on-execute' is off.

Returns value of `py-split-window-on-execute'. 

py-toggle-shell-switch-buffers-on-execute
-----------------------------------------
If `py-switch-buffers-on-execute-p' should be on or off.

  Returns value of `py-switch-buffers-on-execute-p' switched to. 

py-shell-switch-buffers-on-execute-on
-------------------------------------
Make sure, `py-switch-buffers-on-execute-p' is on.

Returns value of `py-switch-buffers-on-execute-p'. 

py-shell-switch-buffers-on-execute-off
--------------------------------------
Make sure, `py-switch-buffers-on-execute-p' is off.

Returns value of `py-switch-buffers-on-execute-p'. 

py-guess-default-python
-----------------------
Defaults to "python", if guessing didn't succeed. 

py-shell-dedicated
------------------
Start an interactive Python interpreter in another window.

With optional C-u user is prompted by
`py-choose-shell' for command and options to pass to the Python
interpreter.


py-set-ipython-completion-command-string
----------------------------------------
Set and return `py-ipython-completion-command-string'. 

py-ipython--module-completion-import
------------------------------------
Import module-completion 

py-toggle-split-window-function
-------------------------------
If window is splitted vertically or horizontally.

When code is executed and `py-split-window-on-execute' is `t', the result is displays in an output-buffer, "*Python*" by default.

Customizable variable `py-split-windows-on-execute-function' tells how to split the screen.

py--manage-windows-split
------------------------
If one window, split according to `py-split-windows-on-execute-function. 

py-kill-shell-unconditional
---------------------------
With optional argument SHELL.

Otherwise kill default (I)Python shell.
Kill buffer and its process.
Receives a buffer-name as argument

py-kill-default-shell-unconditional
-----------------------------------
Kill buffer "*Python*" and its process. 

py-shell
--------
:around advice: `ad-Advice-py-shell'

Start an interactive Python interpreter in another window.
  Interactively, C-u prompts for a new buffer-name.
  C-u 2 prompts for `py-python-command-args'.
  If `default-directory' is a remote file name, it is also prompted
  to change if called with a prefix arg.

  Returns py-shell's buffer-name.
  Optional string PYSHELLNAME overrides default `py-shell-name'.
  BUFFER allows specifying a name, the Python process is connected to
  

(fn &optional ARGPROMPT DEDICATED SHELL BUFFER-NAME FAST-PROCESS EXCEPTION-BUFFER)

py-shell-get-process
--------------------
Get appropriate Python process for current buffer and return it.

py-switch-to-shell
------------------
Switch to Python process buffer.

py-which-execute-file-command
-----------------------------
Return the command appropriate to Python version.

Per default it's "(format "execfile(r'%s') # PYTHON-MODE\n" filename)" for Python 2 series.

py-execute-file
---------------
When called interactively, user is prompted for filename. 

py-execute-string
-----------------
Send the argument STRING to Python default interpreter.

See also `py-execute-region'. 

py-execute-string-dedicated
---------------------------
Send the argument STRING to an unique Python interpreter.

See also `py-execute-region'. 

py-fetch-py-master-file
-----------------------
Lookup if a `py-master-file' is specified.

See also doku of variable `py-master-file' 

py-execute-import-or-reload
---------------------------
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

 - The Python debugger gets line number information about the functions.

py-execute-buffer
-----------------
Send the contents of the buffer to Python default interpreter. 

py-execute-buffer-dedicated
---------------------------
Send buffer to unique interpreter. 

py-execute-buffer-switch
------------------------
Send the contents of the buffer to Python default interpreter and switches to output. 

py-execute-buffer-no-switch
---------------------------
Send the contents of the buffer to Python default interpreter but don't switch to output. 

py-execute-buffer-dedicated-switch
----------------------------------
Send the contents of the buffer to an unique Python interpreter.

Ignores setting of `py-switch-buffers-on-execute-p'. 

py-execute-defun
----------------
Send the current defun (class or method) to the Python process.

py-process-file
---------------
Process "python filename".

Optional OUTPUT-BUFFER and ERROR-BUFFER might be given. 

py-execute-line
---------------
Send current line from beginning of indent to Python interpreter. 

py-remove-overlays-at-point
---------------------------
Remove overlays as set when `py-highlight-error-source-p' is non-nil. 

py-mouseto-exception
--------------------
Jump to the code which caused the Python exception at EVENT.
EVENT is usually a mouse click.

py-goto-exception
-----------------
Go to the line indicated by the traceback.

py-down-exception
-----------------
Go to the next line down in the traceback.
With M-x univeral-argument (programmatically, optional argument
BOTTOM), jump to the bottom (innermost) exception in the exception
stack.

py-up-exception
---------------
Go to the previous line up in the traceback.
With C-u (programmatically, optional argument TOP)
jump to the top (outermost) exception in the exception stack.

py-execute-region
-----------------
Execute region. 

py-execute-region-switch
------------------------
Execute region switch. 

py-execute-region-no-switch
---------------------------
Execute region no-switch. 

py-execute-region-dedicated
---------------------------
Execute region dedicated. 

py-execute-region-python
------------------------
Execute region Python. 

py-execute-region-python-switch
-------------------------------
Execute region Python switch. 

py-execute-region-python-no-switch
----------------------------------
Execute region Python no-switch. 

py-execute-region-python-dedicated
----------------------------------
Execute region Python dedicated. 

py-execute-region-python2
-------------------------
Execute region Python2. 

py-execute-region-python2-switch
--------------------------------
Execute region Python2 switch. 

py-execute-region-python2-no-switch
-----------------------------------
Execute region Python2 no-switch. 

py-execute-region-python2-dedicated
-----------------------------------
Execute region Python2 dedicated. 

py-execute-region-python3
-------------------------
Execute region Python3. 

py-execute-region-python3-switch
--------------------------------
Execute region Python3 switch. 

py-execute-region-python3-no-switch
-----------------------------------
Execute region Python3 no-switch. 

py-execute-region-python3-dedicated
-----------------------------------
Execute region Python3 dedicated. 

py-execute-region-ipython
-------------------------
Execute region IPython. 

py-execute-region-ipython-switch
--------------------------------
Execute region IPython switch. 

py-execute-region-ipython-no-switch
-----------------------------------
Execute region IPython no-switch. 

py-execute-region-ipython-dedicated
-----------------------------------
Execute region IPython dedicated. 

py-execute-region-ipython2\.7
-----------------------------
Execute region IPython2.7. 

py-execute-region-ipython2\.7-switch
------------------------------------
Execute region IPython2.7 switch. 

py-execute-region-ipython2\.7-no-switch
---------------------------------------
Execute region IPython2.7 no-switch. 

py-execute-region-ipython2\.7-dedicated
---------------------------------------
Execute region IPython2.7 dedicated. 

py-execute-region-ipython3
--------------------------
Execute region IPython3. 

py-execute-region-ipython3-switch
---------------------------------
Execute region IPython3 switch. 

py-execute-region-ipython3-no-switch
------------------------------------
Execute region IPython3 no-switch. 

py-execute-region-ipython3-dedicated
------------------------------------
Execute region IPython3 dedicated. 

py-execute-region-jython
------------------------
Execute region Jython. 

py-execute-region-jython-switch
-------------------------------
Execute region Jython switch. 

py-execute-region-jython-no-switch
----------------------------------
Execute region Jython no-switch. 

py-execute-region-jython-dedicated
----------------------------------
Execute region Jython dedicated. 

py-output-buffer-filter
-----------------------
Clear output buffer from py-shell-input prompt etc. 

py-output-filter
----------------
Clear output buffer from py-shell-input prompt etc. 

py-send-string
--------------
Evaluate STRING in Python process.

py-shell-complete
-----------------
Complete word before point, if any. 

py-indent-or-complete
---------------------
Complete or indent depending on the context.

If cursor is at end of a symbol, try to complete
Otherwise call `py-indent-line'

If `(region-active-p)' returns `t', indent region.
Use `C-q TAB' to insert a literally TAB-character

In python-mode `py-complete-function' is called,
in (I)Python shell-modes `py-shell-complete'

py-pdbtrack-toggle-stack-tracking
---------------------------------
Set variable `py-pdbtrack-do-tracking-p'. 

py-execute-statement-pdb
------------------------
Execute statement running pdb. 

py-pdb-help
-----------
Print generic pdb.help() message 

py--pdb-versioned
-----------------
Guess existing pdb version from py-shell-name

Return "pdb[VERSION]" if executable found, just "pdb" otherwise

py-update-gud-pdb-history
-------------------------
If pdb is called at a Python buffer, put it's file name at the head of `gud-pdb-history'. 

py-fetch-docu
-------------
Lookup in current buffer for the doku for the symbol at point.

Useful for newly defined symbol, not known to python yet. 

py-info-current-defun
---------------------
Return name of surrounding function with Python compatible dotted expression syntax.
Optional argument INCLUDE-TYPE indicates to include the type of the defun.
This function is compatible to be used as
`add-log-current-defun-function' since it returns nil if point is
not inside a defun.

py-help-at-point
----------------
Print help on symbol at point.

If symbol is defined in current buffer, jump to it's definition
Optional C-u used for debugging, will prevent deletion of temp file. 

py-describe-mode
----------------
Dump long form of `python-mode' docs.

py-find-definition
------------------
Find source of definition of SYMBOL.

Interactively, prompt for SYMBOL.

py-find-imports
---------------
Find top-level imports.

Returns imports 

py-update-imports
-----------------
Returns imports.

Imports done are displayed in message buffer. 

py-pep8-run
-----------
*Run pep8, check formatting - default on the file currently visited.

py-pep8-help
------------
Display pep8 command line help messages. 

py-pylint-run
-------------
*Run pylint (default on the file currently visited).

For help see M-x pylint-help resp. M-x pylint-long-help.
Home-page: http://www.logilab.org/project/pylint 

py-pylint-help
--------------
Display Pylint command line help messages.

Let's have this until more Emacs-like help is prepared 

py-pylint-doku
--------------
Display Pylint Documentation.

Calls `pylint --full-documentation'

py-pyflakes-run
---------------
*Run pyflakes (default on the file currently visited).

For help see M-x pyflakes-help resp. M-x pyflakes-long-help.
Home-page: http://www.logilab.org/project/pyflakes 

py-pyflakes-help
----------------
Display Pyflakes command line help messages.

Let's have this until more Emacs-like help is prepared 

py-pyflakespep8-run
-------------------
*Run pyflakespep8, check formatting (default on the file currently visited).


py-pyflakespep8-help
--------------------
Display pyflakespep8 command line help messages. 

py-pychecker-run
----------------
*Run pychecker (default on the file currently visited).

py-check-command
----------------
Check a Python file (default current buffer's file).
Runs COMMAND, a shell command, as if by `compile'.
See `python-check-command' for the default.

py-flake8-run
-------------
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

py-flake8-help
--------------
Display flake8 command line help messages. 

py-nesting-level
----------------
Accepts the output of `parse-partial-sexp'. 

py-flycheck-mode
----------------
Toggle `flycheck-mode'.

With negative argument switch off flycheck-mode
See menu "Tools/Syntax Checking"

pylint-flymake-mode
-------------------
Toggle `pylint' `flymake-mode'. 

pyflakes-flymake-mode
---------------------
Toggle `pyflakes' `flymake-mode'. 

pychecker-flymake-mode
----------------------
Toggle `pychecker' `flymake-mode'. 

pep8-flymake-mode
-----------------
Toggle `pep8' `flymake-mode'. 

pyflakespep8-flymake-mode
-------------------------
Toggle `pyflakespep8' `flymake-mode'.

Joint call to pyflakes and pep8 as proposed by
Keegan Carruthers-Smith

variables-state
---------------
Diplays state of python-mode variables in an org-mode buffer.

Reads variables from python-mode.el as current buffer.

Variables which would produce a large output are left out:
- syntax-tables
- python-mode-map

Maybe call M-x describe-variable RET to query its value. 

py-indent-forward-line
----------------------
Indent and move one line forward to next indentation.
Returns column of line reached.

If `py-kill-empty-line' is non-nil, delete an empty line.
When closing a form, use py-close-block et al, which will move and indent likewise.
With M-x universal argument just indent.


py-dedent-forward-line
----------------------
Dedent line and move one line forward. 

py-dedent
---------
Dedent line according to `py-indent-offset'.

With arg, do it that many times.
If point is between indent levels, dedent to next level.
Return indentation reached, if dedent done, nil otherwise.

Affected by `py-dedent-keep-relative-column'. 

py-close-def
------------
Set indent level to that of beginning of function definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. 

py-close-class
--------------
Set indent level to that of beginning of class definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. 

py-close-def-or-class
---------------------
Set indent level to that of beginning of def-or-class definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. 

py-close-clause
---------------
Set indent level to that of beginning of clause definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. 

py-close-block
--------------
Set indent level to that of beginning of block definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. 

py-close-block-or-clause
------------------------
Set indent level to that of beginning of block-or-clause definition.

If final line isn't empty and `py-close-block-or-clause-provides-newline' non-nil, insert a newline. 

py-class-at-point
-----------------
Return class definition as string.

With interactive call, send it to the message buffer too. 

ar-py-function-at-point
-----------------------
Return functions definition as string.

With interactive call, send it to the message buffer too. 

ar-py-beginning-of-function
---------------------------
Jump to the beginning of defun. Returns point. 

ar-py-beginning-of-class
------------------------
Jump to the beginning of class definition. Returns column. 

ar-py-end-of-function
---------------------
Jump to the end of function. 

ar-py-line-at-point
-------------------
Return line as string.
  With interactive call, send it to the message buffer too. 

ar-py-looking-at-keywords-p
---------------------------
If looking at a python keyword. Returns t or nil. 

ar-py-match-paren-mode
----------------------
py-match-paren-mode nil oder t

ar-py-match-paren
-----------------
Goto to the opening or closing of block before or after point.

With arg, do it that many times.
 Closes unclosed block if jumping from beginning. 

ar-py-documentation
-------------------
Launch PyDOC on the Word at Point

eva
---
Put "eval(...)" forms around strings at point. 

pst-here
--------
Kill previous "pdb.set_trace()" and insert it at point. 

py-printform-insert
-------------------
Inserts a print statement out of current `(car kill-ring)' by default, inserts STRING if delivered.

With optional C-u print as string

py-line-to-printform-python2
----------------------------
Transforms the item on current in a print statement. 

py-boolswitch
-------------
Edit the assignment of a boolean variable, revert them.

I.e. switch it from "True" to "False" and vice versa

py-switch-imenu-index-function
------------------------------
Switch between series 5. index machine `py--imenu-create-index' and `py--imenu-create-index-new', which also lists modules variables 

ipython
-------
Start an IPython interpreter.

Optional C-u prompts for path to the interpreter. 

ipython2\.7
-----------
Start an IPython2.7 interpreter.

Optional C-u prompts for path to the interpreter. 

ipython3
--------
Start an IPython3 interpreter.

Optional C-u prompts for path to the interpreter. 

jython
------
Start an Jython interpreter.

Optional C-u prompts for path to the interpreter. 

python
------
Start an Python interpreter.

Optional C-u prompts for path to the interpreter. 

python2
-------
Start an Python2 interpreter.

Optional C-u prompts for path to the interpreter. 

python3
-------
Start an Python3 interpreter.

Optional C-u prompts for path to the interpreter. 

ipython-dedicated
-----------------
Start an unique IPython interpreter in another window.

Optional C-u prompts for path to the interpreter. 

ipython2\.7-dedicated
---------------------
Start an unique IPython2.7 interpreter in another window.

Optional C-u prompts for path to the interpreter. 

ipython3-dedicated
------------------
Start an unique IPython3 interpreter in another window.

Optional C-u prompts for path to the interpreter. 

jython-dedicated
----------------
Start an unique Jython interpreter in another window.

Optional C-u prompts for path to the interpreter. 

python-dedicated
----------------
Start an unique Python interpreter in another window.

Optional C-u prompts for path to the interpreter. 

python2-dedicated
-----------------
Start an unique Python2 interpreter in another window.

Optional C-u prompts for path to the interpreter. 

python3-dedicated
-----------------
Start an unique Python3 interpreter in another window.

Optional C-u prompts for path to the interpreter. 

ipython-switch
--------------
Switch to IPython interpreter in another window.

Optional C-u prompts for path to the interpreter. 

ipython2\.7-switch
------------------
Switch to IPython2.7 interpreter in another window.

Optional C-u prompts for path to the interpreter. 

ipython3-switch
---------------
Switch to IPython3 interpreter in another window.

Optional C-u prompts for path to the interpreter. 

jython-switch
-------------
Switch to Jython interpreter in another window.

Optional C-u prompts for path to the interpreter. 

python-switch
-------------
Switch to Python interpreter in another window.

Optional C-u prompts for path to the interpreter. 

python2-switch
--------------
Switch to Python2 interpreter in another window.

Optional C-u prompts for path to the interpreter. 

python3-switch
--------------
Switch to Python3 interpreter in another window.

Optional C-u prompts for path to the interpreter. 

ipython-no-switch
-----------------
Open an IPython interpreter in another window, but do not switch to it.

Optional C-u prompts for path to the interpreter. 

ipython2\.7-no-switch
---------------------
Open an IPython2.7 interpreter in another window, but do not switch to it.

Optional C-u prompts for path to the interpreter. 

ipython3-no-switch
------------------
Open an IPython3 interpreter in another window, but do not switch to it.

Optional C-u prompts for path to the interpreter. 

jython-no-switch
----------------
Open an Jython interpreter in another window, but do not switch to it.

Optional C-u prompts for path to the interpreter. 

python-no-switch
----------------
Open an Python interpreter in another window, but do not switch to it.

Optional C-u prompts for path to the interpreter. 

python2-no-switch
-----------------
Open an Python2 interpreter in another window, but do not switch to it.

Optional C-u prompts for path to the interpreter. 

python3-no-switch
-----------------
Open an Python3 interpreter in another window, but do not switch to it.

Optional C-u prompts for path to the interpreter. 

ipython-switch-dedicated
------------------------
Switch to an unique IPython interpreter in another window.

Optional C-u prompts for path to the interpreter. 

ipython2\.7-switch-dedicated
----------------------------
Switch to an unique IPython2.7 interpreter in another window.

Optional C-u prompts for path to the interpreter. 

ipython3-switch-dedicated
-------------------------
Switch to an unique IPython3 interpreter in another window.

Optional C-u prompts for path to the interpreter. 

jython-switch-dedicated
-----------------------
Switch to an unique Jython interpreter in another window.

Optional C-u prompts for path to the interpreter. 

python-switch-dedicated
-----------------------
Switch to an unique Python interpreter in another window.

Optional C-u prompts for path to the interpreter. 

python2-switch-dedicated
------------------------
Switch to an unique Python2 interpreter in another window.

Optional C-u prompts for path to the interpreter. 

python3-switch-dedicated
------------------------
Switch to an unique Python3 interpreter in another window.

Optional C-u prompts for path to the interpreter. 

py-electric-colon
-----------------
Insert a colon and indent accordingly.

If a numeric argument ARG is provided, that many colons are inserted
non-electrically.

Electric behavior is inhibited inside a string or
comment or by universal prefix C-u.

Switched by `py-electric-colon-active-p', default is nil
See also `py-electric-colon-greedy-p' 

py-electric-space
-----------------
Close completion buffer when it's sure, it's no longer needed, i.e. when inserting a space.

Works around a bug in `choose-completion'. 

py-electric-comment
-------------------
Insert a comment. If starting a comment, indent accordingly.

If a numeric argument ARG is provided, that many "#" are inserted
non-electrically.
With C-u "#" electric behavior is inhibited inside a string or comment.

py-empty-out-list-backward
--------------------------
Deletes all elements from list before point. 

py-electric-backspace
---------------------
Delete preceding character or level of indentation.

When `delete-active-region' and (region-active-p), delete region.

Unless at indentation:
  With `py-electric-kill-backward-p' delete whitespace before point.
  With `py-electric-kill-backward-p' at end of a list, empty that list.

Returns column reached. 

py-electric-delete
------------------
Delete following character or levels of whitespace.

When `delete-active-region' and (region-active-p), delete region 

py-electric-yank
----------------
Perform command `yank' followed by an `indent-according-to-mode' 

virtualenv-current
------------------
Barfs the current activated virtualenv

virtualenv-deactivate
---------------------
Deactivate the current virtual enviroment

virtualenv-activate
-------------------
Activate the virtualenv located in DIR

virtualenv-workon
-----------------
Issue a virtualenvwrapper-like virtualenv-workon command

py--beginning-of-comment-position
---------------------------------
Returns beginning of comment position. 

py--beginning-of-paragraph-position
-----------------------------------
Returns beginning of paragraph position. 

py-up-statement
---------------
Go to the beginning of next statement upwards in buffer.

Return position if statement found, nil otherwise. 

py-down-statement
-----------------
Go to the beginning of next statement downwards in buffer.

Return position if statement found, nil otherwise. 

py-up-block
-----------
Go to the beginning of next block upwards in buffer.

Return position if block found, nil otherwise. 

py-up-block-or-clause
---------------------
Go to the beginning of next block-or-clause upwards in buffer.

Return position if block-or-clause found, nil otherwise. 

py-up-class
-----------
Go to the beginning of next class upwards in buffer.

Return position if class found, nil otherwise. 

py-up-clause
------------
Go to the beginning of next clause upwards in buffer.

Return position if clause found, nil otherwise. 

py-up-def
---------
Go to the beginning of next def upwards in buffer.

Return position if def found, nil otherwise. 

py-up-def-or-class
------------------
Go to the beginning of next def-or-class upwards in buffer.

Return position if def-or-class found, nil otherwise. 

py-up-minor-block
-----------------
Go to the beginning of next minor-block upwards in buffer.

Return position if minor-block found, nil otherwise. 

py-up-section
-------------
Go to the beginning of next section upwards in buffer.

Return position if section found, nil otherwise. 

py-up-top-level
---------------
Go to the beginning of next top-level upwards in buffer.

Return position if top-level found, nil otherwise. 

py-down-block
-------------
Go to the beginning of next block below in buffer.

Return position if block found, nil otherwise. 

py-down-block-or-clause
-----------------------
Go to the beginning of next block-or-clause below in buffer.

Return position if block-or-clause found, nil otherwise. 

py-down-class
-------------
Go to the beginning of next class below in buffer.

Return position if class found, nil otherwise. 

py-down-clause
--------------
Go to the beginning of next clause below in buffer.

Return position if clause found, nil otherwise. 

py-down-def
-----------
Go to the beginning of next def below in buffer.

Return position if def found, nil otherwise. 

py-down-def-or-class
--------------------
Go to the beginning of next def-or-class below in buffer.

Return position if def-or-class found, nil otherwise. 

py-down-minor-block
-------------------
Go to the beginning of next minor-block below in buffer.

Return position if minor-block found, nil otherwise. 

py-down-section
---------------
Go to the beginning of next section below in buffer.

Return position if section found, nil otherwise. 

py-down-top-level
-----------------
Go to the beginning of next top-level below in buffer.

Return position if top-level found, nil otherwise. 

py-up-block-bol
---------------
Go to the beginning of next block upwards in buffer.

Go to beginning of line.
Return position if block found, nil otherwise. 

py-up-block-or-clause-bol
-------------------------
Go to the beginning of next block-or-clause upwards in buffer.

Go to beginning of line.
Return position if block-or-clause found, nil otherwise. 

py-up-class-bol
---------------
Go to the beginning of next class upwards in buffer.

Go to beginning of line.
Return position if class found, nil otherwise. 

py-up-clause-bol
----------------
Go to the beginning of next clause upwards in buffer.

Go to beginning of line.
Return position if clause found, nil otherwise. 

py-up-def-bol
-------------
Go to the beginning of next def upwards in buffer.

Go to beginning of line.
Return position if def found, nil otherwise. 

py-up-def-or-class-bol
----------------------
Go to the beginning of next def-or-class upwards in buffer.

Go to beginning of line.
Return position if def-or-class found, nil otherwise. 

py-up-minor-block-bol
---------------------
Go to the beginning of next minor-block upwards in buffer.

Go to beginning of line.
Return position if minor-block found, nil otherwise. 

py-up-section-bol
-----------------
Go to the beginning of next section upwards in buffer.

Go to beginning of line.
Return position if section found, nil otherwise. 

py-up-top-level-bol
-------------------
Go to the beginning of next top-level upwards in buffer.

Go to beginning of line.
Return position if top-level found, nil otherwise. 

py-down-block-bol
-----------------
Go to the beginning of next block below in buffer.

Go to beginning of line
Return position if block found, nil otherwise 

py-down-block-or-clause-bol
---------------------------
Go to the beginning of next block-or-clause below in buffer.

Go to beginning of line
Return position if block-or-clause found, nil otherwise 

py-down-class-bol
-----------------
Go to the beginning of next class below in buffer.

Go to beginning of line
Return position if class found, nil otherwise 

py-down-clause-bol
------------------
Go to the beginning of next clause below in buffer.

Go to beginning of line
Return position if clause found, nil otherwise 

py-down-def-bol
---------------
Go to the beginning of next def below in buffer.

Go to beginning of line
Return position if def found, nil otherwise 

py-down-def-or-class-bol
------------------------
Go to the beginning of next def-or-class below in buffer.

Go to beginning of line
Return position if def-or-class found, nil otherwise 

py-down-minor-block-bol
-----------------------
Go to the beginning of next minor-block below in buffer.

Go to beginning of line
Return position if minor-block found, nil otherwise 

py-down-section-bol
-------------------
Go to the beginning of next section below in buffer.

Go to beginning of line
Return position if section found, nil otherwise 

py-down-top-level-bol
---------------------
Go to the beginning of next top-level below in buffer.

Go to beginning of line
Return position if top-level found, nil otherwise 

py-execute-statement
--------------------
Send statement at point to Python default interpreter. 

py-execute-block
----------------
Send block at point to Python default interpreter. 

py-execute-block-or-clause
--------------------------
Send block-or-clause at point to Python default interpreter. 

py-execute-def
--------------
Send def at point to Python default interpreter. 

py-execute-class
----------------
Send class at point to Python default interpreter. 

py-execute-def-or-class
-----------------------
Send def-or-class at point to Python default interpreter. 

py-execute-expression
---------------------
Send expression at point to Python default interpreter. 

py-execute-partial-expression
-----------------------------
Send partial-expression at point to Python default interpreter. 

py-execute-top-level
--------------------
Send top-level at point to Python default interpreter. 

py-execute-clause
-----------------
Send clause at point to Python default interpreter. 

py-execute-statement-dedicated
------------------------------
Send statement to unique interpreter. 

py-execute-statement-python
---------------------------
Send statement at point to default interpreter. 

For `default' see value of `py-shell-name'

py-execute-statement-python-switch
----------------------------------
Send statement at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'

py-execute-statement-python-no-switch
-------------------------------------
Send statement at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'

py-execute-statement-python-dedicated
-------------------------------------
Send statement at point to default unique interpreter. 

For `default' see value of `py-shell-name'

py-execute-statement-python-dedicated-switch
--------------------------------------------
Send statement at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'

py-execute-statement-python2
----------------------------
Send statement at point to Python2 interpreter. 

py-execute-statement-python2-switch
-----------------------------------
Send statement at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-statement-python2-no-switch
--------------------------------------
Send statement at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-statement-python2-dedicated
--------------------------------------
Send statement at point to Python2 unique interpreter. 

py-execute-statement-python2-dedicated-switch
---------------------------------------------
Send statement at point to Python2 unique interpreter and switch to result. 

py-execute-statement-python3
----------------------------
Send statement at point to Python3 interpreter. 

py-execute-statement-python3-switch
-----------------------------------
Send statement at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-statement-python3-no-switch
--------------------------------------
Send statement at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-statement-python3-dedicated
--------------------------------------
Send statement at point to Python3 unique interpreter. 

py-execute-statement-python3-dedicated-switch
---------------------------------------------
Send statement at point to Python3 unique interpreter and switch to result. 

py-execute-statement-ipython
----------------------------
Send statement at point to IPython interpreter. 

py-execute-statement-ipython-switch
-----------------------------------
Send statement at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-statement-ipython-no-switch
--------------------------------------
Send statement at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-statement-ipython-dedicated
--------------------------------------
Send statement at point to IPython unique interpreter. 

py-execute-statement-ipython-dedicated-switch
---------------------------------------------
Send statement at point to IPython unique interpreter and switch to result. 

py-execute-statement-ipython2\.7
--------------------------------
Send statement at point to IPython interpreter. 

py-execute-statement-ipython2\.7-switch
---------------------------------------
Send statement at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-statement-ipython2\.7-no-switch
------------------------------------------
Send statement at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-statement-ipython2\.7-dedicated
------------------------------------------
Send statement at point to IPython unique interpreter. 

py-execute-statement-ipython2\.7-dedicated-switch
-------------------------------------------------
Send statement at point to IPython unique interpreter and switch to result. 

py-execute-statement-ipython3
-----------------------------
Send statement at point to IPython interpreter. 

py-execute-statement-ipython3-switch
------------------------------------
Send statement at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-statement-ipython3-no-switch
---------------------------------------
Send statement at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-statement-ipython3-dedicated
---------------------------------------
Send statement at point to IPython unique interpreter. 

py-execute-statement-ipython3-dedicated-switch
----------------------------------------------
Send statement at point to IPython unique interpreter and switch to result. 

py-execute-statement-jython
---------------------------
Send statement at point to Jython interpreter. 

py-execute-statement-jython-switch
----------------------------------
Send statement at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-statement-jython-no-switch
-------------------------------------
Send statement at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-statement-jython-dedicated
-------------------------------------
Send statement at point to Jython unique interpreter. 

py-execute-statement-jython-dedicated-switch
--------------------------------------------
Send statement at point to Jython unique interpreter and switch to result. 

py-execute-block-dedicated
--------------------------
Send block to unique interpreter. 

py-execute-block-python
-----------------------
Send block at point to default interpreter. 

For `default' see value of `py-shell-name'

py-execute-block-python-switch
------------------------------
Send block at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'

py-execute-block-python-no-switch
---------------------------------
Send block at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'

py-execute-block-python-dedicated
---------------------------------
Send block at point to default unique interpreter. 

For `default' see value of `py-shell-name'

py-execute-block-python-dedicated-switch
----------------------------------------
Send block at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'

py-execute-block-python2
------------------------
Send block at point to Python2 interpreter. 

py-execute-block-python2-switch
-------------------------------
Send block at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-python2-no-switch
----------------------------------
Send block at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-python2-dedicated
----------------------------------
Send block at point to Python2 unique interpreter. 

py-execute-block-python2-dedicated-switch
-----------------------------------------
Send block at point to Python2 unique interpreter and switch to result. 

py-execute-block-python3
------------------------
Send block at point to Python3 interpreter. 

py-execute-block-python3-switch
-------------------------------
Send block at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-python3-no-switch
----------------------------------
Send block at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-python3-dedicated
----------------------------------
Send block at point to Python3 unique interpreter. 

py-execute-block-python3-dedicated-switch
-----------------------------------------
Send block at point to Python3 unique interpreter and switch to result. 

py-execute-block-ipython
------------------------
Send block at point to IPython interpreter. 

py-execute-block-ipython-switch
-------------------------------
Send block at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-ipython-no-switch
----------------------------------
Send block at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-ipython-dedicated
----------------------------------
Send block at point to IPython unique interpreter. 

py-execute-block-ipython-dedicated-switch
-----------------------------------------
Send block at point to IPython unique interpreter and switch to result. 

py-execute-block-ipython2\.7
----------------------------
Send block at point to IPython interpreter. 

py-execute-block-ipython2\.7-switch
-----------------------------------
Send block at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-ipython2\.7-no-switch
--------------------------------------
Send block at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-ipython2\.7-dedicated
--------------------------------------
Send block at point to IPython unique interpreter. 

py-execute-block-ipython2\.7-dedicated-switch
---------------------------------------------
Send block at point to IPython unique interpreter and switch to result. 

py-execute-block-ipython3
-------------------------
Send block at point to IPython interpreter. 

py-execute-block-ipython3-switch
--------------------------------
Send block at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-ipython3-no-switch
-----------------------------------
Send block at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-ipython3-dedicated
-----------------------------------
Send block at point to IPython unique interpreter. 

py-execute-block-ipython3-dedicated-switch
------------------------------------------
Send block at point to IPython unique interpreter and switch to result. 

py-execute-block-jython
-----------------------
Send block at point to Jython interpreter. 

py-execute-block-jython-switch
------------------------------
Send block at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-jython-no-switch
---------------------------------
Send block at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-jython-dedicated
---------------------------------
Send block at point to Jython unique interpreter. 

py-execute-block-jython-dedicated-switch
----------------------------------------
Send block at point to Jython unique interpreter and switch to result. 

py-execute-clause-dedicated
---------------------------
Send clause to unique interpreter. 

py-execute-clause-python
------------------------
Send clause at point to default interpreter. 

For `default' see value of `py-shell-name'

py-execute-clause-python-switch
-------------------------------
Send clause at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'

py-execute-clause-python-no-switch
----------------------------------
Send clause at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'

py-execute-clause-python-dedicated
----------------------------------
Send clause at point to default unique interpreter. 

For `default' see value of `py-shell-name'

py-execute-clause-python-dedicated-switch
-----------------------------------------
Send clause at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'

py-execute-clause-python2
-------------------------
Send clause at point to Python2 interpreter. 

py-execute-clause-python2-switch
--------------------------------
Send clause at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-clause-python2-no-switch
-----------------------------------
Send clause at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-clause-python2-dedicated
-----------------------------------
Send clause at point to Python2 unique interpreter. 

py-execute-clause-python2-dedicated-switch
------------------------------------------
Send clause at point to Python2 unique interpreter and switch to result. 

py-execute-clause-python3
-------------------------
Send clause at point to Python3 interpreter. 

py-execute-clause-python3-switch
--------------------------------
Send clause at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-clause-python3-no-switch
-----------------------------------
Send clause at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-clause-python3-dedicated
-----------------------------------
Send clause at point to Python3 unique interpreter. 

py-execute-clause-python3-dedicated-switch
------------------------------------------
Send clause at point to Python3 unique interpreter and switch to result. 

py-execute-clause-ipython
-------------------------
Send clause at point to IPython interpreter. 

py-execute-clause-ipython-switch
--------------------------------
Send clause at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-clause-ipython-no-switch
-----------------------------------
Send clause at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-clause-ipython-dedicated
-----------------------------------
Send clause at point to IPython unique interpreter. 

py-execute-clause-ipython-dedicated-switch
------------------------------------------
Send clause at point to IPython unique interpreter and switch to result. 

py-execute-clause-ipython2\.7
-----------------------------
Send clause at point to IPython interpreter. 

py-execute-clause-ipython2\.7-switch
------------------------------------
Send clause at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-clause-ipython2\.7-no-switch
---------------------------------------
Send clause at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-clause-ipython2\.7-dedicated
---------------------------------------
Send clause at point to IPython unique interpreter. 

py-execute-clause-ipython2\.7-dedicated-switch
----------------------------------------------
Send clause at point to IPython unique interpreter and switch to result. 

py-execute-clause-ipython3
--------------------------
Send clause at point to IPython interpreter. 

py-execute-clause-ipython3-switch
---------------------------------
Send clause at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-clause-ipython3-no-switch
------------------------------------
Send clause at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-clause-ipython3-dedicated
------------------------------------
Send clause at point to IPython unique interpreter. 

py-execute-clause-ipython3-dedicated-switch
-------------------------------------------
Send clause at point to IPython unique interpreter and switch to result. 

py-execute-clause-jython
------------------------
Send clause at point to Jython interpreter. 

py-execute-clause-jython-switch
-------------------------------
Send clause at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-clause-jython-no-switch
----------------------------------
Send clause at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-clause-jython-dedicated
----------------------------------
Send clause at point to Jython unique interpreter. 

py-execute-clause-jython-dedicated-switch
-----------------------------------------
Send clause at point to Jython unique interpreter and switch to result. 

py-execute-block-or-clause-dedicated
------------------------------------
Send block-or-clause to unique interpreter. 

py-execute-block-or-clause-python
---------------------------------
Send block-or-clause at point to default interpreter. 

For `default' see value of `py-shell-name'

py-execute-block-or-clause-python-switch
----------------------------------------
Send block-or-clause at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'

py-execute-block-or-clause-python-no-switch
-------------------------------------------
Send block-or-clause at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'

py-execute-block-or-clause-python-dedicated
-------------------------------------------
Send block-or-clause at point to default unique interpreter. 

For `default' see value of `py-shell-name'

py-execute-block-or-clause-python-dedicated-switch
--------------------------------------------------
Send block-or-clause at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'

py-execute-block-or-clause-python2
----------------------------------
Send block-or-clause at point to Python2 interpreter. 

py-execute-block-or-clause-python2-switch
-----------------------------------------
Send block-or-clause at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-or-clause-python2-no-switch
--------------------------------------------
Send block-or-clause at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-or-clause-python2-dedicated
--------------------------------------------
Send block-or-clause at point to Python2 unique interpreter. 

py-execute-block-or-clause-python2-dedicated-switch
---------------------------------------------------
Send block-or-clause at point to Python2 unique interpreter and switch to result. 

py-execute-block-or-clause-python3
----------------------------------
Send block-or-clause at point to Python3 interpreter. 

py-execute-block-or-clause-python3-switch
-----------------------------------------
Send block-or-clause at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-or-clause-python3-no-switch
--------------------------------------------
Send block-or-clause at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-or-clause-python3-dedicated
--------------------------------------------
Send block-or-clause at point to Python3 unique interpreter. 

py-execute-block-or-clause-python3-dedicated-switch
---------------------------------------------------
Send block-or-clause at point to Python3 unique interpreter and switch to result. 

py-execute-block-or-clause-ipython
----------------------------------
Send block-or-clause at point to IPython interpreter. 

py-execute-block-or-clause-ipython-switch
-----------------------------------------
Send block-or-clause at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-or-clause-ipython-no-switch
--------------------------------------------
Send block-or-clause at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-or-clause-ipython-dedicated
--------------------------------------------
Send block-or-clause at point to IPython unique interpreter. 

py-execute-block-or-clause-ipython-dedicated-switch
---------------------------------------------------
Send block-or-clause at point to IPython unique interpreter and switch to result. 

py-execute-block-or-clause-ipython2\.7
--------------------------------------
Send block-or-clause at point to IPython interpreter. 

py-execute-block-or-clause-ipython2\.7-switch
---------------------------------------------
Send block-or-clause at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-or-clause-ipython2\.7-no-switch
------------------------------------------------
Send block-or-clause at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-or-clause-ipython2\.7-dedicated
------------------------------------------------
Send block-or-clause at point to IPython unique interpreter. 

py-execute-block-or-clause-ipython2\.7-dedicated-switch
-------------------------------------------------------
Send block-or-clause at point to IPython unique interpreter and switch to result. 

py-execute-block-or-clause-ipython3
-----------------------------------
Send block-or-clause at point to IPython interpreter. 

py-execute-block-or-clause-ipython3-switch
------------------------------------------
Send block-or-clause at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-or-clause-ipython3-no-switch
---------------------------------------------
Send block-or-clause at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-or-clause-ipython3-dedicated
---------------------------------------------
Send block-or-clause at point to IPython unique interpreter. 

py-execute-block-or-clause-ipython3-dedicated-switch
----------------------------------------------------
Send block-or-clause at point to IPython unique interpreter and switch to result. 

py-execute-block-or-clause-jython
---------------------------------
Send block-or-clause at point to Jython interpreter. 

py-execute-block-or-clause-jython-switch
----------------------------------------
Send block-or-clause at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-or-clause-jython-no-switch
-------------------------------------------
Send block-or-clause at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-or-clause-jython-dedicated
-------------------------------------------
Send block-or-clause at point to Jython unique interpreter. 

py-execute-block-or-clause-jython-dedicated-switch
--------------------------------------------------
Send block-or-clause at point to Jython unique interpreter and switch to result. 

py-execute-def-dedicated
------------------------
Send def to unique interpreter. 

py-execute-def-python
---------------------
Send def at point to default interpreter. 

For `default' see value of `py-shell-name'

py-execute-def-python-switch
----------------------------
Send def at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'

py-execute-def-python-no-switch
-------------------------------
Send def at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'

py-execute-def-python-dedicated
-------------------------------
Send def at point to default unique interpreter. 

For `default' see value of `py-shell-name'

py-execute-def-python-dedicated-switch
--------------------------------------
Send def at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'

py-execute-def-python2
----------------------
Send def at point to Python2 interpreter. 

py-execute-def-python2-switch
-----------------------------
Send def at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-def-python2-no-switch
--------------------------------
Send def at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-def-python2-dedicated
--------------------------------
Send def at point to Python2 unique interpreter. 

py-execute-def-python2-dedicated-switch
---------------------------------------
Send def at point to Python2 unique interpreter and switch to result. 

py-execute-def-python3
----------------------
Send def at point to Python3 interpreter. 

py-execute-def-python3-switch
-----------------------------
Send def at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-def-python3-no-switch
--------------------------------
Send def at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-def-python3-dedicated
--------------------------------
Send def at point to Python3 unique interpreter. 

py-execute-def-python3-dedicated-switch
---------------------------------------
Send def at point to Python3 unique interpreter and switch to result. 

py-execute-def-ipython
----------------------
Send def at point to IPython interpreter. 

py-execute-def-ipython-switch
-----------------------------
Send def at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-def-ipython-no-switch
--------------------------------
Send def at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-def-ipython-dedicated
--------------------------------
Send def at point to IPython unique interpreter. 

py-execute-def-ipython-dedicated-switch
---------------------------------------
Send def at point to IPython unique interpreter and switch to result. 

py-execute-def-ipython2\.7
--------------------------
Send def at point to IPython interpreter. 

py-execute-def-ipython2\.7-switch
---------------------------------
Send def at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-def-ipython2\.7-no-switch
------------------------------------
Send def at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-def-ipython2\.7-dedicated
------------------------------------
Send def at point to IPython unique interpreter. 

py-execute-def-ipython2\.7-dedicated-switch
-------------------------------------------
Send def at point to IPython unique interpreter and switch to result. 

py-execute-def-ipython3
-----------------------
Send def at point to IPython interpreter. 

py-execute-def-ipython3-switch
------------------------------
Send def at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-def-ipython3-no-switch
---------------------------------
Send def at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-def-ipython3-dedicated
---------------------------------
Send def at point to IPython unique interpreter. 

py-execute-def-ipython3-dedicated-switch
----------------------------------------
Send def at point to IPython unique interpreter and switch to result. 

py-execute-def-jython
---------------------
Send def at point to Jython interpreter. 

py-execute-def-jython-switch
----------------------------
Send def at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-def-jython-no-switch
-------------------------------
Send def at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-def-jython-dedicated
-------------------------------
Send def at point to Jython unique interpreter. 

py-execute-def-jython-dedicated-switch
--------------------------------------
Send def at point to Jython unique interpreter and switch to result. 

py-execute-class-dedicated
--------------------------
Send class to unique interpreter. 

py-execute-class-python
-----------------------
Send class at point to default interpreter. 

For `default' see value of `py-shell-name'

py-execute-class-python-switch
------------------------------
Send class at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'

py-execute-class-python-no-switch
---------------------------------
Send class at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'

py-execute-class-python-dedicated
---------------------------------
Send class at point to default unique interpreter. 

For `default' see value of `py-shell-name'

py-execute-class-python-dedicated-switch
----------------------------------------
Send class at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'

py-execute-class-python2
------------------------
Send class at point to Python2 interpreter. 

py-execute-class-python2-switch
-------------------------------
Send class at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-class-python2-no-switch
----------------------------------
Send class at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-class-python2-dedicated
----------------------------------
Send class at point to Python2 unique interpreter. 

py-execute-class-python2-dedicated-switch
-----------------------------------------
Send class at point to Python2 unique interpreter and switch to result. 

py-execute-class-python3
------------------------
Send class at point to Python3 interpreter. 

py-execute-class-python3-switch
-------------------------------
Send class at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-class-python3-no-switch
----------------------------------
Send class at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-class-python3-dedicated
----------------------------------
Send class at point to Python3 unique interpreter. 

py-execute-class-python3-dedicated-switch
-----------------------------------------
Send class at point to Python3 unique interpreter and switch to result. 

py-execute-class-ipython
------------------------
Send class at point to IPython interpreter. 

py-execute-class-ipython-switch
-------------------------------
Send class at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-class-ipython-no-switch
----------------------------------
Send class at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-class-ipython-dedicated
----------------------------------
Send class at point to IPython unique interpreter. 

py-execute-class-ipython-dedicated-switch
-----------------------------------------
Send class at point to IPython unique interpreter and switch to result. 

py-execute-class-ipython2\.7
----------------------------
Send class at point to IPython interpreter. 

py-execute-class-ipython2\.7-switch
-----------------------------------
Send class at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-class-ipython2\.7-no-switch
--------------------------------------
Send class at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-class-ipython2\.7-dedicated
--------------------------------------
Send class at point to IPython unique interpreter. 

py-execute-class-ipython2\.7-dedicated-switch
---------------------------------------------
Send class at point to IPython unique interpreter and switch to result. 

py-execute-class-ipython3
-------------------------
Send class at point to IPython interpreter. 

py-execute-class-ipython3-switch
--------------------------------
Send class at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-class-ipython3-no-switch
-----------------------------------
Send class at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-class-ipython3-dedicated
-----------------------------------
Send class at point to IPython unique interpreter. 

py-execute-class-ipython3-dedicated-switch
------------------------------------------
Send class at point to IPython unique interpreter and switch to result. 

py-execute-class-jython
-----------------------
Send class at point to Jython interpreter. 

py-execute-class-jython-switch
------------------------------
Send class at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-class-jython-no-switch
---------------------------------
Send class at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-class-jython-dedicated
---------------------------------
Send class at point to Jython unique interpreter. 

py-execute-class-jython-dedicated-switch
----------------------------------------
Send class at point to Jython unique interpreter and switch to result. 

py-execute-def-or-class-dedicated
---------------------------------
Send def-or-class to unique interpreter. 

py-execute-def-or-class-python
------------------------------
Send def-or-class at point to default interpreter. 

For `default' see value of `py-shell-name'

py-execute-def-or-class-python-switch
-------------------------------------
Send def-or-class at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'

py-execute-def-or-class-python-no-switch
----------------------------------------
Send def-or-class at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'

py-execute-def-or-class-python-dedicated
----------------------------------------
Send def-or-class at point to default unique interpreter. 

For `default' see value of `py-shell-name'

py-execute-def-or-class-python-dedicated-switch
-----------------------------------------------
Send def-or-class at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'

py-execute-def-or-class-python2
-------------------------------
Send def-or-class at point to Python2 interpreter. 

py-execute-def-or-class-python2-switch
--------------------------------------
Send def-or-class at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-def-or-class-python2-no-switch
-----------------------------------------
Send def-or-class at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-def-or-class-python2-dedicated
-----------------------------------------
Send def-or-class at point to Python2 unique interpreter. 

py-execute-def-or-class-python2-dedicated-switch
------------------------------------------------
Send def-or-class at point to Python2 unique interpreter and switch to result. 

py-execute-def-or-class-python3
-------------------------------
Send def-or-class at point to Python3 interpreter. 

py-execute-def-or-class-python3-switch
--------------------------------------
Send def-or-class at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-def-or-class-python3-no-switch
-----------------------------------------
Send def-or-class at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-def-or-class-python3-dedicated
-----------------------------------------
Send def-or-class at point to Python3 unique interpreter. 

py-execute-def-or-class-python3-dedicated-switch
------------------------------------------------
Send def-or-class at point to Python3 unique interpreter and switch to result. 

py-execute-def-or-class-ipython
-------------------------------
Send def-or-class at point to IPython interpreter. 

py-execute-def-or-class-ipython-switch
--------------------------------------
Send def-or-class at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-def-or-class-ipython-no-switch
-----------------------------------------
Send def-or-class at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-def-or-class-ipython-dedicated
-----------------------------------------
Send def-or-class at point to IPython unique interpreter. 

py-execute-def-or-class-ipython-dedicated-switch
------------------------------------------------
Send def-or-class at point to IPython unique interpreter and switch to result. 

py-execute-def-or-class-ipython2\.7
-----------------------------------
Send def-or-class at point to IPython interpreter. 

py-execute-def-or-class-ipython2\.7-switch
------------------------------------------
Send def-or-class at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-def-or-class-ipython2\.7-no-switch
---------------------------------------------
Send def-or-class at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-def-or-class-ipython2\.7-dedicated
---------------------------------------------
Send def-or-class at point to IPython unique interpreter. 

py-execute-def-or-class-ipython2\.7-dedicated-switch
----------------------------------------------------
Send def-or-class at point to IPython unique interpreter and switch to result. 

py-execute-def-or-class-ipython3
--------------------------------
Send def-or-class at point to IPython interpreter. 

py-execute-def-or-class-ipython3-switch
---------------------------------------
Send def-or-class at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-def-or-class-ipython3-no-switch
------------------------------------------
Send def-or-class at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-def-or-class-ipython3-dedicated
------------------------------------------
Send def-or-class at point to IPython unique interpreter. 

py-execute-def-or-class-ipython3-dedicated-switch
-------------------------------------------------
Send def-or-class at point to IPython unique interpreter and switch to result. 

py-execute-def-or-class-jython
------------------------------
Send def-or-class at point to Jython interpreter. 

py-execute-def-or-class-jython-switch
-------------------------------------
Send def-or-class at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-def-or-class-jython-no-switch
----------------------------------------
Send def-or-class at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-def-or-class-jython-dedicated
----------------------------------------
Send def-or-class at point to Jython unique interpreter. 

py-execute-def-or-class-jython-dedicated-switch
-----------------------------------------------
Send def-or-class at point to Jython unique interpreter and switch to result. 

py-execute-buffer-python
------------------------
Send buffer at point to default interpreter. 

For `default' see value of `py-shell-name'

py-execute-buffer-python-switch
-------------------------------
Send buffer at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'

py-execute-buffer-python-no-switch
----------------------------------
Send buffer at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'

py-execute-buffer-python-dedicated
----------------------------------
Send buffer at point to default unique interpreter. 

For `default' see value of `py-shell-name'

py-execute-buffer-python-dedicated-switch
-----------------------------------------
Send buffer at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'

py-execute-buffer-python2
-------------------------
Send buffer at point to Python2 interpreter. 

py-execute-buffer-python2-switch
--------------------------------
Send buffer at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-buffer-python2-no-switch
-----------------------------------
Send buffer at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-buffer-python2-dedicated
-----------------------------------
Send buffer at point to Python2 unique interpreter. 

py-execute-buffer-python2-dedicated-switch
------------------------------------------
Send buffer at point to Python2 unique interpreter and switch to result. 

py-execute-buffer-python3
-------------------------
Send buffer at point to Python3 interpreter. 

py-execute-buffer-python3-switch
--------------------------------
Send buffer at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-buffer-python3-no-switch
-----------------------------------
Send buffer at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-buffer-python3-dedicated
-----------------------------------
Send buffer at point to Python3 unique interpreter. 

py-execute-buffer-python3-dedicated-switch
------------------------------------------
Send buffer at point to Python3 unique interpreter and switch to result. 

py-execute-buffer-ipython
-------------------------
Send buffer at point to IPython interpreter. 

py-execute-buffer-ipython-switch
--------------------------------
Send buffer at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-buffer-ipython-no-switch
-----------------------------------
Send buffer at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-buffer-ipython-dedicated
-----------------------------------
Send buffer at point to IPython unique interpreter. 

py-execute-buffer-ipython-dedicated-switch
------------------------------------------
Send buffer at point to IPython unique interpreter and switch to result. 

py-execute-buffer-ipython2\.7
-----------------------------
Send buffer at point to IPython interpreter. 

py-execute-buffer-ipython2\.7-switch
------------------------------------
Send buffer at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-buffer-ipython2\.7-no-switch
---------------------------------------
Send buffer at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-buffer-ipython2\.7-dedicated
---------------------------------------
Send buffer at point to IPython unique interpreter. 

py-execute-buffer-ipython2\.7-dedicated-switch
----------------------------------------------
Send buffer at point to IPython unique interpreter and switch to result. 

py-execute-buffer-ipython3
--------------------------
Send buffer at point to IPython interpreter. 

py-execute-buffer-ipython3-switch
---------------------------------
Send buffer at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-buffer-ipython3-no-switch
------------------------------------
Send buffer at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-buffer-ipython3-dedicated
------------------------------------
Send buffer at point to IPython unique interpreter. 

py-execute-buffer-ipython3-dedicated-switch
-------------------------------------------
Send buffer at point to IPython unique interpreter and switch to result. 

py-execute-buffer-jython
------------------------
Send buffer at point to Jython interpreter. 

py-execute-buffer-jython-switch
-------------------------------
Send buffer at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-buffer-jython-no-switch
----------------------------------
Send buffer at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-buffer-jython-dedicated
----------------------------------
Send buffer at point to Jython unique interpreter. 

py-execute-buffer-jython-dedicated-switch
-----------------------------------------
Send buffer at point to Jython unique interpreter and switch to result. 

py-execute-expression-dedicated
-------------------------------
Send expression to unique interpreter. 

py-execute-expression-python
----------------------------
Send expression at point to default interpreter. 

For `default' see value of `py-shell-name'

py-execute-expression-python-switch
-----------------------------------
Send expression at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'

py-execute-expression-python-no-switch
--------------------------------------
Send expression at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'

py-execute-expression-python-dedicated
--------------------------------------
Send expression at point to default unique interpreter. 

For `default' see value of `py-shell-name'

py-execute-expression-python-dedicated-switch
---------------------------------------------
Send expression at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'

py-execute-expression-python2
-----------------------------
Send expression at point to Python2 interpreter. 

py-execute-expression-python2-switch
------------------------------------
Send expression at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-expression-python2-no-switch
---------------------------------------
Send expression at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-expression-python2-dedicated
---------------------------------------
Send expression at point to Python2 unique interpreter. 

py-execute-expression-python2-dedicated-switch
----------------------------------------------
Send expression at point to Python2 unique interpreter and switch to result. 

py-execute-expression-python3
-----------------------------
Send expression at point to Python3 interpreter. 

py-execute-expression-python3-switch
------------------------------------
Send expression at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-expression-python3-no-switch
---------------------------------------
Send expression at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-expression-python3-dedicated
---------------------------------------
Send expression at point to Python3 unique interpreter. 

py-execute-expression-python3-dedicated-switch
----------------------------------------------
Send expression at point to Python3 unique interpreter and switch to result. 

py-execute-expression-ipython
-----------------------------
Send expression at point to IPython interpreter. 

py-execute-expression-ipython-switch
------------------------------------
Send expression at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-expression-ipython-no-switch
---------------------------------------
Send expression at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-expression-ipython-dedicated
---------------------------------------
Send expression at point to IPython unique interpreter. 

py-execute-expression-ipython-dedicated-switch
----------------------------------------------
Send expression at point to IPython unique interpreter and switch to result. 

py-execute-expression-ipython2\.7
---------------------------------
Send expression at point to IPython interpreter. 

py-execute-expression-ipython2\.7-switch
----------------------------------------
Send expression at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-expression-ipython2\.7-no-switch
-------------------------------------------
Send expression at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-expression-ipython2\.7-dedicated
-------------------------------------------
Send expression at point to IPython unique interpreter. 

py-execute-expression-ipython2\.7-dedicated-switch
--------------------------------------------------
Send expression at point to IPython unique interpreter and switch to result. 

py-execute-expression-ipython3
------------------------------
Send expression at point to IPython interpreter. 

py-execute-expression-ipython3-switch
-------------------------------------
Send expression at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-expression-ipython3-no-switch
----------------------------------------
Send expression at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-expression-ipython3-dedicated
----------------------------------------
Send expression at point to IPython unique interpreter. 

py-execute-expression-ipython3-dedicated-switch
-----------------------------------------------
Send expression at point to IPython unique interpreter and switch to result. 

py-execute-expression-jython
----------------------------
Send expression at point to Jython interpreter. 

py-execute-expression-jython-switch
-----------------------------------
Send expression at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-expression-jython-no-switch
--------------------------------------
Send expression at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-expression-jython-dedicated
--------------------------------------
Send expression at point to Jython unique interpreter. 

py-execute-expression-jython-dedicated-switch
---------------------------------------------
Send expression at point to Jython unique interpreter and switch to result. 

py-execute-partial-expression-dedicated
---------------------------------------
Send partial-expression to unique interpreter. 

py-execute-partial-expression-python
------------------------------------
Send partial-expression at point to default interpreter. 

For `default' see value of `py-shell-name'

py-execute-partial-expression-python-switch
-------------------------------------------
Send partial-expression at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'

py-execute-partial-expression-python-no-switch
----------------------------------------------
Send partial-expression at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'

py-execute-partial-expression-python-dedicated
----------------------------------------------
Send partial-expression at point to default unique interpreter. 

For `default' see value of `py-shell-name'

py-execute-partial-expression-python-dedicated-switch
-----------------------------------------------------
Send partial-expression at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'

py-execute-partial-expression-python2
-------------------------------------
Send partial-expression at point to Python2 interpreter. 

py-execute-partial-expression-python2-switch
--------------------------------------------
Send partial-expression at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-partial-expression-python2-no-switch
-----------------------------------------------
Send partial-expression at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-partial-expression-python2-dedicated
-----------------------------------------------
Send partial-expression at point to Python2 unique interpreter. 

py-execute-partial-expression-python2-dedicated-switch
------------------------------------------------------
Send partial-expression at point to Python2 unique interpreter and switch to result. 

py-execute-partial-expression-python3
-------------------------------------
Send partial-expression at point to Python3 interpreter. 

py-execute-partial-expression-python3-switch
--------------------------------------------
Send partial-expression at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-partial-expression-python3-no-switch
-----------------------------------------------
Send partial-expression at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-partial-expression-python3-dedicated
-----------------------------------------------
Send partial-expression at point to Python3 unique interpreter. 

py-execute-partial-expression-python3-dedicated-switch
------------------------------------------------------
Send partial-expression at point to Python3 unique interpreter and switch to result. 

py-execute-partial-expression-ipython
-------------------------------------
Send partial-expression at point to IPython interpreter. 

py-execute-partial-expression-ipython-switch
--------------------------------------------
Send partial-expression at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-partial-expression-ipython-no-switch
-----------------------------------------------
Send partial-expression at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-partial-expression-ipython-dedicated
-----------------------------------------------
Send partial-expression at point to IPython unique interpreter. 

py-execute-partial-expression-ipython-dedicated-switch
------------------------------------------------------
Send partial-expression at point to IPython unique interpreter and switch to result. 

py-execute-partial-expression-ipython2\.7
-----------------------------------------
Send partial-expression at point to IPython interpreter. 

py-execute-partial-expression-ipython2\.7-switch
------------------------------------------------
Send partial-expression at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-partial-expression-ipython2\.7-no-switch
---------------------------------------------------
Send partial-expression at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-partial-expression-ipython2\.7-dedicated
---------------------------------------------------
Send partial-expression at point to IPython unique interpreter. 

py-execute-partial-expression-ipython2\.7-dedicated-switch
----------------------------------------------------------
Send partial-expression at point to IPython unique interpreter and switch to result. 

py-execute-partial-expression-ipython3
--------------------------------------
Send partial-expression at point to IPython interpreter. 

py-execute-partial-expression-ipython3-switch
---------------------------------------------
Send partial-expression at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-partial-expression-ipython3-no-switch
------------------------------------------------
Send partial-expression at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-partial-expression-ipython3-dedicated
------------------------------------------------
Send partial-expression at point to IPython unique interpreter. 

py-execute-partial-expression-ipython3-dedicated-switch
-------------------------------------------------------
Send partial-expression at point to IPython unique interpreter and switch to result. 

py-execute-partial-expression-jython
------------------------------------
Send partial-expression at point to Jython interpreter. 

py-execute-partial-expression-jython-switch
-------------------------------------------
Send partial-expression at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-partial-expression-jython-no-switch
----------------------------------------------
Send partial-expression at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-partial-expression-jython-dedicated
----------------------------------------------
Send partial-expression at point to Jython unique interpreter. 

py-execute-partial-expression-jython-dedicated-switch
-----------------------------------------------------
Send partial-expression at point to Jython unique interpreter and switch to result. 

py-execute-minor-block-dedicated
--------------------------------
Send minor-block to unique interpreter. 

py-execute-minor-block-python
-----------------------------
Send minor-block at point to default interpreter. 

For `default' see value of `py-shell-name'

py-execute-minor-block-python-switch
------------------------------------
Send minor-block at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'

py-execute-minor-block-python-no-switch
---------------------------------------
Send minor-block at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'

py-execute-minor-block-python-dedicated
---------------------------------------
Send minor-block at point to default unique interpreter. 

For `default' see value of `py-shell-name'

py-execute-minor-block-python-dedicated-switch
----------------------------------------------
Send minor-block at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'

py-execute-minor-block-python2
------------------------------
Send minor-block at point to Python2 interpreter. 

py-execute-minor-block-python2-switch
-------------------------------------
Send minor-block at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-minor-block-python2-no-switch
----------------------------------------
Send minor-block at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-minor-block-python2-dedicated
----------------------------------------
Send minor-block at point to Python2 unique interpreter. 

py-execute-minor-block-python2-dedicated-switch
-----------------------------------------------
Send minor-block at point to Python2 unique interpreter and switch to result. 

py-execute-minor-block-python3
------------------------------
Send minor-block at point to Python3 interpreter. 

py-execute-minor-block-python3-switch
-------------------------------------
Send minor-block at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-minor-block-python3-no-switch
----------------------------------------
Send minor-block at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-minor-block-python3-dedicated
----------------------------------------
Send minor-block at point to Python3 unique interpreter. 

py-execute-minor-block-python3-dedicated-switch
-----------------------------------------------
Send minor-block at point to Python3 unique interpreter and switch to result. 

py-execute-minor-block-ipython
------------------------------
Send minor-block at point to IPython interpreter. 

py-execute-minor-block-ipython-switch
-------------------------------------
Send minor-block at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-minor-block-ipython-no-switch
----------------------------------------
Send minor-block at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-minor-block-ipython-dedicated
----------------------------------------
Send minor-block at point to IPython unique interpreter. 

py-execute-minor-block-ipython-dedicated-switch
-----------------------------------------------
Send minor-block at point to IPython unique interpreter and switch to result. 

py-execute-minor-block-ipython2\.7
----------------------------------
Send minor-block at point to IPython interpreter. 

py-execute-minor-block-ipython2\.7-switch
-----------------------------------------
Send minor-block at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-minor-block-ipython2\.7-no-switch
--------------------------------------------
Send minor-block at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-minor-block-ipython2\.7-dedicated
--------------------------------------------
Send minor-block at point to IPython unique interpreter. 

py-execute-minor-block-ipython2\.7-dedicated-switch
---------------------------------------------------
Send minor-block at point to IPython unique interpreter and switch to result. 

py-execute-minor-block-ipython3
-------------------------------
Send minor-block at point to IPython interpreter. 

py-execute-minor-block-ipython3-switch
--------------------------------------
Send minor-block at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-minor-block-ipython3-no-switch
-----------------------------------------
Send minor-block at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-minor-block-ipython3-dedicated
-----------------------------------------
Send minor-block at point to IPython unique interpreter. 

py-execute-minor-block-ipython3-dedicated-switch
------------------------------------------------
Send minor-block at point to IPython unique interpreter and switch to result. 

py-execute-minor-block-jython
-----------------------------
Send minor-block at point to Jython interpreter. 

py-execute-minor-block-jython-switch
------------------------------------
Send minor-block at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-minor-block-jython-no-switch
---------------------------------------
Send minor-block at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-minor-block-jython-dedicated
---------------------------------------
Send minor-block at point to Jython unique interpreter. 

py-execute-minor-block-jython-dedicated-switch
----------------------------------------------
Send minor-block at point to Jython unique interpreter and switch to result. 

py-execute-if-block-dedicated
-----------------------------
Send if-block to unique interpreter. 

py-execute-if-block-python
--------------------------
Send if-block at point to default interpreter. 

For `default' see value of `py-shell-name'

py-execute-if-block-python-switch
---------------------------------
Send if-block at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'

py-execute-if-block-python-no-switch
------------------------------------
Send if-block at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'

py-execute-if-block-python-dedicated
------------------------------------
Send if-block at point to default unique interpreter. 

For `default' see value of `py-shell-name'

py-execute-if-block-python-dedicated-switch
-------------------------------------------
Send if-block at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'

py-execute-if-block-python2
---------------------------
Send if-block at point to Python2 interpreter. 

py-execute-if-block-python2-switch
----------------------------------
Send if-block at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-if-block-python2-no-switch
-------------------------------------
Send if-block at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-if-block-python2-dedicated
-------------------------------------
Send if-block at point to Python2 unique interpreter. 

py-execute-if-block-python2-dedicated-switch
--------------------------------------------
Send if-block at point to Python2 unique interpreter and switch to result. 

py-execute-if-block-python3
---------------------------
Send if-block at point to Python3 interpreter. 

py-execute-if-block-python3-switch
----------------------------------
Send if-block at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-if-block-python3-no-switch
-------------------------------------
Send if-block at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-if-block-python3-dedicated
-------------------------------------
Send if-block at point to Python3 unique interpreter. 

py-execute-if-block-python3-dedicated-switch
--------------------------------------------
Send if-block at point to Python3 unique interpreter and switch to result. 

py-execute-if-block-ipython
---------------------------
Send if-block at point to IPython interpreter. 

py-execute-if-block-ipython-switch
----------------------------------
Send if-block at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-if-block-ipython-no-switch
-------------------------------------
Send if-block at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-if-block-ipython-dedicated
-------------------------------------
Send if-block at point to IPython unique interpreter. 

py-execute-if-block-ipython-dedicated-switch
--------------------------------------------
Send if-block at point to IPython unique interpreter and switch to result. 

py-execute-if-block-ipython2\.7
-------------------------------
Send if-block at point to IPython interpreter. 

py-execute-if-block-ipython2\.7-switch
--------------------------------------
Send if-block at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-if-block-ipython2\.7-no-switch
-----------------------------------------
Send if-block at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-if-block-ipython2\.7-dedicated
-----------------------------------------
Send if-block at point to IPython unique interpreter. 

py-execute-if-block-ipython2\.7-dedicated-switch
------------------------------------------------
Send if-block at point to IPython unique interpreter and switch to result. 

py-execute-if-block-ipython3
----------------------------
Send if-block at point to IPython interpreter. 

py-execute-if-block-ipython3-switch
-----------------------------------
Send if-block at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-if-block-ipython3-no-switch
--------------------------------------
Send if-block at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-if-block-ipython3-dedicated
--------------------------------------
Send if-block at point to IPython unique interpreter. 

py-execute-if-block-ipython3-dedicated-switch
---------------------------------------------
Send if-block at point to IPython unique interpreter and switch to result. 

py-execute-if-block-jython
--------------------------
Send if-block at point to Jython interpreter. 

py-execute-if-block-jython-switch
---------------------------------
Send if-block at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-if-block-jython-no-switch
------------------------------------
Send if-block at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-if-block-jython-dedicated
------------------------------------
Send if-block at point to Jython unique interpreter. 

py-execute-if-block-jython-dedicated-switch
-------------------------------------------
Send if-block at point to Jython unique interpreter and switch to result. 

py-execute-try-block-dedicated
------------------------------
Send try-block to unique interpreter. 

py-execute-try-block-python
---------------------------
Send try-block at point to default interpreter. 

For `default' see value of `py-shell-name'

py-execute-try-block-python-switch
----------------------------------
Send try-block at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'

py-execute-try-block-python-no-switch
-------------------------------------
Send try-block at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'

py-execute-try-block-python-dedicated
-------------------------------------
Send try-block at point to default unique interpreter. 

For `default' see value of `py-shell-name'

py-execute-try-block-python-dedicated-switch
--------------------------------------------
Send try-block at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'

py-execute-try-block-python2
----------------------------
Send try-block at point to Python2 interpreter. 

py-execute-try-block-python2-switch
-----------------------------------
Send try-block at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-try-block-python2-no-switch
--------------------------------------
Send try-block at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-try-block-python2-dedicated
--------------------------------------
Send try-block at point to Python2 unique interpreter. 

py-execute-try-block-python2-dedicated-switch
---------------------------------------------
Send try-block at point to Python2 unique interpreter and switch to result. 

py-execute-try-block-python3
----------------------------
Send try-block at point to Python3 interpreter. 

py-execute-try-block-python3-switch
-----------------------------------
Send try-block at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-try-block-python3-no-switch
--------------------------------------
Send try-block at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-try-block-python3-dedicated
--------------------------------------
Send try-block at point to Python3 unique interpreter. 

py-execute-try-block-python3-dedicated-switch
---------------------------------------------
Send try-block at point to Python3 unique interpreter and switch to result. 

py-execute-try-block-ipython
----------------------------
Send try-block at point to IPython interpreter. 

py-execute-try-block-ipython-switch
-----------------------------------
Send try-block at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-try-block-ipython-no-switch
--------------------------------------
Send try-block at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-try-block-ipython-dedicated
--------------------------------------
Send try-block at point to IPython unique interpreter. 

py-execute-try-block-ipython-dedicated-switch
---------------------------------------------
Send try-block at point to IPython unique interpreter and switch to result. 

py-execute-try-block-ipython2\.7
--------------------------------
Send try-block at point to IPython interpreter. 

py-execute-try-block-ipython2\.7-switch
---------------------------------------
Send try-block at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-try-block-ipython2\.7-no-switch
------------------------------------------
Send try-block at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-try-block-ipython2\.7-dedicated
------------------------------------------
Send try-block at point to IPython unique interpreter. 

py-execute-try-block-ipython2\.7-dedicated-switch
-------------------------------------------------
Send try-block at point to IPython unique interpreter and switch to result. 

py-execute-try-block-ipython3
-----------------------------
Send try-block at point to IPython interpreter. 

py-execute-try-block-ipython3-switch
------------------------------------
Send try-block at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-try-block-ipython3-no-switch
---------------------------------------
Send try-block at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-try-block-ipython3-dedicated
---------------------------------------
Send try-block at point to IPython unique interpreter. 

py-execute-try-block-ipython3-dedicated-switch
----------------------------------------------
Send try-block at point to IPython unique interpreter and switch to result. 

py-execute-try-block-jython
---------------------------
Send try-block at point to Jython interpreter. 

py-execute-try-block-jython-switch
----------------------------------
Send try-block at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-try-block-jython-no-switch
-------------------------------------
Send try-block at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-try-block-jython-dedicated
-------------------------------------
Send try-block at point to Jython unique interpreter. 

py-execute-try-block-jython-dedicated-switch
--------------------------------------------
Send try-block at point to Jython unique interpreter and switch to result. 

py-execute-except-block-dedicated
---------------------------------
Send except-block to unique interpreter. 

py-execute-except-block-python
------------------------------
Send except-block at point to default interpreter. 

For `default' see value of `py-shell-name'

py-execute-except-block-python-switch
-------------------------------------
Send except-block at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'

py-execute-except-block-python-no-switch
----------------------------------------
Send except-block at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'

py-execute-except-block-python-dedicated
----------------------------------------
Send except-block at point to default unique interpreter. 

For `default' see value of `py-shell-name'

py-execute-except-block-python-dedicated-switch
-----------------------------------------------
Send except-block at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'

py-execute-except-block-python2
-------------------------------
Send except-block at point to Python2 interpreter. 

py-execute-except-block-python2-switch
--------------------------------------
Send except-block at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-except-block-python2-no-switch
-----------------------------------------
Send except-block at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-except-block-python2-dedicated
-----------------------------------------
Send except-block at point to Python2 unique interpreter. 

py-execute-except-block-python2-dedicated-switch
------------------------------------------------
Send except-block at point to Python2 unique interpreter and switch to result. 

py-execute-except-block-python3
-------------------------------
Send except-block at point to Python3 interpreter. 

py-execute-except-block-python3-switch
--------------------------------------
Send except-block at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-except-block-python3-no-switch
-----------------------------------------
Send except-block at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-except-block-python3-dedicated
-----------------------------------------
Send except-block at point to Python3 unique interpreter. 

py-execute-except-block-python3-dedicated-switch
------------------------------------------------
Send except-block at point to Python3 unique interpreter and switch to result. 

py-execute-except-block-ipython
-------------------------------
Send except-block at point to IPython interpreter. 

py-execute-except-block-ipython-switch
--------------------------------------
Send except-block at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-except-block-ipython-no-switch
-----------------------------------------
Send except-block at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-except-block-ipython-dedicated
-----------------------------------------
Send except-block at point to IPython unique interpreter. 

py-execute-except-block-ipython-dedicated-switch
------------------------------------------------
Send except-block at point to IPython unique interpreter and switch to result. 

py-execute-except-block-ipython2\.7
-----------------------------------
Send except-block at point to IPython interpreter. 

py-execute-except-block-ipython2\.7-switch
------------------------------------------
Send except-block at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-except-block-ipython2\.7-no-switch
---------------------------------------------
Send except-block at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-except-block-ipython2\.7-dedicated
---------------------------------------------
Send except-block at point to IPython unique interpreter. 

py-execute-except-block-ipython2\.7-dedicated-switch
----------------------------------------------------
Send except-block at point to IPython unique interpreter and switch to result. 

py-execute-except-block-ipython3
--------------------------------
Send except-block at point to IPython interpreter. 

py-execute-except-block-ipython3-switch
---------------------------------------
Send except-block at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-except-block-ipython3-no-switch
------------------------------------------
Send except-block at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-except-block-ipython3-dedicated
------------------------------------------
Send except-block at point to IPython unique interpreter. 

py-execute-except-block-ipython3-dedicated-switch
-------------------------------------------------
Send except-block at point to IPython unique interpreter and switch to result. 

py-execute-except-block-jython
------------------------------
Send except-block at point to Jython interpreter. 

py-execute-except-block-jython-switch
-------------------------------------
Send except-block at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-except-block-jython-no-switch
----------------------------------------
Send except-block at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-except-block-jython-dedicated
----------------------------------------
Send except-block at point to Jython unique interpreter. 

py-execute-except-block-jython-dedicated-switch
-----------------------------------------------
Send except-block at point to Jython unique interpreter and switch to result. 

py-execute-top-level-dedicated
------------------------------
Send top-level to unique interpreter. 

py-execute-top-level-python
---------------------------
Send top-level at point to default interpreter. 

For `default' see value of `py-shell-name'

py-execute-top-level-python-switch
----------------------------------
Send top-level at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'

py-execute-top-level-python-no-switch
-------------------------------------
Send top-level at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'

py-execute-top-level-python-dedicated
-------------------------------------
Send top-level at point to default unique interpreter. 

For `default' see value of `py-shell-name'

py-execute-top-level-python-dedicated-switch
--------------------------------------------
Send top-level at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'

py-execute-top-level-python2
----------------------------
Send top-level at point to Python2 interpreter. 

py-execute-top-level-python2-switch
-----------------------------------
Send top-level at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-top-level-python2-no-switch
--------------------------------------
Send top-level at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-top-level-python2-dedicated
--------------------------------------
Send top-level at point to Python2 unique interpreter. 

py-execute-top-level-python2-dedicated-switch
---------------------------------------------
Send top-level at point to Python2 unique interpreter and switch to result. 

py-execute-top-level-python3
----------------------------
Send top-level at point to Python3 interpreter. 

py-execute-top-level-python3-switch
-----------------------------------
Send top-level at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-top-level-python3-no-switch
--------------------------------------
Send top-level at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-top-level-python3-dedicated
--------------------------------------
Send top-level at point to Python3 unique interpreter. 

py-execute-top-level-python3-dedicated-switch
---------------------------------------------
Send top-level at point to Python3 unique interpreter and switch to result. 

py-execute-top-level-ipython
----------------------------
Send top-level at point to IPython interpreter. 

py-execute-top-level-ipython-switch
-----------------------------------
Send top-level at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-top-level-ipython-no-switch
--------------------------------------
Send top-level at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-top-level-ipython-dedicated
--------------------------------------
Send top-level at point to IPython unique interpreter. 

py-execute-top-level-ipython-dedicated-switch
---------------------------------------------
Send top-level at point to IPython unique interpreter and switch to result. 

py-execute-top-level-ipython2\.7
--------------------------------
Send top-level at point to IPython interpreter. 

py-execute-top-level-ipython2\.7-switch
---------------------------------------
Send top-level at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-top-level-ipython2\.7-no-switch
------------------------------------------
Send top-level at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-top-level-ipython2\.7-dedicated
------------------------------------------
Send top-level at point to IPython unique interpreter. 

py-execute-top-level-ipython2\.7-dedicated-switch
-------------------------------------------------
Send top-level at point to IPython unique interpreter and switch to result. 

py-execute-top-level-ipython3
-----------------------------
Send top-level at point to IPython interpreter. 

py-execute-top-level-ipython3-switch
------------------------------------
Send top-level at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-top-level-ipython3-no-switch
---------------------------------------
Send top-level at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-top-level-ipython3-dedicated
---------------------------------------
Send top-level at point to IPython unique interpreter. 

py-execute-top-level-ipython3-dedicated-switch
----------------------------------------------
Send top-level at point to IPython unique interpreter and switch to result. 

py-execute-top-level-jython
---------------------------
Send top-level at point to Jython interpreter. 

py-execute-top-level-jython-switch
----------------------------------
Send top-level at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-top-level-jython-no-switch
-------------------------------------
Send top-level at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-top-level-jython-dedicated
-------------------------------------
Send top-level at point to Jython unique interpreter. 

py-execute-top-level-jython-dedicated-switch
--------------------------------------------
Send top-level at point to Jython unique interpreter and switch to result. 

py-edit-abbrevs
---------------
Jumps to `python-mode-abbrev-table' in a buffer containing lists of abbrev definitions.
You can edit them and type C-c C-c to redefine abbrevs
according to your editing.
Buffer contains a header line for each abbrev table,
 which is the abbrev table name in parentheses.
This is followed by one line per abbrev in that table:
NAME   USECOUNT   EXPANSION   HOOK
where NAME and EXPANSION are strings with quotes,
USECOUNT is an integer, and HOOK is any valid function
or may be omitted (it is usually omitted).  

py-add-abbrev
-------------
Defines python-mode specific abbrev for last expressions before point.
Argument is how many `py-partial-expression's form the expansion; or zero means the region is the expansion.

Reads the abbreviation in the minibuffer; with numeric arg it displays a proposal for an abbrev.
Proposal is composed from the initial character(s) of the
expansion.

Don't use this function in a Lisp program; use `define-abbrev' instead.

py-fill-string-django
---------------------
Fill docstring according to Django's coding standards style.

    """
    Process foo, return bar.
    """

    """
    Process foo, return bar.

    If processing fails throw ProcessingError.
    """

See available styles at `py-fill-paragraph' or var `py-docstring-style'


py-fill-string-onetwo
---------------------
One newline and start and Two at end style.

    """Process foo, return bar."""

    """
    Process foo, return bar.

    If processing fails throw ProcessingError.

    """

See available styles at `py-fill-paragraph' or var `py-docstring-style'


py-fill-string-pep-257
----------------------
PEP-257 with 2 newlines at end of string.

    """Process foo, return bar."""

    """Process foo, return bar.

    If processing fails throw ProcessingError.

    """

See available styles at `py-fill-paragraph' or var `py-docstring-style'


py-fill-string-pep-257-nn
-------------------------
PEP-257 with 1 newline at end of string.

    """Process foo, return bar."""

    """Process foo, return bar.

    If processing fails throw ProcessingError.
    """

See available styles at `py-fill-paragraph' or var `py-docstring-style'


py-fill-string-symmetric
------------------------
Symmetric style.

    """Process foo, return bar."""

    """
    Process foo, return bar.

    If processing fails throw ProcessingError.
    """

See available styles at `py-fill-paragraph' or var `py-docstring-style'


py-set-nil-docstring-style
--------------------------
Set py-docstring-style to 'nil

py-set-pep-257-nn-docstring-style
---------------------------------
Set py-docstring-style to 'pep-257-nn

py-set-pep-257-docstring-style
------------------------------
Set py-docstring-style to 'pep-257

py-set-django-docstring-style
-----------------------------
Set py-docstring-style to 'django

py-set-symmetric-docstring-style
--------------------------------
Set py-docstring-style to 'symmetric

py-set-onetwo-docstring-style
-----------------------------
Set py-docstring-style to 'onetwo

py-fill-comment
---------------
Fill the comment paragraph at point

py-fill-labelled-string
-----------------------
Fill string or paragraph containing lines starting with label

See lp:1066489 

py--string-fence-delete-spaces
------------------------------
Delete spaces following or preceding delimiters of string at point. 

py-fill-string
--------------
String fill function for `py-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'.

Fill according to `py-docstring-style' 

py-shift-left
-------------
Dedent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is dedented.
Returns indentation reached. 

py-shift-right
--------------
Indent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is indented.
Returns indentation reached. 

py-shift-paragraph-right
------------------------
Indent paragraph by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-paragraph-left
-----------------------
Dedent paragraph by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-block-right
--------------------
Indent block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-block-left
-------------------
Dedent block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-minor-block-left
-------------------------
Dedent minor-block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached.
A minor block is started by a `for', `if', `try' or `with'. 

py-shift-minor-block-right
--------------------------
Indent minor-block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached.
A minor block is started by a `for', `if', `try' or `with'. 

py-shift-clause-right
---------------------
Indent clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-clause-left
--------------------
Dedent clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-block-or-clause-right
------------------------------
Indent block-or-clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-block-or-clause-left
-----------------------------
Dedent block-or-clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-def-right
------------------
Indent def by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-def-left
-----------------
Dedent def by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-class-right
--------------------
Indent class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-class-left
-------------------
Dedent class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-def-or-class-right
---------------------------
Indent def-or-class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-def-or-class-left
--------------------------
Dedent def-or-class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-line-right
-------------------
Indent line by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-line-left
------------------
Dedent line by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-statement-right
------------------------
Indent statement by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-statement-left
-----------------------
Dedent statement by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-execute-file-python
----------------------
Send file to Python default interpreter.

py-execute-file-python-switch
-----------------------------
Send file to Python default interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python-no-switch
--------------------------------
Send file to Python default interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil"

py-execute-file-python-dedicated
--------------------------------
Send file to Python default interpreter.

Uses a dedicated shell.

py-execute-file-python-dedicated-switch
---------------------------------------
Send file to Python default interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-ipython
-----------------------
Send file to a Ipython interpreter.

py-execute-file-ipython-switch
------------------------------
Send file to a Ipython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-ipython-no-switch
---------------------------------
Send file to a Ipython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil"

py-execute-file-ipython-dedicated
---------------------------------
Send file to a Ipython interpreter.

Uses a dedicated shell.

py-execute-file-ipython-dedicated-switch
----------------------------------------
Send file to a Ipython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python3
-----------------------
Send file to a Python3 interpreter.

py-execute-file-python3-switch
------------------------------
Send file to a Python3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python3-no-switch
---------------------------------
Send file to a Python3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil"

py-execute-file-python3-dedicated
---------------------------------
Send file to a Python3 interpreter.

Uses a dedicated shell.

py-execute-file-python3-dedicated-switch
----------------------------------------
Send file to a Python3 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python2
-----------------------
Send file to a Python2 interpreter.

py-execute-file-python2-switch
------------------------------
Send file to a Python2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python2-no-switch
---------------------------------
Send file to a Python2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil"

py-execute-file-python2-dedicated
---------------------------------
Send file to a Python2 interpreter.

Uses a dedicated shell.

py-execute-file-python2-dedicated-switch
----------------------------------------
Send file to a Python2 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python2\.7
--------------------------
Send file to a Python2.7 interpreter.

py-execute-file-python2\.7-switch
---------------------------------
Send file to a Python2.7 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python2\.7-no-switch
------------------------------------
Send file to a Python2.7 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil"

py-execute-file-python2\.7-dedicated
------------------------------------
Send file to a Python2.7 interpreter.

Uses a dedicated shell.

py-execute-file-python2\.7-dedicated-switch
-------------------------------------------
Send file to a Python2.7 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-jython
----------------------
Send file to a Jython interpreter.

py-execute-file-jython-switch
-----------------------------
Send file to a Jython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-jython-no-switch
--------------------------------
Send file to a Jython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil"

py-execute-file-jython-dedicated
--------------------------------
Send file to a Jython interpreter.

Uses a dedicated shell.

py-execute-file-jython-dedicated-switch
---------------------------------------
Send file to a Jython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python3\.2
--------------------------
Send file to a Python3.2 interpreter.

py-execute-file-python3\.2-switch
---------------------------------
Send file to a Python3.2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python3\.2-no-switch
------------------------------------
Send file to a Python3.2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil"

py-execute-file-python3\.2-dedicated
------------------------------------
Send file to a Python3.2 interpreter.

Uses a dedicated shell.

py-execute-file-python3\.2-dedicated-switch
-------------------------------------------
Send file to a Python3.2 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python3\.3
--------------------------
Send file to a Python3.3 interpreter.

py-execute-file-python3\.3-switch
---------------------------------
Send file to a Python3.3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python3\.3-no-switch
------------------------------------
Send file to a Python3.3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil"

py-execute-file-python3\.3-dedicated
------------------------------------
Send file to a Python3.3 interpreter.

Uses a dedicated shell.

py-execute-file-python3\.3-dedicated-switch
-------------------------------------------
Send file to a Python3.3 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-section
------------------
Execute section at point.

py-execute-section-python
-------------------------
Execute section at point using python interpreter.

py-execute-section-python2
--------------------------
Execute section at point using python2 interpreter.

py-execute-section-python3
--------------------------
Execute section at point using python3 interpreter.

py-execute-section-ipython
--------------------------
Execute section at point using ipython interpreter.

py-execute-section-ipython2\.7
------------------------------
Execute section at point using ipython2.7 interpreter.

py-execute-section-ipython3
---------------------------
Execute section at point using ipython3 interpreter.

py-execute-section-jython
-------------------------
Execute section at point using jython interpreter.

py-backward-comment
-------------------
Leave commented section upwards,  include empty lines.

Return position reached, if successful

py-beginning-of-comment
-----------------------
Go to the beginning of current line's comment, if any.

Returns position if succesful. 

py-forward-comment
------------------
Go to the end of comment at point.

Returns position, nil if not in comment.

py-uncomment
------------
Uncomment commented lines at point.

If region is active, restrict uncommenting at region 

py-comment-block
----------------
Comments block at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default

py-comment-minor-block
----------------------
Comments a block started by a `for', `if', `try' or `with'.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default

py-comment-top-level
--------------------
Comments top-level form at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default

py-comment-clause
-----------------
Comments clause at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default

py-comment-block-or-clause
--------------------------
Comments block-or-clause at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default

py-comment-def
--------------
Comments def at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default

py-comment-class
----------------
Comments class at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default

py-comment-def-or-class
-----------------------
Comments def-or-class at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default

py-comment-statement
--------------------
Comments statement at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default

py-block
--------
Block at point.

Return code of `py-block' at point, a string. 

py-block-or-clause
------------------
Block-Or-Clause at point.

Return code of `py-block-or-clause' at point, a string. 

py-class
--------
Class at point.

Return code of `py-class' at point, a string. 

py-clause
---------
Clause at point.

Return code of `py-clause' at point, a string. 

py-def
------
Def at point.

Return code of `py-def' at point, a string. 

py-def-or-class
---------------
Def-Or-Class at point.

Return code of `py-def-or-class' at point, a string. 

py-expression
-------------
Expression at point.

Return code of `py-expression' at point, a string. 

py-line
-------
Line at point.

Return code of `py-line' at point, a string. 

py-minor-block
--------------
Minor-Block at point.

Return code of `py-minor-block' at point, a string. 

py-paragraph
------------
Paragraph at point.

Return code of `py-paragraph' at point, a string. 

py-partial-expression
---------------------
Partial-Expression at point.

Return code of `py-partial-expression' at point, a string. 

py-statement
------------
Statement at point.

Return code of `py-statement' at point, a string. 

py-top-level
------------
Top-Level at point.

Return code of `py-top-level' at point, a string. 

py-fast-process
---------------
Connect am (I)Python process suitable for large output.

Output buffer displays "Fast" in name by default
It is not in interactive, i.e. comint-mode, as its bookkeepings seem linked to the freeze reported by lp:1253907

Return the process

py-execute-string-fast
----------------------
Evaluate STRING in Python process which is not in comint-mode.

From a programm use `py--fast-send-string'

py-execute-statement-fast
-------------------------
Process statement at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode 

py-execute-block-fast
---------------------
Process block at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode 

py-execute-block-or-clause-fast
-------------------------------
Process block-or-clause at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode 

py-execute-def-fast
-------------------
Process def at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode 

py-execute-class-fast
---------------------
Process class at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode 

py-execute-def-or-class-fast
----------------------------
Process def-or-class at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode 

py-execute-expression-fast
--------------------------
Process expression at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode 

py-execute-partial-expression-fast
----------------------------------
Process partial-expression at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode 

py-execute-top-level-fast
-------------------------
Process top-level at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode 

py-execute-clause-fast
----------------------
Process clause at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode 

py-narrow-to-block
------------------
Narrow to block at point.

py-narrow-to-block-or-clause
----------------------------
Narrow to block-or-clause at point.

py-narrow-to-class
------------------
Narrow to class at point.

py-narrow-to-clause
-------------------
Narrow to clause at point.

py-narrow-to-def
----------------
Narrow to def at point.

py-narrow-to-def-or-class
-------------------------
Narrow to def-or-class at point.

py-narrow-to-statement
----------------------
Narrow to statement at point.

py-comment-auto-fill
--------------------
Toggles comment-auto-fill mode

py-hide-show
------------
Toggle visibility of existing forms at point. 

py-hide-region
--------------
Hide active region. 

py-show-region
--------------
Un-hide active region. 

py-hide-statement
-----------------
Hide statement at point. 

py-show-statement
-----------------
Show statement at point. 

py-hide-block
-------------
Hide block at point. 

py-show-block
-------------
Show block at point. 

py-hide-clause
--------------
Hide clause at point. 

py-show-clause
--------------
Show clause at point. 

py-hide-block-or-clause
-----------------------
Hide block-or-clause at point. 

py-show-block-or-clause
-----------------------
Show block-or-clause at point. 

py-hide-def
-----------
Hide def at point. 

py-show-def
-----------
Show def at point. 

py-hide-class
-------------
Hide class at point. 

py-show-class
-------------
Show class at point. 

py-hide-expression
------------------
Hide expression at point. 

py-show-expression
------------------
Show expression at point. 

py-hide-partial-expression
--------------------------
Hide partial-expression at point. 

py-show-partial-expression
--------------------------
Show partial-expression at point. 

py-hide-line
------------
Hide line at point. 

py-show-line
------------
Show line at point. 

py-hide-top-level
-----------------
Hide top-level at point. 

py-show-top-level
-----------------
Show top-level at point. 

py-fast-complete
----------------
Complete word before point, if any.

Use `py-fast-process' 

py-load-skeletons
-----------------
Load skeletons from extensions. 

py-python-version
-----------------
Returns versions number of a Python EXECUTABLE, string.

If no EXECUTABLE given, `py-shell-name' is used.
Interactively output of `--version' is displayed. 

py-version
----------
Echo the current version of `python-mode' in the minibuffer.

py-load-file
------------
Load a Python file FILE-NAME into the Python process.

If the file has extension `.py' import or reload it as a module.
Treating it as a module keeps the global namespace clean, provides
function location information for debugging, and supports users of
module-qualified names.

py-proc
-------
Return the current Python process.

Start a new process if necessary. 

py-guess-pdb-path
-----------------
If py-pdb-path isn't set, find location of pdb.py. 

py-switch-shell
---------------
Toggles between the interpreter customized in `py-shell-toggle-1' resp. `py-shell-toggle-2'. Was hard-coded CPython and Jython in earlier versions, now starts with Python2 and Python3 by default.

ARG might be a python-version string to set to.

C-u `py-toggle-shell' prompts to specify a reachable Python command.
C-u followed by numerical arg 2 or 3, `py-toggle-shell' opens a respective Python shell.
C-u followed by numerical arg 5 opens a Jython shell.

Should you need more shells to select, extend this command by adding inside the first cond:

                    ((eq NUMBER (prefix-numeric-value arg))
                     "MY-PATH-TO-SHELL")

py--unfontify-banner
--------------------
Unfontify the shell banner-text.

Cancels `py--timer'
Expects being called by `py--run-unfontify-timer' 

py-set-command-args
-------------------
Set Python arguments on the fly, override defaults in this session.

Use `defcustom' to keep value across sessions 

py-symbol-at-point
------------------
Return the current Python symbol.

py-kill-buffer-unconditional
----------------------------
Kill buffer unconditional, kill buffer-process if existing. 

py-compute-indentation
----------------------
Compute Python indentation.

When HONOR-BLOCK-CLOSE-P is non-nil, statements such as `return',
`raise', `break', `continue', and `pass' force one level of dedenting.

Optional arguments are flags resp. values set and used by `py-compute-indentation' internally:
ORIG keeps original position
ORIGLINE keeps line where compute started
CLOSING is t when started at a char delimiting a list as "]})"
LINE indicates being not at origline now
NESTING tells repeated executing was started from inside a list
REPEAT counter enables checks against `py-max-specpdl-size'
INDENT-OFFSET allows calculation of block-local values
LIEP stores line-end-position at point-of-interest


py-continuation-offset
----------------------
With numeric ARG different from 1 py-continuation-offset is set to that value; returns py-continuation-offset. 

py-indentation-of-statement
---------------------------
Returns the indenation of the statement at point. 

py-list-beginning-position
--------------------------
Return lists beginning position, nil if not inside.

Optional ARG indicates a start-position for `parse-partial-sexp'.

py-end-of-list-position
-----------------------
Return end position, nil if not inside.

Optional ARG indicates a start-position for `parse-partial-sexp'.

py-in-triplequoted-string-p
---------------------------
Returns character address of start tqs-string, nil if not inside. 

py-in-string-p
--------------
if inside a double- triple- or singlequoted string,

If non-nil, return a list composed of
- beginning position
- the character used as string-delimiter (in decimal)
- and length of delimiter, commonly 1 or 3 

py-in-statement-p
-----------------
Returns list of beginning and end-position if inside.

Result is useful for booleans too: (when (py-in-statement-p)...)
will work.


py-backward-top-level-p
-----------------------
Returns position, if cursor is at the beginning of a top-level, nil otherwise. 

py-look-downward-for-clause
---------------------------
If beginning of other clause exists downward in current block.

If succesful return position. 

py-current-defun
----------------
Go to the outermost method or class definition in current scope.

Python value for `add-log-current-defun-function'.
This tells add-log.el how to find the current function/method/variable.
Returns name of class or methods definition, if found, nil otherwise.

See customizable variables `py-current-defun-show' and `py-current-defun-delay'.

py-sort-imports
---------------
Sort multiline imports.

Put point inside the parentheses of a multiline import and hit
M-x py-sort-imports to sort the imports lexicographically

py-which-function
-----------------
Return the name of the function or class, if curser is in, return nil otherwise. 

py-install-local-shells
-----------------------
Builds Python-shell commands from executable found in LOCAL.

If LOCAL is empty, shell-command `find' searches beneath current directory.
Eval resulting buffer to install it, see customizable `py-extensions'. 

py-end-of-string
----------------
Go to end of string at point if any, if successful return position. 

py-which-def-or-class
---------------------
Returns concatenated `def' and `class' names in hierarchical order, if cursor is inside.

Returns "???" otherwise
Used by variable `which-func-functions' 

py--beginning-of-form-intern
----------------------------
Go to beginning of FORM.

With INDENT, go to beginning one level above.
Whit IACT, print result in message buffer.

Returns beginning of FORM if successful, nil otherwise

py-unload-python-el
-------------------
Unloads python-mode delivered by shipped python.el

Removes python-skeleton forms from abbrevs.
These would interfere when inserting forms heading a block

py-backward-top-level
---------------------
Go up to beginning of statments until level of indentation is null.

Returns position if successful, nil otherwise 

py-forward-top-level
--------------------
Go to end of top-level form at point.

Returns position if successful, nil otherwise

py-forward-top-level-bol
------------------------
Go to end of top-level form at point, stop at next beginning-of-line.

Returns position successful, nil otherwise

py-up
-----
Go up or to beginning of form if inside.

If inside a delimited form --string or list-- go to its beginning.
If not at beginning of a statement or block, go to its beginning.
If at beginning of a statement or block, go to beginning one level above of compound statement or definition at point.

py-down
-------
Go to beginning one level below of compound statement or definition at point.

If no statement or block below, but a delimited form --string or list-- go to its beginning. Repeated call from there will behave like down-list.

Returns position if successful, nil otherwise

py-beginning
------------
Go to beginning of compound statement or definition at point.

With C-u, go to beginning one level above.
Returns position if successful, nil otherwise

py-end
------
Go to end of of compound statement or definition at point.

Returns position block if successful, nil otherwise

py-backward-same-level
----------------------
Go form backward keeping indent level if possible.

If inside a delimited form --string or list-- go to its beginning.
If not at beginning of a statement or block, go to its beginning.
If at beginning of a statement or block, go to previous beginning of compound statement or definition at point.
If no further element at same level, go one level up.

py--end-of-paragraph-position
-----------------------------
Returns end of paragraph position. 

py--end-of-comment-position
---------------------------
Returns end of comment position. 

py-pdb
------
Run pdb on program FILE in buffer `*gud-FILE*'.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

At GNU Linux systems required pdb version should be detected by `py--pdb-version', at Windows configure `py-python-ms-pdb-command'

lp:963253

py-sectionize-region
--------------------
Markup code in region as section.

Use current region unless optional args BEG END are delivered.

