Python-Mode commands
====================

py-load-pymacs
--------------
Load Pymacs as delivered with python-mode.el.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.ca

py-insert-default-shebang
-------------------------
Insert in buffer shebang of installed default Python. 

py-electric-comment
-------------------
Insert a comment. If starting a comment, indent accordingly.

If a numeric argument ARG is provided, that many colons are inserted
non-electrically.
With C-u "#" electric behavior is inhibited inside a string or comment.

py-electric-colon
-----------------
Insert a colon and indent accordingly.

If a numeric argument ARG is provided, that many colons are inserted
non-electrically.

Electric behavior is inhibited inside a string or
comment or by universal prefix C-u.
Default is nil, controlled by `py-electric-colon-active-p'

py-electric-backspace
---------------------
Delete preceding character or level of indentation.

With ARG do that ARG times.
Returns column reached. 

py-electric-delete
------------------
Delete following character or levels of whitespace.

With ARG do that ARG times. 

py-indent-line-outmost
----------------------
Indent the current line to the outmost reasonable indent.

With optional C-u an indent with length `py-indent-offset' is inserted unconditionally 

py-indent-line
--------------
Indent the current line according to Python rules.

When called interactivly with C-u, ignore dedenting rules for block closing statements
(e.g. return, raise, break, continue, pass)

This function is normally used by `indent-line-function' resp.
TAB.
Returns current indentation 

py-newline-and-indent
---------------------
Add a newline and indent to outmost reasonable indent.
When indent is set back manually, this is honoured in following lines. 

py-newline-and-dedent
---------------------
Add a newline and indent to one level below current.
Returns column. 

py-guess-indent-offset
----------------------
Guess a value for, and change, `py-indent-offset'.

By default, make a buffer-local copy of `py-indent-offset' with the
new value.
With optional argument GLOBAL change the global value of `py-indent-offset'. 

py-narrow-to-defun
------------------
Make text outside current defun invisible.

The defun visible is the one that contains point or follows point.
Optional CLASS is passed directly to `py-beginning-of-def-or-class'.

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

py-indent-region
----------------
Reindent a region of Python code.

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
initial line; and comment lines beginning in column 1 are ignored.

py-beginning-of-paragraph-position
----------------------------------
Returns beginning of paragraph position. 

py-beginning-of-block-position
------------------------------
Returns beginning of block position. 

py-beginning-of-clause-position
-------------------------------
Returns beginning of clause position. 

py-beginning-of-def-position
----------------------------
Returns beginning of def position. 

py-beginning-of-class-position
------------------------------
Returns beginning of class position. 

py-beginning-of-line-position
-----------------------------
Returns beginning of line position. 

py-beginning-of-statement-position
----------------------------------
Returns beginning of statement position. 

py-end-of-paragraph-position
----------------------------
Returns end of paragraph position. 

py-end-of-block-position
------------------------
Returns end of block position. 

py-end-of-clause-position
-------------------------
Returns end of clause position. 

py-end-of-def-position
----------------------
Returns end of def position. 

py-end-of-class-position
------------------------
Returns end of class position. 

py-end-of-line-position
-----------------------
Returns end of line position. 

py-end-of-statement-position
----------------------------
Returns end of statement position. 

py-bounds-of-declarations
-------------------------
Bounds of consecutive multitude of assigments resp. statements around point.

Indented same level, which don't open blocks.
Typically declarations resp. initialisations of variables following
a class or function definition.
See also py-bounds-of-statements 

py-beginning-of-declarations
----------------------------
Got to the beginning of assigments resp. statements in current level which don't open blocks.


py-end-of-declarations
----------------------
Got to the end of assigments resp. statements in current level which don't open blocks. 

py-declarations
---------------
Copy and mark assigments resp. statements in current level which don't open blocks or start with a keyword.

See also `py-statements', which is more general, taking also simple statements starting with a keyword. 

py-kill-declarations
--------------------
Delete variables declared in current level.

Store deleted variables in kill-ring 

py-bounds-of-statements
-----------------------
Bounds of consecutive multitude of statements around point.

Indented same level, which don't open blocks. 

py-beginning-of-statements
--------------------------
Got to the beginning of statements in current level which don't open blocks. 

py-end-of-statements
--------------------
Got to the end of statements in current level which don't open blocks. 

py-statements
-------------
Copy and mark simple statements in current level which don't open blocks.

More general than py-declarations, which would stop at keywords like a print-statement. 

py-kill-statements
------------------
Delete statements declared in current level.

Store deleted statements in kill-ring 

py-comment-region
-----------------
Like `comment-region' but uses double hash (`#') comment starter.

py-fill-paragraph
-----------------
Like M-q, but handle Python comments and strings.

If any of the current line is a comment, fill the comment or the
paragraph of it that point is in, preserving the comment's indentation
and initial `#'s.
If point is inside a string, narrow to that string and fill.


py-insert-super
---------------
Insert a function "super()" from current environment.

As example given in Python v3.1 documentation » The Python Standard Library »

class C(B):
    def method(self, arg):
        super().method(arg) # This does the same thing as:
                               # super(C, self).method(arg)

py-nesting-level
----------------
Accepts the output of `parse-partial-sexp'. 

py-compute-indentation
----------------------
Compute Python indentation.

When HONOR-BLOCK-CLOSE-P is non-nil, statements such as `return',
`raise', `break', `continue', and `pass' force one level of dedenting.

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

py-preceding-line-backslashed-p
-------------------------------
Return t if preceding line is a backslashed continuation line. 

py-current-line-backslashed-p
-----------------------------
Return t if current line is a backslashed continuation line. 

py-escaped
----------
Return t if char is preceded by an odd number of backslashes. 

py-in-triplequoted-string-p
---------------------------
Returns character address of start tqs-string, nil if not inside. 

py-in-string-p
--------------
Returns character address of start of string, nil if not inside. 

py-in-statement-p
-----------------
Returns list of beginning and end-position if inside.

Result is useful for booleans too: (when (py-in-statement-p)...)
will work.


py-beginning-of-expression-p
----------------------------
Returns position, if cursor is at the beginning of a expression, nil otherwise. 

py-beginning-of-partial-expression-p
------------------------------------
Returns position, if cursor is at the beginning of a expression, nil otherwise. 

py-beginning-of-statement-p
---------------------------
Returns position, if cursor is at the beginning of a statement, nil otherwise. 

py-statement-opens-block-p
--------------------------
Return position if the current statement opens a block
in stricter or wider sense.

For stricter sense specify regexp. 

py-statement-opens-clause-p
---------------------------
Return position if the current statement opens block or clause. 

py-statement-opens-block-or-clause-p
------------------------------------
Return position if the current statement opens block or clause. 

py-statement-opens-class-p
--------------------------
Return `t' if the statement opens a functions or class definition, nil otherwise. 

py-statement-opens-def-p
------------------------
Return `t' if the statement opens a functions or class definition, nil otherwise. 

py-statement-opens-def-or-class-p
---------------------------------
Return `t' if the statement opens a functions or class definition, nil otherwise. 

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

empty-line-p
------------
Returns t if cursor is at an line with nothing but whitespace-characters, nil otherwise.

py-count-lines
--------------
Count lines in buffer, optional without given boundaries.
Ignores common region.

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=7115

py-which-function
-----------------
Return the name of the function or class, if curser is in, return nil otherwise. 

py-beginning-of-block
---------------------
Looks up for nearest opening block, i.e. compound statement

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-beginning-of-if-block
------------------------
Looks up for nearest opening if-block, i.e. compound statement

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-beginning-of-try-block
-------------------------
Looks up for nearest opening try-block, i.e. compound statement.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-end-of-block
---------------
Go to the end of a compound statement.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-beginning-of-block-or-clause
-------------------------------
Looks up for nearest opening clause or block.

With universal argument looks for next compound statements
i.e. blocks only.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-end-of-block-or-clause
-------------------------
Without arg, go to the end of a compound statement.

With arg , move point to end of clause at point.
Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-beginning-of-class
---------------------
Move point to start of next `class'.

See also `py-beginning-of-def-or-class'.
Returns position reached, if any, nil otherwise.

py-end-of-class
---------------
Move point beyond next method definition.

Returns position reached, if any, nil otherwise.

py-beginning-of-clause
----------------------
Looks up for nearest opening clause, i.e. a compound statements
subform.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-end-of-clause
----------------
Without arg, go to the end of a compound statement.

With arg , move point to end of clause at point.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-beginning-of-def
-------------------
Move point to start of `def'.

Returns position reached, if any, nil otherwise 

py-end-of-def
-------------
Move point beyond next method definition.

Returns position reached, if any, nil otherwise.

py-beginning-of-def-or-class
----------------------------
Move point to start of `def' or `class', whatever is next.

With optional universal arg CLASS, move to the beginn of class definition.
Returns position reached, if any, nil otherwise 

py-end-of-def-or-class
----------------------
Move point beyond next `def' or `class' definition.

With optional universal arg, move to the end of class exclusively.
Returns position reached, if any, nil otherwise.

py-beginning-of-expression
--------------------------
Go to the beginning of a compound python expression.

A a compound python expression might be concatenated by "." operator, thus composed by minor python expressions.

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes.


py-end-of-expression
--------------------
Go to the end of a compound python expression.

A a compound python expression might be concatenated by "." operator, thus composed by minor python expressions.

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference

Operators however are left aside resp. limit py-expression designed for edit-purposes. 

py-beginning-of-partial-expression
----------------------------------
Go to the beginning of a minor python expression.

"." operators delimit a minor expression on their level.
Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes. 

py-end-of-partial-expression
----------------------------
Go to the end of a minor python expression.

"." operators delimit a minor expression on their level.
Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes. 

py-beginning-of-statement
-------------------------
Go to the initial line of a simple statement.

For beginning of compound statement use py-beginning-of-block.
For beginning of clause py-beginning-of-clause.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html


py-end-of-statement
-------------------
Go to the point just beyond the final line of the current statement. 

py-goto-statement-below
-----------------------
Goto beginning of next statement. 

py-mark-expression
------------------
Mark expression at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-partial-expression
--------------------------
Mark partial-expression at point.

Returns beginning and end positions of marked area, a cons.
"." operators delimit a partial-expression expression on it's level, that's the difference to compound expressions. 

py-mark-statement
-----------------
Mark statement at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-block
-------------
Mark block at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-block-or-clause
-----------------------
Mark block-or-clause at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-def-or-class
--------------------
Mark def-or-class at point.

With universal argument or `py-mark-decorators' set to `t' decorators are marked too.
Returns beginning and end positions of marked area, a cons.

py-mark-class
-------------
Mark class at point.

With universal argument or `py-mark-decorators' set to `t' decorators are marked too.
Returns beginning and end positions of marked area, a cons.

py-mark-def
-----------
Mark def at point.

With universal argument or `py-mark-decorators' set to `t' decorators are marked too.
Returns beginning and end positions of marked area, a cons.

py-mark-clause
--------------
Mark clause at point.

Returns beginning and end positions of marked area, a cons. 

py-beginning-of-decorator
-------------------------
Go to the beginning of a decorator.

Returns position if succesful 

py-end-of-decorator
-------------------
Go to the end of a decorator.

Returns position if succesful 

py-copy-expression
------------------
Mark expression at point.

Returns beginning and end positions of marked area, a cons. 

py-copy-partial-expression
--------------------------
Mark partial-expression at point.

Returns beginning and end positions of marked area, a cons.

"." operators delimit a partial-expression expression on it's level, that's the difference to compound expressions.

Given the function below, `py-partial-expression'
called at pipe symbol would copy and return:

def usage():
    print """Usage: %s
    ....""" % (
        os.path.basename(sys.argv[0]))
------------|-------------------------
==> path

        os.path.basename(sys.argv[0]))
------------------|-------------------
==> basename(sys.argv[0]))

        os.path.basename(sys.argv[0]))
--------------------------|-----------
==> sys

        os.path.basename(sys.argv[0]))
------------------------------|-------
==> argv[0]

while `py-expression' would copy and return

(
        os.path.basename(sys.argv[0]))

;;;;;

Also for existing commands a shorthand is defined:

(defalias 'py-statement 'py-copy-statement)

py-copy-statement
-----------------
Mark statement at point.

Returns beginning and end positions of marked area, a cons. 

py-copy-block
-------------
Mark block at point.

Returns beginning and end positions of marked area, a cons. 

py-copy-block-or-clause
-----------------------
Mark block-or-clause at point.

Returns beginning and end positions of marked area, a cons. 

py-copy-def
-----------
Mark def at point.

With universal argument or `py-mark-decorators' set to `t' decorators are copied too.
Returns beginning and end positions of marked area, a cons.

py-copy-def-or-class
--------------------
Mark def-or-class at point.

With universal argument or `py-mark-decorators' set to `t' decorators are copied too.
Returns beginning and end positions of marked area, a cons.

py-copy-class
-------------
Mark class at point.

With universal argument or `py-mark-decorators' set to `t' decorators are copied too.
Returns beginning and end positions of marked area, a cons.

py-copy-clause
--------------
Mark clause at point.
  Returns beginning and end positions of marked area, a cons. 

py-kill-expression
------------------
Delete expression at point.
  Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-partial-expression
--------------------------
Delete partial-expression at point.
  Stores data in kill ring. Might be yanked back using `C-y'.

"." operators delimit a partial-expression expression on it's level, that's the difference to compound expressions.

py-kill-statement
-----------------
Delete statement at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-block
-------------
Delete block at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-block-or-clause
-----------------------
Delete block-or-clause at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-def-or-class
--------------------
Delete def-or-class at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-class
-------------
Delete class at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-def
-----------
Delete def at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-clause
--------------
Delete clause at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-forward-line
---------------
Goes to end of line after forward move.

Travels right-margin comments. 

py-beginning-of-comment
-----------------------
Go to the beginning of current line's comment, if any. 

py-leave-comment-or-string-backward
-----------------------------------
If inside a comment or string, leave it backward. 

py-beginning-of-list-pps
------------------------
Go to the beginning of a list.
Optional ARG indicates a start-position for `parse-partial-sexp'.
Return beginning position, nil if not inside.

py-beginning-of-list
--------------------
Go to beginning of any parentized, braced or bracketed expression in statement. 

py-end-of-list
--------------
Go to end of any parentized, braced or bracketed expression in statement. 

py-down-block-lc
----------------
Goto beginning of line following end of block.

Returns position reached, if successful, nil otherwise.

"-lc" stands for "left-corner" - a complementary command travelling left, whilst `py-end-of-block' stops at right corner.

See also `py-down-block': down from current definition to next beginning of block below. 

py-down-clause-lc
-----------------
Goto beginning of line following end of clause.

Returns position reached, if successful, nil otherwise.

"-lc" stands for "left-corner" - a complementary command travelling left, whilst `py-end-of-clause' stops at right corner.

See also `py-down-clause': down from current definition to next beginning of clause below. 

py-down-def-lc
--------------
Goto beginning of line following end of def.

Returns position reached, if successful, nil otherwise.

"-lc" stands for "left-corner" - a complementary command travelling left, whilst `py-end-of-def' stops at right corner.

See also `py-down-def': down from current definition to next beginning of def below. 

py-down-class-lc
----------------
Goto beginning of line following end of class.

Returns position reached, if successful, nil otherwise.

"-lc" stands for "left-corner" - a complementary command travelling left, whilst `py-end-of-class' stops at right corner.

See also `py-down-class': down from current definition to next beginning of class below. 

py-down-statement-lc
--------------------
Goto beginning of line following end of statement.

Returns position reached, if successful, nil otherwise.

"-lc" stands for "left-corner" - a complementary command travelling left, whilst `py-end-of-statement' stops at right corner.

See also `py-down-statement': down from current definition to next beginning of statement below. 

py-down-statement
-----------------
Go to the beginning of next statement below in buffer.

Returns indentation if statement found, nil otherwise. 

py-down-block
-------------
Go to the beginning of next block below in buffer.

Returns indentation if block found, nil otherwise. 

py-down-clause
--------------
Go to the beginning of next clause below in buffer.

Returns indentation if clause found, nil otherwise. 

py-down-block-or-clause
-----------------------
Go to the beginning of next block-or-clause below in buffer.

Returns indentation if block-or-clause found, nil otherwise. 

py-down-def
-----------
Go to the beginning of next def below in buffer.

Returns indentation if def found, nil otherwise. 

py-down-class
-------------
Go to the beginning of next class below in buffer.

Returns indentation if class found, nil otherwise. 

py-down-def-or-class
--------------------
Go to the beginning of next def-or-class below in buffer.

Returns indentation if def-or-class found, nil otherwise. 

py-forward-into-nomenclature
----------------------------
Move forward to end of a nomenclature section or word.

With C-u (programmatically, optional argument ARG), do it that many times.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores.

py-backward-into-nomenclature
-----------------------------
Move backward to beginning of a nomenclature section or word.

With optional ARG, move that many times.  If ARG is negative, move
forward.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores.

match-paren
-----------
Go to the matching brace, bracket or parenthesis if on its counterpart.

Otherwise insert the character, the key is assigned to, here `%'.
With universal arg  insert a `%'. 

py-toggle-execute-keep-temporary-file-p
---------------------------------------
Toggle py-execute-keep-temporary-file-p 

py-guess-default-python
-----------------------
If any Python is installed. Used by `py-shell' if `py-shell-name' is neither selected nor has a customized default value. 

py-shell-dedicated
------------------
Start an interactive Python interpreter in another window.

With optional C-u user is prompted by
`py-choose-shell' for command and options to pass to the Python
interpreter.


py-shell
--------
Start an interactive Python interpreter in another window.

With optional C-u user is prompted by
`py-choose-shell' for command and options to pass to the Python
interpreter.
Returns variable `py-process-name' used by function `get-process'.


python
------
Start an Python interpreter in another window.

With optional C-u user is prompted
for options to pass to the Python interpreter. 

python2
-------
Start an Python2 interpreter in another window.

With optional C-u user is prompted
for options to pass to the Python2 interpreter. 

python2\.7
----------
Start an Python2.7 interpreter in another window.

With optional C-u user is prompted
for options to pass to the Python2.7 interpreter. 

python3
-------
Start an Python3 interpreter in another window.

With optional C-u user is prompted
for options to pass to the Python3 interpreter. 

python3\.2
----------
Start an Python3.2 interpreter in another window.

With optional C-u user is prompted
for options to pass to the Python3.2 interpreter. 

ipython
-------
Start an IPython interpreter in another window.

With optional C-u user is prompted
for options to pass to the IPython interpreter. 

jython
------
Start an Jython interpreter in another window.

With optional C-u user is prompted
for options to pass to the Jython interpreter. 

python-dedicated
----------------
Start an unique Python interpreter in another window.

With optional C-u user is prompted
for options to pass to the Python interpreter. 

python2-dedicated
-----------------
Start an unique Python2 interpreter in another window.

With optional C-u user is prompted
for options to pass to the Python2 interpreter. 

python2\.7-dedicated
--------------------
Start an unique Python2.7 interpreter in another window.

With optional C-u user is prompted
for options to pass to the Python2.7 interpreter. 

python3-dedicated
-----------------
Start an unique Python3 interpreter in another window.

With optional C-u user is prompted
for options to pass to the Python3 interpreter. 

python3\.2-dedicated
--------------------
Start an unique Python3.2 interpreter in another window.

With optional C-u user is prompted
for options to pass to the Python3.2 interpreter. 

ipython-dedicated
-----------------
Start an unique IPython interpreter in another window.

With optional C-u user is prompted
for options to pass to the IPython interpreter. 

jython-dedicated
----------------
Start an unique Jython interpreter in another window.

With optional C-u user is prompted
for options to pass to the Jython interpreter. 

py-which-execute-file-command
-----------------------------
Return the command appropriate to Python version.

Per default it's "(format "execfile(r'%s') # PYTHON-MODE\n" filename)" for Python 2 series.

py-execute-region-no-switch
---------------------------
Send the region to a common shell calling a Python interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', buffer with region stays current.
 

py-execute-region-switch
------------------------
Send the region to a common shell calling a Python interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to.


py-execute-region
-----------------
Send the region to a common shell calling a Python interpreter. 

py-execute-region-dedicated
---------------------------
Get the region processed by an unique Python interpreter. 

py-execute-string
-----------------
Send the argument STRING to a Python interpreter.

See the `M-x py-execute-region' docs for an account of some
subtleties, including the use of the optional ASYNC argument.

py-shell-command-on-region
--------------------------
Execute region in a shell.

Avoids writing to temporary files.

Caveat: Can't be used for expressions containing
Unicode strings like u'\xA9' 

py-ipython-shell-command-on-region
----------------------------------
Execute region in a shell.

Avoids writing to temporary files.

Caveat: Can't be used for expressions containing
Unicode strings like u'\xA9' 

py-send-region-ipython
----------------------
Execute the region through an ipython shell. 

py-execute-region-in-shell
--------------------------
Execute the region in a Python shell. 

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

See the `M-x py-execute-region' docs for an account of some
subtleties, including the use of the optional ASYNC argument.

This may be preferable to `M-x py-execute-buffer' because:

 - Definitions stay in their module rather than appearing at top
   level, where they would clutter the global namespace and not affect
   uses of qualified names (MODULE.NAME).

 - The Python debugger gets line number information about the functions.

py-execute-buffer
-----------------
Send the contents of the buffer to a Python interpreter.

If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.
If there is a *Python* process buffer, it is used.  
If a clipping restriction is in effect, only the accessible portion of the buffer is sent. A trailing newline will be supplied if needed.

See the `M-x py-execute-region' docs for an account of some
subtleties, including the use of the optional ASYNC argument.

py-execute-buffer-no-switch
---------------------------
Like `py-execute-buffer', but ignores setting of `py-shell-switch-buffers-on-execute'.

Buffer called from is current afterwards again.

py-execute-buffer-switch
------------------------
Like `py-execute-buffer', but ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. 

py-execute-region-python
------------------------
Send the region to a common shell calling the python interpreter. 

py-execute-region-python-switch
-------------------------------
Send the region to a common shell calling the python interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. 

py-execute-region-python-no-switch
----------------------------------
Send the region to a common shell calling the python interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to.

py-execute-region-python2
-------------------------
Send the region to a common shell calling the python2 interpreter. 

py-execute-region-python2-switch
--------------------------------
Send the region to a common shell calling the python2 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. 

py-execute-region-python2-no-switch
-----------------------------------
Send the region to a common shell calling the python2 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to.

py-execute-region-python2\.7
----------------------------
Send the region to a common shell calling the python2.7 interpreter. 

py-execute-region-python2\.7-switch
-----------------------------------
Send the region to a common shell calling the python2.7 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. 

py-execute-region-python2\.7-no-switch
--------------------------------------
Send the region to a common shell calling the python2.7 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to.

py-execute-region-python3
-------------------------
Send the region to a common shell calling the python3 interpreter. 

py-execute-region-python3-switch
--------------------------------
Send the region to a common shell calling the python3 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. 

py-execute-region-python3-no-switch
-----------------------------------
Send the region to a common shell calling the python3 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to.

py-execute-region-python3\.2
----------------------------
Send the region to a common shell calling the python3.2 interpreter. 

py-execute-region-python3\.2-switch
-----------------------------------
Send the region to a common shell calling the python3.2 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. 

py-execute-region-python3\.2-no-switch
--------------------------------------
Send the region to a common shell calling the python3.2 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to.

py-execute-region-ipython
-------------------------
Send the region to a common shell calling the ipython interpreter. 

py-execute-region-ipython-switch
--------------------------------
Send the region to a common shell calling the ipython interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. 

py-execute-region-ipython-no-switch
-----------------------------------
Send the region to a common shell calling the ipython interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to.

py-execute-region-jython
------------------------
Send the region to a common shell calling the jython interpreter. 

py-execute-region-jython-switch
-------------------------------
Send the region to a common shell calling the jython interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. 

py-execute-region-jython-no-switch
----------------------------------
Send the region to a common shell calling the jython interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to.

py-execute-defun
----------------
Send the current defun (class or method) to the inferior Python process.

py-process-file
---------------
Process "python filename".

Optional OUTPUT-BUFFER and ERROR-BUFFER might be given. 

py-exec-execfile-region
-----------------------
Execute the region in a Python interpreter. 

py-exec-execfile
----------------
Process "python filename",
Optional OUTPUT-BUFFER and ERROR-BUFFER might be given.')


py-execute-block
----------------
Send python-form at point as is to Python interpreter. 

py-execute-block-or-clause
--------------------------
Send python-form at point as is to Python interpreter. 

py-execute-class
----------------
Send python-form at point as is to Python interpreter. 

py-execute-clause
-----------------
Send python-form at point as is to Python interpreter. 

py-execute-def
--------------
Send python-form at point as is to Python interpreter. 

py-execute-def-or-class
-----------------------
Send python-form at point as is to Python interpreter. 

py-execute-expression
---------------------
Send python-form at point as is to Python interpreter. 

py-execute-partial-expression
-----------------------------
Send python-form at point as is to Python interpreter. 

py-execute-statement
--------------------
Send python-form at point as is to Python interpreter. 

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

py-output-buffer-filter
-----------------------
Clear output buffer from py-shell-input prompt etc. 

py-send-string
--------------
Evaluate STRING in inferior Python process.

py-pdbtrack-toggle-stack-tracking
---------------------------------
Set variable `py-pdbtrack-do-tracking-p'. 

turn-on-pdbtrack
----------------


turn-off-pdbtrack
-----------------


py-fetch-docu
-------------
Lookup in current buffer for the doku for the symbol at point.

Useful for newly defined symbol, not known to python yet. 

py-describe-symbol
------------------
Print help on symbol at point. 

py-describe-mode
----------------
Dump long form of Python-mode docs.

py-find-function
----------------
Find source of definition of function NAME.

Interactively, prompt for name.

py-find-imports
---------------
Find top-level imports, updating `python-imports'.

py-update-imports
-----------------
Returns `python-imports'.

Imports done are displayed in message buffer. 

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

py-close-clause
---------------
Set indent level to that of beginning of clause definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. 

py-close-block
--------------
Set indent level to that of beginning of block definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. 

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
Inserts a print statement out of current `(car kill-ring)' by default, inserts ARG instead if delivered. 

py-line-to-printform-python2
----------------------------
Transforms the item on current in a print statement. 

py-switch-imenu-index-function
------------------------------
For development only. Good old renamed `py-imenu-create-index'-function hangs with medium size files already. Working `py-imenu-create-index-new' is active by default.

Switch between classic index machine `py-imenu-create-index'-function and new `py-imenu-create-index-new'.

The former may provide a more detailed report, thus delivering two different index-machines is considered. 

py-choose-shell-by-shebang
--------------------------
Choose shell by looking at #! on the first line.

Returns the specified Python resp. Jython shell command name. 

py-which-python
---------------
Returns version of Python of current default environment, a number. 

py-python-default-environment
-----------------------------
Returns path of Python default installation. 

py-toggle-shells
----------------
Toggles between the CPython and Jython default interpreter.

ARG might be a python-version string to set to.
With C-u) user is prompted for the command to use.
If no arg given and py-shell-name not set yet, shell is set according to `py-shell-name' 

py-choose-shell
---------------
Looks for an appropriate mode function.
This does the following:
 - reads py-shell-name
 - look for an interpreter with `py-choose-shell-by-shebang'
 - examine imports using `py-choose-shell-by-import'
 - default to the variable `py-shell-name'

With C-u) user is prompted to specify a reachable Python version.

python-mode
-----------
Major mode for editing Python files.

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
s-I             py-indent-line
s-i             py-indent-forward-line
s-s             suche-settrace
<C-backspace>   py-hungry-delete-backwards
<C-return>      py-newline-and-dedent
<backspace>     py-electric-backspace
<delete>        py-electric-delete
<s-backspace>   py-dedent-forward-line

C-x n           Prefix Command

C-M-a           py-beginning-of-def-or-class
C-M-e           py-end-of-def-or-class
C-M-h           py-mark-def-or-class
M-TAB           completion-at-point
C-M-n           py-end-of-block
C-M-p           py-beginning-of-block
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
py-shell-name		shell command to invoke Python interpreter
py-temp-directory		directory used for temp files (if needed)
py-beep-if-tab-change		ring the bell if `tab-width' is changed

py-def-or-class-beginning-position
----------------------------------
Returns beginning position of function or class definition. 

py-def-or-class-end-position
----------------------------
Returns end position of function or class definition. 

py-statement-beginning-position
-------------------------------
Returns beginning position of statement. 

py-statement-end-position
-------------------------
Returns end position of statement. 

py-current-indentation
----------------------
Returns beginning position of code in line. 

py-version
----------
Echo the current version of `python-mode' in the minibuffer.

run-python
----------
Run an inferior Python process, input and output via buffer *Python*.

CMD is the Python command to run.  NOSHOW non-nil means don't
show the buffer automatically.

Interactively, a prefix arg means to prompt for the initial
Python command line (default is `python-command').

A new process is started if one isn't running attached to
`python-buffer', or if called from Lisp with non-nil arg NEW.
Otherwise, if a process is already running in `python-buffer',
switch to that buffer.

This command runs the hook `inferior-python-mode-hook' after
running `comint-mode-hook'.  Type C-h m in the
process buffer for a list of commands.

By default, Emacs inhibits the loading of Python modules from the
current working directory, for security reasons.  To disable this
behavior, change `python-remove-cwd-from-path' to nil.

py-send-region
--------------
Send the region to the inferior Python process.

py-send-buffer
--------------
Send the current buffer to the inferior Python process.

py-switch-to-python
-------------------
Switch to the Python process buffer, maybe starting new process.

With prefix arg, position cursor at end of buffer.

py-send-region-and-go
---------------------
Send the region to the inferior Python process.

Then switch to the process buffer.

py-load-file
------------
Load a Python file FILE-NAME into the inferior Python process.

If the file has extension `.py' import or reload it as a module.
Treating it as a module keeps the global namespace clean, provides
function location information for debugging, and supports users of
module-qualified names.

py-set-proc
-----------
Set the default value of `python-buffer' to correspond to this buffer.

If the current buffer has a local value of `python-buffer', set the
default (global) value to that.  The associated Python process is
the one that gets input from M-x py-send-region et al when used
in a buffer that doesn't have a local value of `python-buffer'.

py-shell-redirect-send-command-to-process
-----------------------------------------
Send COMMAND to PROCESS, with output to OUTPUT-BUFFER.
With prefix arg, echo output in process buffer.

If NO-DISPLAY is non-nil, do not show the output buffer.

py-shell-complete
-----------------
Complete word before point, if any. Otherwise insert TAB. 

ipython-complete
----------------
Complete the python symbol before point. 

Only knows about the stuff in the current *Python* session.

