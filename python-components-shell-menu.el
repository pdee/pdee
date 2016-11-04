;; python-components-shell-menu.el --- Provide the Py-Shell mode menu -*- lexical-binding: t; -*- 

;; This file not shipped as part of GNU Emacs.

;; Copyright (C) 2015-2016 Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>

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

;;; Code:

(and (ignore-errors (require 'easymenu) t)
     ;; (easy-menu-define py-menu map "Python Tools"
     ;;           `("PyTools"
     (easy-menu-define
       py-shell-menu py-python-shell-mode-map "Py-Shell Mode menu"
       `("Py-Shell"
         ("Edit"
          ("Shift"
           ("Shift right"
	    ["Shift block right" py-shift-block-right
	     :help " `py-shift-block-right'
Indent block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift block or clause right" py-shift-block-or-clause-right
	     :help " `py-shift-block-or-clause-right'
Indent block-or-clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift class right" py-shift-class-right
	     :help " `py-shift-class-right'
Indent class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift clause right" py-shift-clause-right
	     :help " `py-shift-clause-right'
Indent clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift comment right" py-shift-comment-right
	     :help " `py-shift-comment-right'"]

	    ["Shift def right" py-shift-def-right
	     :help " `py-shift-def-right'
Indent def by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift def or class right" py-shift-def-or-class-right
	     :help " `py-shift-def-or-class-right'
Indent def-or-class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift minor block right" py-shift-minor-block-right
	     :help " `py-shift-minor-block-right'
Indent minor-block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached.
A minor block is started by a `for', `if', `try' or `with'."]

	    ["Shift paragraph right" py-shift-paragraph-right
	     :help " `py-shift-paragraph-right'
Indent paragraph by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift region right" py-shift-region-right
	     :help " `py-shift-region-right'
Indent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is indented.
Returns indentation reached."]

	    ["Shift statement right" py-shift-statement-right
	     :help " `py-shift-statement-right'
Indent statement by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift top level right" py-shift-top-level-right
	     :help " `py-shift-top-level-right'"]
            )
           ("Shift left"
	    ["Shift block left" py-shift-block-left
	     :help " `py-shift-block-left'
Dedent block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift block or clause left" py-shift-block-or-clause-left
	     :help " `py-shift-block-or-clause-left'
Dedent block-or-clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift class left" py-shift-class-left
	     :help " `py-shift-class-left'
Dedent class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift clause left" py-shift-clause-left
	     :help " `py-shift-clause-left'
Dedent clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift comment left" py-shift-comment-left
	     :help " `py-shift-comment-left'"]

	    ["Shift def left" py-shift-def-left
	     :help " `py-shift-def-left'
Dedent def by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift def or class left" py-shift-def-or-class-left
	     :help " `py-shift-def-or-class-left'
Dedent def-or-class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift minor block left" py-shift-minor-block-left
	     :help " `py-shift-minor-block-left'
Dedent minor-block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached.
A minor block is started by a `for', `if', `try' or `with'."]

	    ["Shift paragraph left" py-shift-paragraph-left
	     :help " `py-shift-paragraph-left'
Dedent paragraph by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift region left" py-shift-region-left
	     :help " `py-shift-region-left'
Dedent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is dedented.
Returns indentation reached."]

	    ["Shift statement left" py-shift-statement-left
	     :help " `py-shift-statement-left'
Dedent statement by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]
            ))
          ("Mark"
	   ["Mark block" py-mark-block
	    :help " `py-mark-block'
Mark block at point.

Returns beginning and end positions of marked area, a cons."]

	   ["Mark block or clause" py-mark-block-or-clause
	    :help " `py-mark-block-or-clause'
Mark block-or-clause at point.

Returns beginning and end positions of marked area, a cons."]

	   ["Mark class" py-mark-class
	    :help " `py-mark-class'
Mark class at point.

With C-u or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of marked area, a cons."]

	   ["Mark clause" py-mark-clause
	    :help " `py-mark-clause'
Mark clause at point.

Returns beginning and end positions of marked area, a cons."]

	   ["Mark comment" py-mark-comment
	    :help " `py-mark-comment'
Mark comment at point.

Returns beginning and end positions of marked area, a cons."]

	   ["Mark def" py-mark-def
	    :help " `py-mark-def'
Mark def at point.

With C-u or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of marked area, a cons."]

	   ["Mark def or class" py-mark-def-or-class
	    :help " `py-mark-def-or-class'
Mark def-or-class at point.

With C-u or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of marked area, a cons."]

	   ["Mark expression" py-mark-expression
	    :help " `py-mark-expression'
Mark expression at point.

Returns beginning and end positions of marked area, a cons."]

	   ["Mark line" py-mark-line
	    :help " `py-mark-line'
Mark line at point.

Returns beginning and end positions of marked area, a cons."]

	   ["Mark minor block" py-mark-minor-block
	    :help " `py-mark-minor-block'
Mark minor-block at point.

Returns beginning and end positions of marked area, a cons."]

	   ["Mark paragraph" py-mark-paragraph
	    :help " `py-mark-paragraph'
Mark paragraph at point.

Returns beginning and end positions of marked area, a cons."]

	   ["Mark partial expression" py-mark-partial-expression
	    :help " `py-mark-partial-expression'
Mark partial-expression at point.

Returns beginning and end positions of marked area, a cons."]

	   ["Mark statement" py-mark-statement
	    :help " `py-mark-statement'
Mark statement at point.

Returns beginning and end positions of marked area, a cons."]

	   ["Mark top level" py-mark-top-level
	    :help " `py-mark-top-level'
Mark top-level at point.

Returns beginning and end positions of marked area, a cons."]
           )
          ("Copy"
	   ["Copy block" py-copy-block
	    :help " `py-copy-block'
Copy block at point.

Store data in kill ring, so it might yanked back."]

	   ["Copy block or clause" py-copy-block-or-clause
	    :help " `py-copy-block-or-clause'
Copy block-or-clause at point.

Store data in kill ring, so it might yanked back."]

	   ["Copy class" py-copy-class
	    :help " `py-copy-class'
Copy class at point.

Store data in kill ring, so it might yanked back."]

	   ["Copy clause" py-copy-clause
	    :help " `py-copy-clause'
Copy clause at point.

Store data in kill ring, so it might yanked back."]

	   ["Copy comment" py-copy-comment
	    :help " `py-copy-comment'"]

	   ["Copy def" py-copy-def
	    :help " `py-copy-def'
Copy def at point.

Store data in kill ring, so it might yanked back."]

	   ["Copy def or class" py-copy-def-or-class
	    :help " `py-copy-def-or-class'
Copy def-or-class at point.

Store data in kill ring, so it might yanked back."]

	   ["Copy expression" py-copy-expression
	    :help " `py-copy-expression'
Copy expression at point.

Store data in kill ring, so it might yanked back."]

	   ["Copy line" py-copy-line
	    :help " `py-copy-line'"]

	   ["Copy minor block" py-copy-minor-block
	    :help " `py-copy-minor-block'
Copy minor-block at point.

Store data in kill ring, so it might yanked back."]

	   ["Copy paragraph" py-copy-paragraph
	    :help " `py-copy-paragraph'"]

	   ["Copy partial expression" py-copy-partial-expression
	    :help " `py-copy-partial-expression'
Copy partial-expression at point.

Store data in kill ring, so it might yanked back."]

	   ["Copy statement" py-copy-statement
	    :help " `py-copy-statement'
Copy statement at point.

Store data in kill ring, so it might yanked back."]

	   ["Copy top level" py-copy-top-level
	    :help " `py-copy-top-level'
Copy top-level at point.

Store data in kill ring, so it might yanked back."]
           )
          ("Kill"
	   ["Kill block" py-kill-block
	    :help " `py-kill-block'
Delete `block' at point.

Stores data in kill ring"]

	   ["Kill block or clause" py-kill-block-or-clause
	    :help " `py-kill-block-or-clause'
Delete `block-or-clause' at point.

Stores data in kill ring"]

	   ["Kill class" py-kill-class
	    :help " `py-kill-class'
Delete `class' at point.

Stores data in kill ring"]

	   ["Kill clause" py-kill-clause
	    :help " `py-kill-clause'
Delete `clause' at point.

Stores data in kill ring"]

	   ["Kill comment" py-kill-comment
	    :help " `py-kill-comment'"]

	   ["Kill def" py-kill-def
	    :help " `py-kill-def'
Delete `def' at point.

Stores data in kill ring"]

	   ["Kill def or class" py-kill-def-or-class
	    :help " `py-kill-def-or-class'
Delete `def-or-class' at point.

Stores data in kill ring"]

	   ["Kill expression" py-kill-expression
	    :help " `py-kill-expression'
Delete `expression' at point.

Stores data in kill ring"]

	   ["Kill line" py-kill-line
	    :help " `py-kill-line'"]

	   ["Kill minor block" py-kill-minor-block
	    :help " `py-kill-minor-block'
Delete `minor-block' at point.

Stores data in kill ring"]

	   ["Kill paragraph" py-kill-paragraph
	    :help " `py-kill-paragraph'"]

	   ["Kill partial expression" py-kill-partial-expression
	    :help " `py-kill-partial-expression'
Delete `partial-expression' at point.

Stores data in kill ring"]

	   ["Kill statement" py-kill-statement
	    :help " `py-kill-statement'
Delete `statement' at point.

Stores data in kill ring"]

	   ["Kill top level" py-kill-top-level
	    :help " `py-kill-top-level'
Delete `top-level' at point.

Stores data in kill ring"]
           )
          ("Delete"
	   ["Delete block" py-delete-block
	    :help " `py-delete-block'
Delete BLOCK at point.

Don't store data in kill ring."]

	   ["Delete block or clause" py-delete-block-or-clause
	    :help " `py-delete-block-or-clause'
Delete BLOCK-OR-CLAUSE at point.

Don't store data in kill ring."]

	   ["Delete class" py-delete-class
	    :help " `py-delete-class'
Delete CLASS at point.

Don't store data in kill ring.
With C-u or `py-mark-decorators' set to `t', `decorators' are included."]

	   ["Delete clause" py-delete-clause
	    :help " `py-delete-clause'
Delete CLAUSE at point.

Don't store data in kill ring."]

	   ["Delete comment" py-delete-comment
	    :help " `py-delete-comment'"]

	   ["Delete def" py-delete-def
	    :help " `py-delete-def'
Delete DEF at point.

Don't store data in kill ring.
With C-u or `py-mark-decorators' set to `t', `decorators' are included."]

	   ["Delete def or class" py-delete-def-or-class
	    :help " `py-delete-def-or-class'
Delete DEF-OR-CLASS at point.

Don't store data in kill ring.
With C-u or `py-mark-decorators' set to `t', `decorators' are included."]

	   ["Delete expression" py-delete-expression
	    :help " `py-delete-expression'
Delete EXPRESSION at point.

Don't store data in kill ring."]

	   ["Delete line" py-delete-line
	    :help " `py-delete-line'"]

	   ["Delete minor block" py-delete-minor-block
	    :help " `py-delete-minor-block'
Delete MINOR-BLOCK at point.

Don't store data in kill ring."]

	   ["Delete paragraph" py-delete-paragraph
	    :help " `py-delete-paragraph'"]

	   ["Delete partial expression" py-delete-partial-expression
	    :help " `py-delete-partial-expression'
Delete PARTIAL-EXPRESSION at point.

Don't store data in kill ring."]

	   ["Delete statement" py-delete-statement
	    :help " `py-delete-statement'
Delete STATEMENT at point.

Don't store data in kill ring."]

	   ["Delete top level" py-delete-top-level
	    :help " `py-delete-top-level'
Delete TOP-LEVEL at point.

Don't store data in kill ring."]
           )
          ("Comment"
	   ["Comment block" py-comment-block
	    :help " `py-comment-block'
Comments block at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"]

	   ["Comment block or clause" py-comment-block-or-clause
	    :help " `py-comment-block-or-clause'
Comments block-or-clause at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"]

	   ["Comment class" py-comment-class
	    :help " `py-comment-class'
Comments class at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"]

	   ["Comment clause" py-comment-clause
	    :help " `py-comment-clause'
Comments clause at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"]

	   ["Comment def" py-comment-def
	    :help " `py-comment-def'
Comments def at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"]

	   ["Comment def or class" py-comment-def-or-class
	    :help " `py-comment-def-or-class'
Comments def-or-class at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"]

	   ["Comment statement" py-comment-statement
	    :help " `py-comment-statement'
Comments statement at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"]
           ))
         ("Move"
          ("Backward"
	   ["Beginning of block" py-beginning-of-block
	    :help " `py-beginning-of-block'
Go to beginning block, skip whitespace at BOL.

Returns beginning of block if successful, nil otherwise"]

	   ["Beginning of block or clause" py-beginning-of-block-or-clause
	    :help " `py-beginning-of-block-or-clause'
Go to beginning block-or-clause, skip whitespace at BOL.

Returns beginning of block-or-clause if successful, nil otherwise"]

	   ["Beginning of class" py-beginning-of-class
	    :help " `py-beginning-of-class'
Go to beginning class, skip whitespace at BOL.

Returns beginning of class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too."]

	   ["Beginning of clause" py-beginning-of-clause
	    :help " `py-beginning-of-clause'
Go to beginning clause, skip whitespace at BOL.

Returns beginning of clause if successful, nil otherwise"]

	   ["Beginning of def" py-beginning-of-def
	    :help " `py-beginning-of-def'
Go to beginning def, skip whitespace at BOL.

Returns beginning of def if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too."]

	   ["Beginning of def or class" py-beginning-of-def-or-class
	    :help " `py-beginning-of-def-or-class'
Go to beginning def-or-class, skip whitespace at BOL.

Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too."]

	   ["Beginning of elif block" py-beginning-of-elif-block
	    :help " `py-beginning-of-elif-block'
Go to beginning elif-block, skip whitespace at BOL.

Returns beginning of elif-block if successful, nil otherwise"]

	   ["Beginning of else block" py-beginning-of-else-block
	    :help " `py-beginning-of-else-block'
Go to beginning else-block, skip whitespace at BOL.

Returns beginning of else-block if successful, nil otherwise"]

	   ["Beginning of except block" py-beginning-of-except-block
	    :help " `py-beginning-of-except-block'
Go to beginning except-block, skip whitespace at BOL.

Returns beginning of except-block if successful, nil otherwise"]

	   ["Beginning of expression" py-beginning-of-expression
	    :help " `py-beginning-of-expression'
Go to the beginning of a compound python expression.

With numeric ARG do it that many times.

A a compound python expression might be concatenated by \".\" operator, thus composed by minor python expressions.

If already at the beginning or before a expression, go to next expression in buffer upwards

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes."]

	   ["Beginning of if block" py-beginning-of-if-block
	    :help " `py-beginning-of-if-block'
Go to beginning if-block, skip whitespace at BOL.

Returns beginning of if-block if successful, nil otherwise"]

	   ["Beginning of partial expression" py-beginning-of-partial-expression
	    :help " `py-beginning-of-partial-expression'"]

	   ["Beginning of statement" py-beginning-of-statement
	    :help " `py-beginning-of-statement'
Go to the initial line of a simple statement.

For beginning of compound statement use py-beginning-of-block.
For beginning of clause py-beginning-of-clause."]

	   ["Beginning of top level" py-beginning-of-top-level
	    :help " `py-beginning-of-top-level'
Go up to beginning of statments until level of indentation is null.

Returns position if successful, nil otherwise"]

	   ["Beginning of try block" py-beginning-of-try-block
	    :help " `py-beginning-of-try-block'
Go to beginning try-block, skip whitespace at BOL.

Returns beginning of try-block if successful, nil otherwise"]
           )
          ("Forward"
	   ["End of block" py-end-of-block
	    :help " `py-end-of-block'
Go to end of block.

Returns end of block if successful, nil otherwise"]

	   ["End of block or clause" py-end-of-block-or-clause
	    :help " `py-end-of-block-or-clause'
Go to end of block-or-clause.

Returns end of block-or-clause if successful, nil otherwise"]

	   ["End of class" py-end-of-class
	    :help " `py-end-of-class'
Go to end of class.

Returns end of class if successful, nil otherwise"]

	   ["End of clause" py-end-of-clause
	    :help " `py-end-of-clause'
Go to end of clause.

Returns end of clause if successful, nil otherwise"]

	   ["End of def" py-end-of-def
	    :help " `py-end-of-def'
Go to end of def.

Returns end of def if successful, nil otherwise"]

	   ["End of def or class" py-end-of-def-or-class
	    :help " `py-end-of-def-or-class'
Go to end of def-or-class.

Returns end of def-or-class if successful, nil otherwise"]

	   ["End of elif block" py-end-of-elif-block
	    :help " `py-end-of-elif-block'
Go to end of elif-block.

Returns end of elif-block if successful, nil otherwise"]

	   ["End of else block" py-end-of-else-block
	    :help " `py-end-of-else-block'
Go to end of else-block.

Returns end of else-block if successful, nil otherwise"]

	   ["End of except block" py-end-of-except-block
	    :help " `py-end-of-except-block'
Go to end of except-block.

Returns end of except-block if successful, nil otherwise"]

	   ["End of expression" py-end-of-expression
	    :help " `py-end-of-expression'
Go to the end of a compound python expression.

With numeric ARG do it that many times.

A a compound python expression might be concatenated by \".\" operator, thus composed by minor python expressions.

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference

Operators however are left aside resp. limit py-expression designed for edit-purposes."]

	   ["End of if block" py-end-of-if-block
	    :help " `py-end-of-if-block'
Go to end of if-block.

Returns end of if-block if successful, nil otherwise"]

	   ["End of partial expression" py-end-of-partial-expression
	    :help " `py-end-of-partial-expression'"]

	   ["End of statement" py-end-of-statement
	    :help " `py-end-of-statement'
Go to the last char of current statement.

Optional argument REPEAT, the number of loops done already, is checked for py-max-specpdl-size error. Avoid eternal loops due to missing string delimters etc."]

	   ["End of top level" py-end-of-top-level
	    :help " `py-end-of-top-level'
Go to end of top-level form at point.

Returns position if successful, nil otherwise"]

	   ["End of try block" py-end-of-try-block
	    :help " `py-end-of-try-block'
Go to end of try-block.

Returns end of try-block if successful, nil otherwise"]
           )
          ("BOL-forms"
           ("Backward"
	    ["Beginning of block bol" py-beginning-of-block-bol
	     :help " `py-beginning-of-block-bol'
Go to beginning block, go to BOL.

Returns beginning of block if successful, nil otherwise"]

	    ["Beginning of block or clause bol" py-beginning-of-block-or-clause-bol
	     :help " `py-beginning-of-block-or-clause-bol'
Go to beginning block-or-clause, go to BOL.

Returns beginning of block-or-clause if successful, nil otherwise"]

	    ["Beginning of class bol" py-beginning-of-class-bol
	     :help " `py-beginning-of-class-bol'
Go to beginning class, go to BOL.

Returns beginning of class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too."]

	    ["Beginning of clause bol" py-beginning-of-clause-bol
	     :help " `py-beginning-of-clause-bol'
Go to beginning clause, go to BOL.

Returns beginning of clause if successful, nil otherwise"]

	    ["Beginning of def bol" py-beginning-of-def-bol
	     :help " `py-beginning-of-def-bol'
Go to beginning def, go to BOL.

Returns beginning of def if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too."]

	    ["Beginning of def or class bol" py-beginning-of-def-or-class-bol
	     :help " `py-beginning-of-def-or-class-bol'
Go to beginning def-or-class, go to BOL.

Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too."]

	    ["Beginning of elif block bol" py-beginning-of-elif-block-bol
	     :help " `py-beginning-of-elif-block-bol'
Go to beginning elif-block, go to BOL.

Returns beginning of elif-block if successful, nil otherwise"]

	    ["Beginning of else block bol" py-beginning-of-else-block-bol
	     :help " `py-beginning-of-else-block-bol'
Go to beginning else-block, go to BOL.

Returns beginning of else-block if successful, nil otherwise"]

	    ["Beginning of except block bol" py-beginning-of-except-block-bol
	     :help " `py-beginning-of-except-block-bol'
Go to beginning except-block, go to BOL.

Returns beginning of except-block if successful, nil otherwise"]

	    ["Beginning of expression bol" py-beginning-of-expression-bol
	     :help " `py-beginning-of-expression-bol'"]

	    ["Beginning of if block bol" py-beginning-of-if-block-bol
	     :help " `py-beginning-of-if-block-bol'
Go to beginning if-block, go to BOL.

Returns beginning of if-block if successful, nil otherwise"]

	    ["Beginning of partial expression bol" py-beginning-of-partial-expression-bol
	     :help " `py-beginning-of-partial-expression-bol'"]

	    ["Beginning of statement bol" py-beginning-of-statement-bol
	     :help " `py-beginning-of-statement-bol'
Goto beginning of line where statement starts.
  Returns position reached, if successful, nil otherwise.

See also `py-up-statement': up from current definition to next beginning of statement above."]

	    ["Beginning of try block bol" py-beginning-of-try-block-bol
	     :help " `py-beginning-of-try-block-bol'
Go to beginning try-block, go to BOL.

Returns beginning of try-block if successful, nil otherwise"]
            )
           ("Forward"
	    ["End of block bol" py-end-of-block-bol
	     :help " `py-end-of-block-bol'
Goto beginning of line following end of block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-block': down from current definition to next beginning of block below."]

	    ["End of block or clause bol" py-end-of-block-or-clause-bol
	     :help " `py-end-of-block-or-clause-bol'
Goto beginning of line following end of block-or-clause.
  Returns position reached, if successful, nil otherwise.

See also `py-down-block-or-clause': down from current definition to next beginning of block-or-clause below."]

	    ["End of class bol" py-end-of-class-bol
	     :help " `py-end-of-class-bol'
Goto beginning of line following end of class.
  Returns position reached, if successful, nil otherwise.

See also `py-down-class': down from current definition to next beginning of class below."]

	    ["End of clause bol" py-end-of-clause-bol
	     :help " `py-end-of-clause-bol'
Goto beginning of line following end of clause.
  Returns position reached, if successful, nil otherwise.

See also `py-down-clause': down from current definition to next beginning of clause below."]

	    ["End of def bol" py-end-of-def-bol
	     :help " `py-end-of-def-bol'
Goto beginning of line following end of def.
  Returns position reached, if successful, nil otherwise.

See also `py-down-def': down from current definition to next beginning of def below."]

	    ["End of def or class bol" py-end-of-def-or-class-bol
	     :help " `py-end-of-def-or-class-bol'
Goto beginning of line following end of def-or-class.
  Returns position reached, if successful, nil otherwise.

See also `py-down-def-or-class': down from current definition to next beginning of def-or-class below."]

	    ["End of elif block bol" py-end-of-elif-block-bol
	     :help " `py-end-of-elif-block-bol'
Goto beginning of line following end of elif-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-elif-block': down from current definition to next beginning of elif-block below."]

	    ["End of else block bol" py-end-of-else-block-bol
	     :help " `py-end-of-else-block-bol'
Goto beginning of line following end of else-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-else-block': down from current definition to next beginning of else-block below."]

	    ["End of except block bol" py-end-of-except-block-bol
	     :help " `py-end-of-except-block-bol'
Goto beginning of line following end of except-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-except-block': down from current definition to next beginning of except-block below."]

	    ["End of expression bol" py-end-of-expression-bol
	     :help " `py-end-of-expression-bol'"]

	    ["End of if block bol" py-end-of-if-block-bol
	     :help " `py-end-of-if-block-bol'
Goto beginning of line following end of if-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-if-block': down from current definition to next beginning of if-block below."]

	    ["End of partial expression bol" py-end-of-partial-expression-bol
	     :help " `py-end-of-partial-expression-bol'"]

	    ["End of statement bol" py-end-of-statement-bol
	     :help " `py-end-of-statement-bol'
Go to the beginning-of-line following current statement."]

	    ["End of top level bol" py-end-of-top-level-bol
	     :help " `py-end-of-top-level-bol'
Go to end of top-level form at point, stop at next beginning-of-line.

Returns position successful, nil otherwise"]

	    ["End of try block bol" py-end-of-try-block-bol
	     :help " `py-end-of-try-block-bol'
Goto beginning of line following end of try-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-try-block': down from current definition to next beginning of try-block below."]
            ))
          ("Up/Down"
	   ["Up" py-up
	    :help " `py-up'
Go up or to beginning of form if inside.

If inside a delimited form --string or list-- go to its beginning.
If not at beginning of a statement or block, go to its beginning.
If at beginning of a statement or block, go to beginning one level above of compound statement or definition at point."]

	   ["Down" py-down
	    :help " `py-down'
Go to beginning one level below of compound statement or definition at point.

If no statement or block below, but a delimited form --string or list-- go to its beginning. Repeated call from there will behave like down-list.

Returns position if successful, nil otherwise"]
           ))
         ("Hide-Show"
          ("Hide"
	   ["Hide region" py-hide-region
	    :help " `py-hide-region'
Hide active region."]

	   ["Hide statement" py-hide-statement
	    :help " `py-hide-statement'
Hide statement at point."]

	   ["Hide block" py-hide-block
	    :help " `py-hide-block'
Hide block at point."]

	   ["Hide clause" py-hide-clause
	    :help " `py-hide-clause'
Hide clause at point."]

	   ["Hide block or clause" py-hide-block-or-clause
	    :help " `py-hide-block-or-clause'
Hide block-or-clause at point."]

	   ["Hide def" py-hide-def
	    :help " `py-hide-def'
Hide def at point."]

	   ["Hide class" py-hide-class
	    :help " `py-hide-class'
Hide class at point."]

	   ["Hide expression" py-hide-expression
	    :help " `py-hide-expression'
Hide expression at point."]

	   ["Hide partial expression" py-hide-partial-expression
	    :help " `py-hide-partial-expression'
Hide partial-expression at point."]

	   ["Hide line" py-hide-line
	    :help " `py-hide-line'
Hide line at point."]

	   ["Hide top level" py-hide-top-level
	    :help " `py-hide-top-level'
Hide top-level at point."]
           )
          ("Show"
	   ["Show region" py-show-region
	    :help " `py-show-region'
Un-hide active region."]

	   ["Show statement" py-show-statement
	    :help " `py-show-statement'
Show statement at point."]

	   ["Show block" py-show-block
	    :help " `py-show-block'
Show block at point."]

	   ["Show clause" py-show-clause
	    :help " `py-show-clause'
Show clause at point."]

	   ["Show block or clause" py-show-block-or-clause
	    :help " `py-show-block-or-clause'
Show block-or-clause at point."]

	   ["Show def" py-show-def
	    :help " `py-show-def'
Show def at point."]

	   ["Show class" py-show-class
	    :help " `py-show-class'
Show class at point."]

	   ["Show expression" py-show-expression
	    :help " `py-show-expression'
Show expression at point."]

	   ["Show partial expression" py-show-partial-expression
	    :help " `py-show-partial-expression'
Show partial-expression at point."]

	   ["Show line" py-show-line
	    :help " `py-show-line'
Show line at point."]

	   ["Show top level" py-show-top-level
	    :help " `py-show-top-level'
Show top-level at point."]
           ))
         ("Virtualenv"
          ["Virtualenv activate" virtualenv-activate
	   :help " `virtualenv-activate'
Activate the virtualenv located in DIR"]

          ["Virtualenv deactivate" virtualenv-deactivate
	   :help " `virtualenv-deactivate'
Deactivate the current virtual enviroment"]

          ["Virtualenv p" virtualenv-p
	   :help " `virtualenv-p'
Check if a directory is a virtualenv"]

          ["Virtualenv workon" virtualenv-workon
	   :help " `virtualenv-workon'
Issue a virtualenvwrapper-like virtualenv-workon command"]
          )
         ("Help"
          ["Find definition" py-find-definition
	   :help " `py-find-definition'
Find source of definition of SYMBOL.

Interactively, prompt for SYMBOL."]

          ["Help at point" py-help-at-point
	   :help " `py-help-at-point'
Print help on symbol at point.

If symbol is defined in current buffer, jump to it's definition
Optional C-u used for debugging, will prevent deletion of temp file."]

          ["Info lookup symbol" py-info-lookup-symbol
	   :help " `py-info-lookup-symbol'"]

          ["Symbol at point" py-symbol-at-point
	   :help " `py-symbol-at-point'
Return the current Python symbol."]
          )
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

	     ["Set Pymacs-based complete keymap "
	      (setq py-set-complete-keymap-p
		    (not py-set-complete-keymap-p))
	      :help "If `py-complete-initialize', which sets up enviroment for Pymacs based py-complete, should load it's keys into `python-mode-map'

Default is nil.
See also resp. edit `py-complete-set-keymap' Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-set-complete-keymap-p]

	     ["Indent no completion "
	      (setq py-indent-no-completion-p
		    (not py-indent-no-completion-p))
	      :help "If completion function should indent when no completion found. Default is `t'

Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-indent-no-completion-p]

	     ["Company pycomplete "
	      (setq py-company-pycomplete-p
		    (not py-company-pycomplete-p))
	      :help "Load company-pycomplete stuff. Default is nilUse `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-company-pycomplete-p])

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
	     (setq py-split-window-on-execute
		   (not py-split-window-on-execute))
	     :help "When non-nil split windows.

Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-split-window-on-execute]

	    ["Keep windows configuration"
	     (setq py-keep-windows-configuration
		   (not py-keep-windows-configuration))
	     :help "If a windows is splitted displaying results, this is directed by variable `py-split-window-on-execute'\. Also setting `py-switch-buffers-on-execute-p' affects window-configuration\. While commonly a screen splitted into source and Python-shell buffer is assumed, user may want to keep a different config\.

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
	     (setq py-empty-comment-line-separates-paragraph-p
		   (not py-empty-comment-line-separates-paragraph-p))
	     :help "Consider paragraph start/end lines with nothing inside but comment sign.

Default is non-nilUse `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-empty-comment-line-separates-paragraph-p]

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
         ("Other"
          ["Boolswitch" py-boolswitch
	   :help " `py-boolswitch'
Edit the assignment of a boolean variable, revert them.

I.e. switch it from \"True\" to \"False\" and vice versa"]

          ["Empty out list backward" py-empty-out-list-backward
	   :help " `py-empty-out-list-backward'
Deletes all elements from list before point."]

          ["Kill buffer unconditional" py-kill-buffer-unconditional
	   :help " `py-kill-buffer-unconditional'
Kill buffer unconditional, kill buffer-process if existing."]

          ["Remove overlays at point" py-remove-overlays-at-point
	   :help " `py-remove-overlays-at-point'
Remove overlays as set when `py-highlight-error-source-p' is non-nil."]
          ("Electric"
	   ["Complete electric comma" py-complete-electric-comma
	    :help " `py-complete-electric-comma'"]

	   ["Complete electric lparen" py-complete-electric-lparen
	    :help " `py-complete-electric-lparen'"]

	   ["Electric backspace" py-electric-backspace
	    :help " `py-electric-backspace'
Delete preceding character or level of indentation.

With ARG do that ARG times.
Returns column reached."]

	   ["Electric colon" py-electric-colon
	    :help " `py-electric-colon'
Insert a colon and indent accordingly.

If a numeric argument ARG is provided, that many colons are inserted
non-electrically.

Electric behavior is inhibited inside a string or
comment or by universal prefix C-u.

Switched by `py-electric-colon-active-p', default is nil
See also `py-electric-colon-greedy-p'"]

	   ["Electric comment" py-electric-comment
	    :help " `py-electric-comment'
Insert a comment. If starting a comment, indent accordingly.

If a numeric argument ARG is provided, that many \"#\" are inserted
non-electrically.
With C-u \"#\" electric behavior is inhibited inside a string or comment."]

	   ["Electric delete" py-electric-delete
	    :help " `py-electric-delete'
Delete following character or levels of whitespace.

With ARG do that ARG times."]

	   ["Electric yank" py-electric-yank
	    :help " `py-electric-yank'
Perform command `yank' followed by an `indent-according-to-mode'"]

	   ["Hungry delete backwards" py-hungry-delete-backwards
	    :help " `py-hungry-delete-backwards'
Delete the preceding character or all preceding whitespace
back to the previous non-whitespace character.
See also C-c <delete>."]

	   ["Hungry delete forward" py-hungry-delete-forward
	    :help " `py-hungry-delete-forward'
Delete the following character or all following whitespace
up to the next non-whitespace character.
See also C-c <C-backspace>."]
            )
          ("Abbrevs"	   :help "see also `py-add-abbrev'"
	   :filter (lambda (&rest junk)
		     (abbrev-table-menu python-mode-abbrev-table))            )

          ["Add abbrev" py-add-abbrev
	   :help " `py-add-abbrev'
Defines python-mode specific abbrev for last expressions before point.
Argument is how many `py-partial-expression's form the expansion; or zero means the region is the expansion.

Reads the abbreviation in the minibuffer; with numeric arg it displays a proposal for an abbrev.
Proposal is composed from the initial character(s) of the
expansion.

Don't use this function in a Lisp program; use `define-abbrev' instead."]
          ("Completion"
	   ["Py indent or complete" py-py-indent-or-complete
	    :help " `py-py-indent-or-complete'"]

	   ["Py shell complete" py-py-shell-complete
	    :help " `py-py-shell-complete'"]

	   ["Py complete" py-py-complete
	    :help " `py-py-complete'"]
            )))))

(provide 'python-components-shell-menu)
;;; python-components-shell-menu.el ends here
