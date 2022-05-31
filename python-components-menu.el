;; python-components-menu.el --- Provide the python-mode menu -*- lexical-binding: t; -*-

;; This file not shipped as part of GNU Emacs.

;; URL: https://gitlab.com/python-mode-devs

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
(defun py-define-menu (map)
  (easy-menu-define py-menu map "Py"
    `("Python"
      ("Interpreter"
       ["Ipython" ipython
	:help " `ipython'
Start an IPython interpreter."]

       ["Ipython2\.7" ipython2\.7
	:help " `ipython2\.7'"]

       ["Ipython3" ipython3
	:help " `ipython3'
Start an IPython3 interpreter."]

       ["Jython" jython
	:help " `jython'
Start an Jython interpreter."]

       ["Python" python
	:help " `python'
Start an Python interpreter."]

       ["Python2" python2
	:help " `python2'
Start an Python2 interpreter."]

       ["Python3" python3
	:help " `python3'
Start an Python3 interpreter."]
       ["SymPy" isympy3
	:help " `isympy3'
Start an SymPy interpreter."])

      ("Edit"
       ("Shift"
	("Shift right"
	 ["Shift block right" py-shift-block-right
	  :help " `py-shift-block-right'
Indent block by COUNT spaces."]

	 ["Shift block or clause right" py-shift-block-or-clause-right
	  :help " `py-shift-block-or-clause-right'
Indent block-or-clause by COUNT spaces."]

	 ["Shift class right" py-shift-class-right
	  :help " `py-shift-class-right'
Indent class by COUNT spaces."]

	 ["Shift clause right" py-shift-clause-right
	  :help " `py-shift-clause-right'
Indent clause by COUNT spaces."]

	 ["Shift comment right" py-shift-comment-right
	  :help " `py-shift-comment-right'
Indent comment by COUNT spaces."]

	 ["Shift def right" py-shift-def-right
	  :help " `py-shift-def-right'
Indent def by COUNT spaces."]

	 ["Shift def or class right" py-shift-def-or-class-right
	  :help " `py-shift-def-or-class-right'
Indent def-or-class by COUNT spaces."]

	 ["Shift indent right" py-shift-indent-right
	  :help " `py-shift-indent-right'
Indent indent by COUNT spaces."]

	 ["Shift minor block right" py-shift-minor-block-right
	  :help " `py-shift-minor-block-right'
Indent minor-block by COUNT spaces."]

	 ["Shift paragraph right" py-shift-paragraph-right
	  :help " `py-shift-paragraph-right'
Indent paragraph by COUNT spaces."]

	 ["Shift region right" py-shift-region-right
	  :help " `py-shift-region-right'
Indent region by COUNT spaces."]

	 ["Shift statement right" py-shift-statement-right
	  :help " `py-shift-statement-right'
Indent statement by COUNT spaces."]

	 ["Shift top level right" py-shift-top-level-right
	  :help " `py-shift-top-level-right'
Indent top-level by COUNT spaces."])
	("Shift left"
	 ["Shift block left" py-shift-block-left
	  :help " `py-shift-block-left'
Dedent block by COUNT spaces."]

	 ["Shift block or clause left" py-shift-block-or-clause-left
	  :help " `py-shift-block-or-clause-left'
Dedent block-or-clause by COUNT spaces."]

	 ["Shift class left" py-shift-class-left
	  :help " `py-shift-class-left'
Dedent class by COUNT spaces."]

	 ["Shift clause left" py-shift-clause-left
	  :help " `py-shift-clause-left'
Dedent clause by COUNT spaces."]

	 ["Shift comment left" py-shift-comment-left
	  :help " `py-shift-comment-left'
Dedent comment by COUNT spaces."]

	 ["Shift def left" py-shift-def-left
	  :help " `py-shift-def-left'
Dedent def by COUNT spaces."]

	 ["Shift def or class left" py-shift-def-or-class-left
	  :help " `py-shift-def-or-class-left'
Dedent def-or-class by COUNT spaces."]

	 ["Shift indent left" py-shift-indent-left
	  :help " `py-shift-indent-left'
Dedent indent by COUNT spaces."]

	 ["Shift minor block left" py-shift-minor-block-left
	  :help " `py-shift-minor-block-left'
Dedent minor-block by COUNT spaces."]

	 ["Shift paragraph left" py-shift-paragraph-left
	  :help " `py-shift-paragraph-left'
Dedent paragraph by COUNT spaces."]

	 ["Shift region left" py-shift-region-left
	  :help " `py-shift-region-left'
Dedent region by COUNT spaces."]

	 ["Shift statement left" py-shift-statement-left
	  :help " `py-shift-statement-left'
Dedent statement by COUNT spaces."]))
       ("Mark"
	["Mark block" py-mark-block
	 :help " `py-mark-block'
Mark block, take beginning of line positions."]

	["Mark block or clause" py-mark-block-or-clause
	 :help " `py-mark-block-or-clause'
Mark block-or-clause, take beginning of line positions."]

	["Mark class" py-mark-class
	 :help " `py-mark-class'
Mark class, take beginning of line positions."]

	["Mark clause" py-mark-clause
	 :help " `py-mark-clause'
Mark clause, take beginning of line positions."]

	["Mark comment" py-mark-comment
	 :help " `py-mark-comment'
Mark comment at point."]

	["Mark def" py-mark-def
	 :help " `py-mark-def'
Mark def, take beginning of line positions."]

	["Mark def or class" py-mark-def-or-class
	 :help " `py-mark-def-or-class'
Mark def-or-class, take beginning of line positions."]

	["Mark expression" py-mark-expression
	 :help " `py-mark-expression'
Mark expression at point."]

	["Mark except block" py-mark-except-block
	 :help " `py-mark-except-block'
Mark except-block, take beginning of line positions."]

	["Mark if block" py-mark-if-block
	 :help " `py-mark-if-block'
Mark if-block, take beginning of line positions."]

	["Mark indent" py-mark-indent
	 :help " `py-mark-indent'
Mark indent, take beginning of line positions."]

	["Mark line" py-mark-line
	 :help " `py-mark-line'
Mark line at point."]

	["Mark minor block" py-mark-minor-block
	 :help " `py-mark-minor-block'
Mark minor-block, take beginning of line positions."]

	["Mark partial expression" py-mark-partial-expression
	 :help " `py-mark-partial-expression'
Mark partial-expression at point."]

	["Mark paragraph" py-mark-paragraph
	 :help " `py-mark-paragraph'
Mark paragraph at point."]

	["Mark section" py-mark-section
	 :help " `py-mark-section'
Mark section at point."]

	["Mark statement" py-mark-statement
	 :help " `py-mark-statement'
Mark statement, take beginning of line positions."]

	["Mark top level" py-mark-top-level
	 :help " `py-mark-top-level'
Mark top-level, take beginning of line positions."]

	["Mark try block" py-mark-try-block
	 :help " `py-mark-try-block'
Mark try-block, take beginning of line positions."])
       ("Copy"
	["Copy block" py-copy-block
	 :help " `py-copy-block'
Copy block at point."]

	["Copy block or clause" py-copy-block-or-clause
	 :help " `py-copy-block-or-clause'
Copy block-or-clause at point."]

	["Copy class" py-copy-class
	 :help " `py-copy-class'
Copy class at point."]

	["Copy clause" py-copy-clause
	 :help " `py-copy-clause'
Copy clause at point."]

	["Copy comment" py-copy-comment
	 :help " `py-copy-comment'"]

	["Copy def" py-copy-def
	 :help " `py-copy-def'
Copy def at point."]

	["Copy def or class" py-copy-def-or-class
	 :help " `py-copy-def-or-class'
Copy def-or-class at point."]

	["Copy expression" py-copy-expression
	 :help " `py-copy-expression'
Copy expression at point."]

	["Copy except block" py-copy-except-block
	 :help " `py-copy-except-block'"]

	["Copy if block" py-copy-if-block
	 :help " `py-copy-if-block'"]

	["Copy indent" py-copy-indent
	 :help " `py-copy-indent'
Copy indent at point."]

	["Copy line" py-copy-line
	 :help " `py-copy-line'
Copy line at point."]

	["Copy minor block" py-copy-minor-block
	 :help " `py-copy-minor-block'
Copy minor-block at point."]

	["Copy partial expression" py-copy-partial-expression
	 :help " `py-copy-partial-expression'
Copy partial-expression at point."]

	["Copy paragraph" py-copy-paragraph
	 :help " `py-copy-paragraph'
Copy paragraph at point."]

	["Copy section" py-copy-section
	 :help " `py-copy-section'"]

	["Copy statement" py-copy-statement
	 :help " `py-copy-statement'
Copy statement at point."]

	["Copy top level" py-copy-top-level
	 :help " `py-copy-top-level'
Copy top-level at point."])
       ("Kill"
	["Kill block" py-kill-block
	 :help " `py-kill-block'
Delete block at point."]

	["Kill block or clause" py-kill-block-or-clause
	 :help " `py-kill-block-or-clause'
Delete block-or-clause at point."]

	["Kill class" py-kill-class
	 :help " `py-kill-class'
Delete class at point."]

	["Kill clause" py-kill-clause
	 :help " `py-kill-clause'
Delete clause at point."]

	["Kill comment" py-kill-comment
	 :help " `py-kill-comment'
Delete comment at point."]

	["Kill def" py-kill-def
	 :help " `py-kill-def'
Delete def at point."]

	["Kill def or class" py-kill-def-or-class
	 :help " `py-kill-def-or-class'
Delete def-or-class at point."]

	["Kill expression" py-kill-expression
	 :help " `py-kill-expression'
Delete expression at point."]

	["Kill except block" py-kill-except-block
	 :help " `py-kill-except-block'
Delete except-block at point."]

	["Kill if block" py-kill-if-block
	 :help " `py-kill-if-block'
Delete if-block at point."]

	["Kill indent" py-kill-indent
	 :help " `py-kill-indent'
Delete indent at point."]

	["Kill line" py-kill-line
	 :help " `py-kill-line'
Delete line at point."]

	["Kill minor block" py-kill-minor-block
	 :help " `py-kill-minor-block'
Delete minor-block at point."]

	["Kill partial expression" py-kill-partial-expression
	 :help " `py-kill-partial-expression'
Delete partial-expression at point."]

	["Kill paragraph" py-kill-paragraph
	 :help " `py-kill-paragraph'
Delete paragraph at point."]

	["Kill section" py-kill-section
	 :help " `py-kill-section'
Delete section at point."]

	["Kill statement" py-kill-statement
	 :help " `py-kill-statement'
Delete statement at point."]

	["Kill top level" py-kill-top-level
	 :help " `py-kill-top-level'
Delete top-level at point."]

	["Kill try block" py-kill-try-block
	 :help " `py-kill-try-block'
Delete try-block at point."])
       ("Delete"
	["Delete block" py-delete-block
	 :help " `py-delete-block'
Delete BLOCK at point until beginning-of-line."]

	["Delete block or clause" py-delete-block-or-clause
	 :help " `py-delete-block-or-clause'
Delete BLOCK-OR-CLAUSE at point until beginning-of-line."]

	["Delete class" py-delete-class
	 :help " `py-delete-class'
Delete CLASS at point until beginning-of-line."]

	["Delete clause" py-delete-clause
	 :help " `py-delete-clause'
Delete CLAUSE at point until beginning-of-line."]

	["Delete comment" py-delete-comment
	 :help " `py-delete-comment'
Delete COMMENT at point."]

	["Delete def" py-delete-def
	 :help " `py-delete-def'
Delete DEF at point until beginning-of-line."]

	["Delete def or class" py-delete-def-or-class
	 :help " `py-delete-def-or-class'
Delete DEF-OR-CLASS at point until beginning-of-line."]

	["Delete expression" py-delete-expression
	 :help " `py-delete-expression'
Delete EXPRESSION at point."]

	["Delete except block" py-delete-except-block
	 :help " `py-delete-except-block'
Delete EXCEPT-BLOCK at point until beginning-of-line."]

	["Delete if block" py-delete-if-block
	 :help " `py-delete-if-block'
Delete IF-BLOCK at point until beginning-of-line."]

	["Delete indent" py-delete-indent
	 :help " `py-delete-indent'
Delete INDENT at point until beginning-of-line."]

	["Delete line" py-delete-line
	 :help " `py-delete-line'
Delete LINE at point."]

	["Delete minor block" py-delete-minor-block
	 :help " `py-delete-minor-block'
Delete MINOR-BLOCK at point until beginning-of-line."]

	["Delete partial expression" py-delete-partial-expression
	 :help " `py-delete-partial-expression'
Delete PARTIAL-EXPRESSION at point."]

	["Delete paragraph" py-delete-paragraph
	 :help " `py-delete-paragraph'
Delete PARAGRAPH at point."]

	["Delete section" py-delete-section
	 :help " `py-delete-section'
Delete SECTION at point."]

	["Delete statement" py-delete-statement
	 :help " `py-delete-statement'
Delete STATEMENT at point until beginning-of-line."]

	["Delete top level" py-delete-top-level
	 :help " `py-delete-top-level'
Delete TOP-LEVEL at point."]

	["Delete try block" py-delete-try-block
	 :help " `py-delete-try-block'
Delete TRY-BLOCK at point until beginning-of-line."])
       ("Comment"
	["Comment block" py-comment-block
	 :help " `py-comment-block'
Comments block at point."]

	["Comment block or clause" py-comment-block-or-clause
	 :help " `py-comment-block-or-clause'
Comments block-or-clause at point."]

	["Comment class" py-comment-class
	 :help " `py-comment-class'
Comments class at point."]

	["Comment clause" py-comment-clause
	 :help " `py-comment-clause'
Comments clause at point."]

	["Comment def" py-comment-def
	 :help " `py-comment-def'
Comments def at point."]

	["Comment def or class" py-comment-def-or-class
	 :help " `py-comment-def-or-class'
Comments def-or-class at point."]

	["Comment indent" py-comment-indent
	 :help " `py-comment-indent'
Comments indent at point."]

	["Comment minor block" py-comment-minor-block
	 :help " `py-comment-minor-block'
Comments minor-block at point."]

	["Comment section" py-comment-section
	 :help " `py-comment-section'
Comments section at point."]

	["Comment statement" py-comment-statement
	 :help " `py-comment-statement'
Comments statement at point."]

	["Comment top level" py-comment-top-level
	 :help " `py-comment-top-level'
Comments top-level at point."]))
      ("Move"
       ("Backward"

	["Backward def or class" py-backward-def-or-class
	 :help " `py-backward-def-or-class'
Go to beginning of def-or-class."]

	["Backward class" py-backward-class
	 :help " `py-backward-class'
Go to beginning of class."]

	["Backward def" py-backward-def
	 :help " `py-backward-def'
Go to beginning of def."]

	["Backward block" py-backward-block
	 :help " `py-backward-block'
Go to beginning of `block'."]

	["Backward statement" py-backward-statement
	 :help " `py-backward-statement'
Go to the initial line of a simple statement."]

	["Backward indent" py-backward-indent
	 :help " `py-backward-indent'
Go to the beginning of a section of equal indent."]

	["Backward top level" py-backward-top-level
	 :help " `py-backward-top-level'
Go up to beginning of statments until level of indentation is null."]

	("Other"
	 ["Backward section" py-backward-section
	  :help " `py-backward-section'
Go to next section start upward in buffer."]

	 ["Backward expression" py-backward-expression
	  :help " `py-backward-expression'"]

	 ["Backward partial expression" py-backward-partial-expression
	  :help " `py-backward-partial-expression'"]

	 ["Backward assignment" py-backward-assignment
	  :help " `py-backward-assignment'"]

	 ["Backward block or clause" py-backward-block-or-clause
	  :help " `py-backward-block-or-clause'
Go to beginning of `block-or-clause'."]

	 ["Backward clause" py-backward-clause
	  :help " `py-backward-clause'
Go to beginning of `clause'."]

	 ["Backward elif block" py-backward-elif-block
	  :help " `py-backward-elif-block'
Go to beginning of `elif-block'."]

	 ["Backward else block" py-backward-else-block
	  :help " `py-backward-else-block'
Go to beginning of `else-block'."]

	 ["Backward except block" py-backward-except-block
	  :help " `py-backward-except-block'
Go to beginning of `except-block'."]

	 ["Backward if block" py-backward-if-block
	  :help " `py-backward-if-block'
Go to beginning of `if-block'."]

	 ["Backward minor block" py-backward-minor-block
	  :help " `py-backward-minor-block'
Go to beginning of `minor-block'."]

	 ["Backward try block" py-backward-try-block
	  :help " `py-backward-try-block'
Go to beginning of `try-block'."]))
       ("Forward"
	["Forward def or class" py-forward-def-or-class
	 :help " `py-forward-def-or-class'
Go to end of def-or-class."]

	["Forward class" py-forward-class
	 :help " `py-forward-class'
Go to end of class."]

	["Forward def" py-forward-def
	 :help " `py-forward-def'
Go to end of def."]

	["Forward block" py-forward-block
	 :help " `py-forward-block'
Go to end of block."]

	["Forward statement" py-forward-statement
	 :help " `py-forward-statement'
Go to the last char of current statement."]

	["Forward indent" py-forward-indent
	 :help " `py-forward-indent'
Go to the end of a section of equal indentation."]

	["Forward top level" py-forward-top-level
	 :help " `py-forward-top-level'
Go to end of top-level form at point."]

	("Other"
	 ["Forward section" py-forward-section
	  :help " `py-forward-section'
Go to next section end downward in buffer."]

	 ["Forward expression" py-forward-expression
	  :help " `py-forward-expression'"]

	 ["Forward partial expression" py-forward-partial-expression
	  :help " `py-forward-partial-expression'"]

	 ["Forward assignment" py-forward-assignment
	  :help " `py-forward-assignment'"]

	 ["Forward block or clause" py-forward-block-or-clause
	  :help " `py-forward-block-or-clause'
Go to end of block-or-clause."]

	 ["Forward clause" py-forward-clause
	  :help " `py-forward-clause'
Go to end of clause."]

	 ["Forward for block" py-forward-for-block
	 :help " `py-forward-for-block'
Go to end of for-block."]

	 ["Forward elif block" py-forward-elif-block
	  :help " `py-forward-elif-block'
Go to end of elif-block."]

	 ["Forward else block" py-forward-else-block
	  :help " `py-forward-else-block'
Go to end of else-block."]

	 ["Forward except block" py-forward-except-block
	  :help " `py-forward-except-block'
Go to end of except-block."]

	 ["Forward if block" py-forward-if-block
	  :help " `py-forward-if-block'
Go to end of if-block."]

	 ["Forward minor block" py-forward-minor-block
	  :help " `py-forward-minor-block'
Go to end of minor-block."]
	 ["Forward try block" py-forward-try-block
	  :help " `py-forward-try-block'
Go to end of try-block."]))
       ("BOL-forms"
	("Backward"
	 ["Backward block bol" py-backward-block-bol
	  :help " `py-backward-block-bol'
Go to beginning of `block', go to BOL."]

	 ["Backward block or clause bol" py-backward-block-or-clause-bol
	  :help " `py-backward-block-or-clause-bol'
Go to beginning of `block-or-clause', go to BOL."]

	 ["Backward class bol" py-backward-class-bol
	  :help " `py-backward-class-bol'
Go to beginning of class, go to BOL."]

	 ["Backward clause bol" py-backward-clause-bol
	  :help " `py-backward-clause-bol'
Go to beginning of `clause', go to BOL."]

	 ["Backward def bol" py-backward-def-bol
	  :help " `py-backward-def-bol'
Go to beginning of def, go to BOL."]

	 ["Backward def or class bol" py-backward-def-or-class-bol
	  :help " `py-backward-def-or-class-bol'
Go to beginning of def-or-class, go to BOL."]

	 ["Backward elif block bol" py-backward-elif-block-bol
	  :help " `py-backward-elif-block-bol'
Go to beginning of `elif-block', go to BOL."]

	 ["Backward else block bol" py-backward-else-block-bol
	  :help " `py-backward-else-block-bol'
Go to beginning of `else-block', go to BOL."]

	 ["Backward except block bol" py-backward-except-block-bol
	  :help " `py-backward-except-block-bol'
Go to beginning of `except-block', go to BOL."]

	 ["Backward expression bol" py-backward-expression-bol
	  :help " `py-backward-expression-bol'"]

	 ["Backward for block bol" py-backward-for-block-bol
	  :help " `py-backward-for-block-bol'
Go to beginning of `for-block', go to BOL."]

	 ["Backward if block bol" py-backward-if-block-bol
	  :help " `py-backward-if-block-bol'
Go to beginning of `if-block', go to BOL."]

	 ["Backward indent bol" py-backward-indent-bol
	  :help " `py-backward-indent-bol'
Go to the beginning of line of a section of equal indent."]

	 ["Backward minor block bol" py-backward-minor-block-bol
	  :help " `py-backward-minor-block-bol'
Go to beginning of `minor-block', go to BOL."]

	 ["Backward partial expression bol" py-backward-partial-expression-bol
	  :help " `py-backward-partial-expression-bol'"]

	 ["Backward section bol" py-backward-section-bol
	  :help " `py-backward-section-bol'"]

	 ["Backward statement bol" py-backward-statement-bol
	  :help " `py-backward-statement-bol'
Goto beginning of line where statement starts."]

	 ["Backward try block bol" py-backward-try-block-bol
	  :help " `py-backward-try-block-bol'
Go to beginning of `try-block', go to BOL."])
	("Forward"
	 ["Forward block bol" py-forward-block-bol
	  :help " `py-forward-block-bol'
Goto beginning of line following end of block."]

	 ["Forward block or clause bol" py-forward-block-or-clause-bol
	  :help " `py-forward-block-or-clause-bol'
Goto beginning of line following end of block-or-clause."]

	 ["Forward class bol" py-forward-class-bol
	  :help " `py-forward-class-bol'
Goto beginning of line following end of class."]

	 ["Forward clause bol" py-forward-clause-bol
	  :help " `py-forward-clause-bol'
Goto beginning of line following end of clause."]

	 ["Forward def bol" py-forward-def-bol
	  :help " `py-forward-def-bol'
Goto beginning of line following end of def."]

	 ["Forward def or class bol" py-forward-def-or-class-bol
	  :help " `py-forward-def-or-class-bol'
Goto beginning of line following end of def-or-class."]

	 ["Forward elif block bol" py-forward-elif-block-bol
	  :help " `py-forward-elif-block-bol'
Goto beginning of line following end of elif-block."]

	 ["Forward else block bol" py-forward-else-block-bol
	  :help " `py-forward-else-block-bol'
Goto beginning of line following end of else-block."]

	 ["Forward except block bol" py-forward-except-block-bol
	  :help " `py-forward-except-block-bol'
Goto beginning of line following end of except-block."]

	 ["Forward expression bol" py-forward-expression-bol
	  :help " `py-forward-expression-bol'"]

	 ["Forward for block bol" py-forward-for-block-bol
	  :help " `py-forward-for-block-bol'
Goto beginning of line following end of for-block."]

	 ["Forward if block bol" py-forward-if-block-bol
	  :help " `py-forward-if-block-bol'
Goto beginning of line following end of if-block."]

	 ["Forward indent bol" py-forward-indent-bol
	  :help " `py-forward-indent-bol'
Go to beginning of line following of a section of equal indentation."]

	 ["Forward minor block bol" py-forward-minor-block-bol
	  :help " `py-forward-minor-block-bol'
Goto beginning of line following end of minor-block."]

	 ["Forward partial expression bol" py-forward-partial-expression-bol
	  :help " `py-forward-partial-expression-bol'"]

	 ["Forward section bol" py-forward-section-bol
	  :help " `py-forward-section-bol'"]

	 ["Forward statement bol" py-forward-statement-bol
	  :help " `py-forward-statement-bol'
Go to the beginning-of-line following current statement."]

	 ["Forward top level bol" py-forward-top-level-bol
	  :help " `py-forward-top-level-bol'
Go to end of top-level form at point, stop at next beginning-of-line."]

	 ["Forward try block bol" py-forward-try-block-bol
	  :help " `py-forward-try-block-bol'
Goto beginning of line following end of try-block."]))
       ("Up/Down"
	["Up" py-up
	 :help " `py-up'
Go up or to beginning of form if inside."]

	["Down" py-down
	 :help " `py-down'
Go to beginning one level below of compound statement or definition at point."]))
      ("Send"
       ["Execute block" py-execute-block
	:help " `py-execute-block'
Send block at point to interpreter."]

       ["Execute block or clause" py-execute-block-or-clause
	:help " `py-execute-block-or-clause'
Send block-or-clause at point to interpreter."]

       ["Execute buffer" py-execute-buffer
	:help " `py-execute-buffer'
:around advice: `ad-Advice-py-execute-buffer'"]

       ["Execute class" py-execute-class
	:help " `py-execute-class'
Send class at point to interpreter."]

       ["Execute clause" py-execute-clause
	:help " `py-execute-clause'
Send clause at point to interpreter."]

       ["Execute def" py-execute-def
	:help " `py-execute-def'
Send def at point to interpreter."]

       ["Execute def or class" py-execute-def-or-class
	:help " `py-execute-def-or-class'
Send def-or-class at point to interpreter."]

       ["Execute expression" py-execute-expression
	:help " `py-execute-expression'
Send expression at point to interpreter."]

       ["Execute indent" py-execute-indent
	:help " `py-execute-indent'
Send indent at point to interpreter."]

       ["Execute line" py-execute-line
	:help " `py-execute-line'
Send line at point to interpreter."]

       ["Execute minor block" py-execute-minor-block
	:help " `py-execute-minor-block'
Send minor-block at point to interpreter."]

       ["Execute paragraph" py-execute-paragraph
	:help " `py-execute-paragraph'
Send paragraph at point to interpreter."]

       ["Execute partial expression" py-execute-partial-expression
	:help " `py-execute-partial-expression'
Send partial-expression at point to interpreter."]

       ["Execute region" py-execute-region
	:help " `py-execute-region'
Send region at point to interpreter."]

       ["Execute statement" py-execute-statement
	:help " `py-execute-statement'
Send statement at point to interpreter."]

       ["Execute top level" py-execute-top-level
	:help " `py-execute-top-level'
Send top-level at point to interpreter."]
       ("Other"
	("IPython"
	 ["Execute block ipython" py-execute-block-ipython
	  :help " `py-execute-block-ipython'
Send block at point to IPython interpreter."]

	 ["Execute block or clause ipython" py-execute-block-or-clause-ipython
	  :help " `py-execute-block-or-clause-ipython'
Send block-or-clause at point to IPython interpreter."]

	 ["Execute buffer ipython" py-execute-buffer-ipython
	  :help " `py-execute-buffer-ipython'
Send buffer at point to IPython interpreter."]

	 ["Execute class ipython" py-execute-class-ipython
	  :help " `py-execute-class-ipython'
Send class at point to IPython interpreter."]

	 ["Execute clause ipython" py-execute-clause-ipython
	  :help " `py-execute-clause-ipython'
Send clause at point to IPython interpreter."]

	 ["Execute def ipython" py-execute-def-ipython
	  :help " `py-execute-def-ipython'
Send def at point to IPython interpreter."]

	 ["Execute def or class ipython" py-execute-def-or-class-ipython
	  :help " `py-execute-def-or-class-ipython'
Send def-or-class at point to IPython interpreter."]

	 ["Execute expression ipython" py-execute-expression-ipython
	  :help " `py-execute-expression-ipython'
Send expression at point to IPython interpreter."]

	 ["Execute indent ipython" py-execute-indent-ipython
	  :help " `py-execute-indent-ipython'
Send indent at point to IPython interpreter."]

	 ["Execute line ipython" py-execute-line-ipython
	  :help " `py-execute-line-ipython'
Send line at point to IPython interpreter."]

	 ["Execute minor block ipython" py-execute-minor-block-ipython
	  :help " `py-execute-minor-block-ipython'
Send minor-block at point to IPython interpreter."]

	 ["Execute paragraph ipython" py-execute-paragraph-ipython
	  :help " `py-execute-paragraph-ipython'
Send paragraph at point to IPython interpreter."]

	 ["Execute partial expression ipython" py-execute-partial-expression-ipython
	  :help " `py-execute-partial-expression-ipython'
Send partial-expression at point to IPython interpreter."]

	 ["Execute region ipython" py-execute-region-ipython
	  :help " `py-execute-region-ipython'
Send region at point to IPython interpreter."]

	 ["Execute statement ipython" py-execute-statement-ipython
	  :help " `py-execute-statement-ipython'
Send statement at point to IPython interpreter."]

	 ["Execute top level ipython" py-execute-top-level-ipython
	  :help " `py-execute-top-level-ipython'
Send top-level at point to IPython interpreter."])
	("IPython2"
	 ["Execute block ipython2" py-execute-block-ipython2
	  :help " `py-execute-block-ipython2'"]

	 ["Execute block or clause ipython2" py-execute-block-or-clause-ipython2
	  :help " `py-execute-block-or-clause-ipython2'"]

	 ["Execute buffer ipython2" py-execute-buffer-ipython2
	  :help " `py-execute-buffer-ipython2'"]

	 ["Execute class ipython2" py-execute-class-ipython2
	  :help " `py-execute-class-ipython2'"]

	 ["Execute clause ipython2" py-execute-clause-ipython2
	  :help " `py-execute-clause-ipython2'"]

	 ["Execute def ipython2" py-execute-def-ipython2
	  :help " `py-execute-def-ipython2'"]

	 ["Execute def or class ipython2" py-execute-def-or-class-ipython2
	  :help " `py-execute-def-or-class-ipython2'"]

	 ["Execute expression ipython2" py-execute-expression-ipython2
	  :help " `py-execute-expression-ipython2'"]

	 ["Execute indent ipython2" py-execute-indent-ipython2
	  :help " `py-execute-indent-ipython2'"]

	 ["Execute line ipython2" py-execute-line-ipython2
	  :help " `py-execute-line-ipython2'"]

	 ["Execute minor block ipython2" py-execute-minor-block-ipython2
	  :help " `py-execute-minor-block-ipython2'"]

	 ["Execute paragraph ipython2" py-execute-paragraph-ipython2
	  :help " `py-execute-paragraph-ipython2'"]

	 ["Execute partial expression ipython2" py-execute-partial-expression-ipython2
	  :help " `py-execute-partial-expression-ipython2'"]

	 ["Execute region ipython2" py-execute-region-ipython2
	  :help " `py-execute-region-ipython2'"]

	 ["Execute statement ipython2" py-execute-statement-ipython2
	  :help " `py-execute-statement-ipython2'"]

	 ["Execute top level ipython2" py-execute-top-level-ipython2
	  :help " `py-execute-top-level-ipython2'"])
	("IPython3"
	 ["Execute block ipython3" py-execute-block-ipython3
	  :help " `py-execute-block-ipython3'
Send block at point to IPython interpreter."]

	 ["Execute block or clause ipython3" py-execute-block-or-clause-ipython3
	  :help " `py-execute-block-or-clause-ipython3'
Send block-or-clause at point to IPython interpreter."]

	 ["Execute buffer ipython3" py-execute-buffer-ipython3
	  :help " `py-execute-buffer-ipython3'
Send buffer at point to IPython interpreter."]

	 ["Execute class ipython3" py-execute-class-ipython3
	  :help " `py-execute-class-ipython3'
Send class at point to IPython interpreter."]

	 ["Execute clause ipython3" py-execute-clause-ipython3
	  :help " `py-execute-clause-ipython3'
Send clause at point to IPython interpreter."]

	 ["Execute def ipython3" py-execute-def-ipython3
	  :help " `py-execute-def-ipython3'
Send def at point to IPython interpreter."]

	 ["Execute def or class ipython3" py-execute-def-or-class-ipython3
	  :help " `py-execute-def-or-class-ipython3'
Send def-or-class at point to IPython interpreter."]

	 ["Execute expression ipython3" py-execute-expression-ipython3
	  :help " `py-execute-expression-ipython3'
Send expression at point to IPython interpreter."]

	 ["Execute indent ipython3" py-execute-indent-ipython3
	  :help " `py-execute-indent-ipython3'
Send indent at point to IPython interpreter."]

	 ["Execute line ipython3" py-execute-line-ipython3
	  :help " `py-execute-line-ipython3'
Send line at point to IPython interpreter."]

	 ["Execute minor block ipython3" py-execute-minor-block-ipython3
	  :help " `py-execute-minor-block-ipython3'
Send minor-block at point to IPython interpreter."]

	 ["Execute paragraph ipython3" py-execute-paragraph-ipython3
	  :help " `py-execute-paragraph-ipython3'
Send paragraph at point to IPython interpreter."]

	 ["Execute partial expression ipython3" py-execute-partial-expression-ipython3
	  :help " `py-execute-partial-expression-ipython3'
Send partial-expression at point to IPython interpreter."]

	 ["Execute region ipython3" py-execute-region-ipython3
	  :help " `py-execute-region-ipython3'
Send region at point to IPython interpreter."]

	 ["Execute statement ipython3" py-execute-statement-ipython3
	  :help " `py-execute-statement-ipython3'
Send statement at point to IPython interpreter."]

	 ["Execute top level ipython3" py-execute-top-level-ipython3
	  :help " `py-execute-top-level-ipython3'
Send top-level at point to IPython interpreter."])
	("Jython"
	 ["Execute block jython" py-execute-block-jython
	  :help " `py-execute-block-jython'
Send block at point to Jython interpreter."]

	 ["Execute block or clause jython" py-execute-block-or-clause-jython
	  :help " `py-execute-block-or-clause-jython'
Send block-or-clause at point to Jython interpreter."]

	 ["Execute buffer jython" py-execute-buffer-jython
	  :help " `py-execute-buffer-jython'
Send buffer at point to Jython interpreter."]

	 ["Execute class jython" py-execute-class-jython
	  :help " `py-execute-class-jython'
Send class at point to Jython interpreter."]

	 ["Execute clause jython" py-execute-clause-jython
	  :help " `py-execute-clause-jython'
Send clause at point to Jython interpreter."]

	 ["Execute def jython" py-execute-def-jython
	  :help " `py-execute-def-jython'
Send def at point to Jython interpreter."]

	 ["Execute def or class jython" py-execute-def-or-class-jython
	  :help " `py-execute-def-or-class-jython'
Send def-or-class at point to Jython interpreter."]

	 ["Execute expression jython" py-execute-expression-jython
	  :help " `py-execute-expression-jython'
Send expression at point to Jython interpreter."]

	 ["Execute indent jython" py-execute-indent-jython
	  :help " `py-execute-indent-jython'
Send indent at point to Jython interpreter."]

	 ["Execute line jython" py-execute-line-jython
	  :help " `py-execute-line-jython'
Send line at point to Jython interpreter."]

	 ["Execute minor block jython" py-execute-minor-block-jython
	  :help " `py-execute-minor-block-jython'
Send minor-block at point to Jython interpreter."]

	 ["Execute paragraph jython" py-execute-paragraph-jython
	  :help " `py-execute-paragraph-jython'
Send paragraph at point to Jython interpreter."]

	 ["Execute partial expression jython" py-execute-partial-expression-jython
	  :help " `py-execute-partial-expression-jython'
Send partial-expression at point to Jython interpreter."]

	 ["Execute region jython" py-execute-region-jython
	  :help " `py-execute-region-jython'
Send region at point to Jython interpreter."]

	 ["Execute statement jython" py-execute-statement-jython
	  :help " `py-execute-statement-jython'
Send statement at point to Jython interpreter."]

	 ["Execute top level jython" py-execute-top-level-jython
	  :help " `py-execute-top-level-jython'
Send top-level at point to Jython interpreter."])
	("Python"
	 ["Execute block python" py-execute-block-python
	  :help " `py-execute-block-python'
Send block at point to default interpreter."]

	 ["Execute block or clause python" py-execute-block-or-clause-python
	  :help " `py-execute-block-or-clause-python'
Send block-or-clause at point to default interpreter."]

	 ["Execute buffer python" py-execute-buffer-python
	  :help " `py-execute-buffer-python'
Send buffer at point to default interpreter."]

	 ["Execute class python" py-execute-class-python
	  :help " `py-execute-class-python'
Send class at point to default interpreter."]

	 ["Execute clause python" py-execute-clause-python
	  :help " `py-execute-clause-python'
Send clause at point to default interpreter."]

	 ["Execute def python" py-execute-def-python
	  :help " `py-execute-def-python'
Send def at point to default interpreter."]

	 ["Execute def or class python" py-execute-def-or-class-python
	  :help " `py-execute-def-or-class-python'
Send def-or-class at point to default interpreter."]

	 ["Execute expression python" py-execute-expression-python
	  :help " `py-execute-expression-python'
Send expression at point to default interpreter."]

	 ["Execute indent python" py-execute-indent-python
	  :help " `py-execute-indent-python'
Send indent at point to default interpreter."]

	 ["Execute line python" py-execute-line-python
	  :help " `py-execute-line-python'
Send line at point to default interpreter."]

	 ["Execute minor block python" py-execute-minor-block-python
	  :help " `py-execute-minor-block-python'
Send minor-block at point to default interpreter."]

	 ["Execute paragraph python" py-execute-paragraph-python
	  :help " `py-execute-paragraph-python'
Send paragraph at point to default interpreter."]

	 ["Execute partial expression python" py-execute-partial-expression-python
	  :help " `py-execute-partial-expression-python'
Send partial-expression at point to default interpreter."]

	 ["Execute region python" py-execute-region-python
	  :help " `py-execute-region-python'
Send region at point to default interpreter."]

	 ["Execute statement python" py-execute-statement-python
	  :help " `py-execute-statement-python'
Send statement at point to default interpreter."]

	 ["Execute top level python" py-execute-top-level-python
	  :help " `py-execute-top-level-python'
Send top-level at point to default interpreter."])
	("Python2"
	 ["Execute block python2" py-execute-block-python2
	  :help " `py-execute-block-python2'
Send block at point to Python2 interpreter."]

	 ["Execute block or clause python2" py-execute-block-or-clause-python2
	  :help " `py-execute-block-or-clause-python2'
Send block-or-clause at point to Python2 interpreter."]

	 ["Execute buffer python2" py-execute-buffer-python2
	  :help " `py-execute-buffer-python2'
Send buffer at point to Python2 interpreter."]

	 ["Execute class python2" py-execute-class-python2
	  :help " `py-execute-class-python2'
Send class at point to Python2 interpreter."]

	 ["Execute clause python2" py-execute-clause-python2
	  :help " `py-execute-clause-python2'
Send clause at point to Python2 interpreter."]

	 ["Execute def python2" py-execute-def-python2
	  :help " `py-execute-def-python2'
Send def at point to Python2 interpreter."]

	 ["Execute def or class python2" py-execute-def-or-class-python2
	  :help " `py-execute-def-or-class-python2'
Send def-or-class at point to Python2 interpreter."]

	 ["Execute expression python2" py-execute-expression-python2
	  :help " `py-execute-expression-python2'
Send expression at point to Python2 interpreter."]

	 ["Execute indent python2" py-execute-indent-python2
	  :help " `py-execute-indent-python2'
Send indent at point to Python2 interpreter."]

	 ["Execute line python2" py-execute-line-python2
	  :help " `py-execute-line-python2'
Send line at point to Python2 interpreter."]

	 ["Execute minor block python2" py-execute-minor-block-python2
	  :help " `py-execute-minor-block-python2'
Send minor-block at point to Python2 interpreter."]

	 ["Execute paragraph python2" py-execute-paragraph-python2
	  :help " `py-execute-paragraph-python2'
Send paragraph at point to Python2 interpreter."]

	 ["Execute partial expression python2" py-execute-partial-expression-python2
	  :help " `py-execute-partial-expression-python2'
Send partial-expression at point to Python2 interpreter."]

	 ["Execute region python2" py-execute-region-python2
	  :help " `py-execute-region-python2'
Send region at point to Python2 interpreter."]

	 ["Execute statement python2" py-execute-statement-python2
	  :help " `py-execute-statement-python2'
Send statement at point to Python2 interpreter."]

	 ["Execute top level python2" py-execute-top-level-python2
	  :help " `py-execute-top-level-python2'
Send top-level at point to Python2 interpreter."])
	("Python3"
	 ["Execute block python3" py-execute-block-python3
	  :help " `py-execute-block-python3'
Send block at point to Python3 interpreter."]

	 ["Execute block or clause python3" py-execute-block-or-clause-python3
	  :help " `py-execute-block-or-clause-python3'
Send block-or-clause at point to Python3 interpreter."]

	 ["Execute buffer python3" py-execute-buffer-python3
	  :help " `py-execute-buffer-python3'
Send buffer at point to Python3 interpreter."]

	 ["Execute class python3" py-execute-class-python3
	  :help " `py-execute-class-python3'
Send class at point to Python3 interpreter."]

	 ["Execute clause python3" py-execute-clause-python3
	  :help " `py-execute-clause-python3'
Send clause at point to Python3 interpreter."]

	 ["Execute def python3" py-execute-def-python3
	  :help " `py-execute-def-python3'
Send def at point to Python3 interpreter."]

	 ["Execute def or class python3" py-execute-def-or-class-python3
	  :help " `py-execute-def-or-class-python3'
Send def-or-class at point to Python3 interpreter."]

	 ["Execute expression python3" py-execute-expression-python3
	  :help " `py-execute-expression-python3'
Send expression at point to Python3 interpreter."]

	 ["Execute indent python3" py-execute-indent-python3
	  :help " `py-execute-indent-python3'
Send indent at point to Python3 interpreter."]

	 ["Execute line python3" py-execute-line-python3
	  :help " `py-execute-line-python3'
Send line at point to Python3 interpreter."]

	 ["Execute minor block python3" py-execute-minor-block-python3
	  :help " `py-execute-minor-block-python3'
Send minor-block at point to Python3 interpreter."]

	 ["Execute paragraph python3" py-execute-paragraph-python3
	  :help " `py-execute-paragraph-python3'
Send paragraph at point to Python3 interpreter."]

	 ["Execute partial expression python3" py-execute-partial-expression-python3
	  :help " `py-execute-partial-expression-python3'
Send partial-expression at point to Python3 interpreter."]

	 ["Execute region python3" py-execute-region-python3
	  :help " `py-execute-region-python3'
Send region at point to Python3 interpreter."]

	 ["Execute statement python3" py-execute-statement-python3
	  :help " `py-execute-statement-python3'
Send statement at point to Python3 interpreter."]

	 ["Execute top level python3" py-execute-top-level-python3
	  :help " `py-execute-top-level-python3'
Send top-level at point to Python3 interpreter."])
	("Ignoring defaults "
	 :help "`M-x py-execute-statement- TAB' for example list commands ignoring defaults

 of `py-switch-buffers-on-execute-p' and `py-split-window-on-execute'")))
      ("Hide-Show"
       ("Hide"
	["Hide block" py-hide-block
	 :help " `py-hide-block'
Hide block at point."]

	["Hide top level" py-hide-top-level
	 :help " `py-hide-top-level'
Hide top-level at point."]

	["Hide def" py-hide-def
	 :help " `py-hide-def'
Hide def at point."]

	["Hide def or class" py-hide-def-or-class
	 :help " `py-hide-def-or-class'
Hide def-or-class at point."]

	["Hide statement" py-hide-statement
	 :help " `py-hide-statement'
Hide statement at point."]

	["Hide class" py-hide-class
	 :help " `py-hide-class'
Hide class at point."]

	["Hide clause" py-hide-clause
	 :help " `py-hide-clause'
Hide clause at point."]

	["Hide block or clause" py-hide-block-or-clause
	 :help " `py-hide-block-or-clause'
Hide block-or-clause at point."]

	["Hide comment" py-hide-comment
	 :help " `py-hide-comment'
Hide comment at point."]

	["Hide indent" py-hide-indent
	 :help " `py-hide-indent'
Hide indent at point."]

	["Hide expression" py-hide-expression
	 :help " `py-hide-expression'
Hide expression at point."]

	["Hide line" py-hide-line
	 :help " `py-hide-line'
Hide line at point."]

	["Hide for-block" py-hide-for-block
	 :help " `py-hide-for-block'
Hide for-block at point."]

	["Hide if-block" py-hide-if-block
	 :help " `py-hide-if-block'
Hide if-block at point."]

	["Hide elif-block" py-hide-elif-block
	 :help " `py-hide-elif-block'
Hide elif-block at point."]

	["Hide else-block" py-hide-else-block
	 :help " `py-hide-else-block'
Hide else-block at point."]

	["Hide except-block" py-hide-except-block
	 :help " `py-hide-except-block'
Hide except-block at point."]

	["Hide minor-block" py-hide-minor-block
	 :help " `py-hide-minor-block'
Hide minor-block at point."]

	["Hide paragraph" py-hide-paragraph
	 :help " `py-hide-paragraph'
Hide paragraph at point."]

	["Hide partial expression" py-hide-partial-expression
	 :help " `py-hide-partial-expression'
Hide partial-expression at point."]

	["Hide section" py-hide-section
	 :help " `py-hide-section'
Hide section at point."])
       ("Show"
	["Show all" py-show-all
	 :help " `py-show-all'
Show all in buffer."]

	["Show" py-show
	 :help " `py-show'
Show hidden code at point."]))
      ("Fast process"
       ["Execute block fast" py-execute-block-fast
	:help " `py-execute-block-fast'
Process block at point by a Python interpreter."]

       ["Execute block or clause fast" py-execute-block-or-clause-fast
	:help " `py-execute-block-or-clause-fast'
Process block-or-clause at point by a Python interpreter."]

       ["Execute class fast" py-execute-class-fast
	:help " `py-execute-class-fast'
Process class at point by a Python interpreter."]

       ["Execute clause fast" py-execute-clause-fast
	:help " `py-execute-clause-fast'
Process clause at point by a Python interpreter."]

       ["Execute def fast" py-execute-def-fast
	:help " `py-execute-def-fast'
Process def at point by a Python interpreter."]

       ["Execute def or class fast" py-execute-def-or-class-fast
	:help " `py-execute-def-or-class-fast'
Process def-or-class at point by a Python interpreter."]

       ["Execute expression fast" py-execute-expression-fast
	:help " `py-execute-expression-fast'
Process expression at point by a Python interpreter."]

       ["Execute partial expression fast" py-execute-partial-expression-fast
	:help " `py-execute-partial-expression-fast'
Process partial-expression at point by a Python interpreter."]

       ["Execute region fast" py-execute-region-fast
	:help " `py-execute-region-fast'"]

       ["Execute statement fast" py-execute-statement-fast
	:help " `py-execute-statement-fast'
Process statement at point by a Python interpreter."]

       ["Execute string fast" py-execute-string-fast
	:help " `py-execute-string-fast'"]

       ["Execute top level fast" py-execute-top-level-fast
	:help " `py-execute-top-level-fast'
Process top-level at point by a Python interpreter."])
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
Issue a virtualenvwrapper-like virtualenv-workon command"])

      ["Execute import or reload" py-execute-import-or-reload
       :help " `py-execute-import-or-reload'
Import the current buffer’s file in a Python interpreter."]
      ("Help"
       ["Find definition" py-find-definition
	:help " `py-find-definition'
Find source of definition of SYMBOL."]

       ["Help at point" py-help-at-point
	:help " `py-help-at-point'
Print help on symbol at point."]

       ["Info lookup symbol" py-info-lookup-symbol
	:help " `py-info-lookup-symbol'"]

       ["Symbol at point" py-symbol-at-point
	:help " `py-symbol-at-point'
Return the current Python symbol."])
      ("Debugger"
       ["Execute statement pdb" py-execute-statement-pdb
	:help " `py-execute-statement-pdb'
Execute statement running pdb."]

       ["Pdb" pdb
	:help " `pdb'
Run pdb on program FILE in buffer `*gud-FILE*'."])
      ("Checks"
       ["Pychecker run" py-pychecker-run
	:help " `py-pychecker-run'
*Run pychecker (default on the file currently visited)."]
       ("Pylint"
	["Pylint run" py-pylint-run
	 :help " `py-pylint-run'
*Run pylint (default on the file currently visited)."]

	["Pylint help" py-pylint-help
	 :help " `py-pylint-help'
Display Pylint command line help messages."]

	["Pylint flymake mode" pylint-flymake-mode
	 :help " `pylint-flymake-mode'
Toggle `pylint' `flymake-mode'."])
       ("Pep8"
	["Pep8 run" py-pep8-run
	 :help " `py-pep8-run'
*Run pep8, check formatting - default on the file currently visited."]

	["Pep8 help" py-pep8-help
	 :help " `py-pep8-help'
Display pep8 command line help messages."]

	["Pep8 flymake mode" pep8-flymake-mode
	 :help " `pep8-flymake-mode'
Toggle `pep8’ `flymake-mode'."])
       ("Pyflakes"
	["Pyflakes run" py-pyflakes-run
	 :help " `py-pyflakes-run'
*Run pyflakes (default on the file currently visited)."]

	["Pyflakes help" py-pyflakes-help
	 :help " `py-pyflakes-help'
Display Pyflakes command line help messages."]

	["Pyflakes flymake mode" pyflakes-flymake-mode
	 :help " `pyflakes-flymake-mode'
Toggle `pyflakes' `flymake-mode'."])
       ("Flake8"
	["Flake8 run" py-flake8-run
	 :help " `py-flake8-run'
Flake8 is a wrapper around these tools:"]

	["Flake8 help" py-flake8-help
	 :help " `py-flake8-help'
Display flake8 command line help messages."]
	("Pyflakes-pep8"
	 ["Pyflakes pep8 run" py-pyflakes-pep8-run
	  :help " `py-pyflakes-pep8-run'"]

	 ["Pyflakes pep8 help" py-pyflakes-pep8-help
	  :help " `py-pyflakes-pep8-help'"]

	 ["Pyflakes pep8 flymake mode" pyflakes-pep8-flymake-mode
	  :help " `pyflakes-pep8-flymake-mode'"])))
      ("Customize"

       ["Python-mode customize group" (customize-group 'python-mode)
	:help "Open the customization buffer for Python mode"]
       ("Switches"
	:help "Toggle useful modes"
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
	 )

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
	  :help " `py-toggle-use-current-dir-when-execute-p'

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
    4, 5, 6,]

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

	   ["Toggle py-smart-indentation" py-toggle-smart-indentation
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
commands `py-backward-top-level', `py-forward-top-level'

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

	  ["Toggle underscore word syntax" py-toggle-underscore-word-syntax-p
	   :help " `py-toggle-underscore-word-syntax-p'

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
	 ;; ["No session mode "
	 ;; 	  (setq py-no-session-p
	 ;; 		(not py-no-session-p))
	 ;; 	  :help "If shell should be in session-mode.

	 ;; Default is nil. Use `M-x customize-variable' to set it permanently"
	 ;; 	  :style toggle :selected py-no-session-p]

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
Edit the assignment of a boolean variable, revert them."]

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
Delete preceding character or level of indentation."]

	["Electric colon" py-electric-colon
	 :help " `py-electric-colon'
Insert a colon and indent accordingly."]

	["Electric comment" py-electric-comment
	 :help " `py-electric-comment'
Insert a comment. If starting a comment, indent accordingly."]

	["Electric delete" py-electric-delete
	 :help " `py-electric-delete'
Delete following character or levels of whitespace."]

	["Electric yank" py-electric-yank
	 :help " `py-electric-yank'
Perform command `yank' followed by an `indent-according-to-mode'"]

	["Hungry delete backwards" py-hungry-delete-backwards
	 :help " `py-hungry-delete-backwards'
Delete the preceding character or all preceding whitespace"]

	["Hungry delete forward" py-hungry-delete-forward
	 :help " `py-hungry-delete-forward'
Delete the following character or all following whitespace"])
       ("Filling"
	["Py docstring style" py-docstring-style
	 :help " `py-docstring-style'"]

	["Py fill comment" py-fill-comment
	 :help " `py-fill-comment'"]

	["Py fill paragraph" py-fill-paragraph
	 :help " `py-fill-paragraph'"]

	["Py fill string" py-fill-string
	 :help " `py-fill-string'"]

	["Py fill string django" py-fill-string-django
	 :help " `py-fill-string-django'"]

	["Py fill string onetwo" py-fill-string-onetwo
	 :help " `py-fill-string-onetwo'"]

	["Py fill string pep 257" py-fill-string-pep-257
	 :help " `py-fill-string-pep-257'"]

	["Py fill string pep 257 nn" py-fill-string-pep-257-nn
	 :help " `py-fill-string-pep-257-nn'"]

	["Py fill string symmetric" py-fill-string-symmetric
	 :help " `py-fill-string-symmetric'"])
       ("Abbrevs"	   :help "see also `py-add-abbrev'"
	:filter (lambda (&rest junk)
		  (abbrev-table-menu python-mode-abbrev-table)))

       ["Add abbrev" py-add-abbrev
	:help " `py-add-abbrev'
Defines python-mode specific abbrev for last expressions before point."]
       ("Completion"
	["Py indent or complete" py-indent-or-complete
	 :help " `py-indent-or-complete'"]

	["Py shell complete" py-shell-complete
	 :help " `py-shell-complete'"]

	["Py complete" py-complete
	 :help " `py-complete'"])

       ["Find function" py-find-function
	:help " `py-find-function'
Find source of definition of SYMBOL."])))
  map)

(provide 'python-components-menu)
;;; python-components-menu.el ends here
