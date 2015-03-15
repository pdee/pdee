;; python-components-intern.el --- Part of python-components-mode

;; Helper functions

;; Maintainer: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages, processes

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
;;

;;; Code:

;;  Keymap
(defvar python-mode-map nil)
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

		   ["Python2" python2
		    :help " `python2'

Start an Python2 interpreter.

Optional C-u prompts for path to the interpreter. "]

		   ["Python3" python3
		    :help " `python3'

Start an Python3 interpreter.

Optional C-u prompts for path to the interpreter. "]

		   ["Ipython" ipython
		    :help " `ipython'

Start an IPython interpreter.

Optional C-u prompts for path to the interpreter. "]

		   ["Ipython2.7" ipython2.7
		    :help " `ipython2.7'

Start an IPython2.7 interpreter.

Optional C-u prompts for path to the interpreter. "]

		   ["Ipython3" ipython3
		    :help " `ipython3'

Start an IPython3 interpreter.

Optional C-u prompts for path to the interpreter. "]

		   ["Jython" jython
		    :help " `jython'

Start an Jython interpreter.

Optional C-u prompts for path to the interpreter. "]

		   ("Dedicated"
		    :help "Dedicated Shells"
		    ["Python dedicated" python-dedicated
		     :help " `python-dedicated'

Start an unique Python interpreter in another window.

Optional C-u prompts for path to the interpreter. "]

		    ["Python2 dedicated" python2-dedicated
		     :help " `python2-dedicated'

Start an unique Python2 interpreter in another window.

Optional C-u prompts for path to the interpreter. "]

		    ["Python3 dedicated" python3-dedicated
		     :help " `python3-dedicated'

Start an unique Python3 interpreter in another window.

Optional C-u prompts for path to the interpreter. "]

		    ["Ipython dedicated" ipython-dedicated
		     :help " `ipython-dedicated'

Start an unique IPython interpreter in another window.

Optional C-u prompts for path to the interpreter. "]

		    ["Ipython2.7 dedicated" ipython2.7-dedicated
		     :help " `ipython2.7-dedicated'

Start an unique IPython2.7 interpreter in another window.

Optional C-u prompts for path to the interpreter. "]

		    ["Ipython3 dedicated" ipython3-dedicated
		     :help " `ipython3-dedicated'

Start an unique IPython3 interpreter in another window.

Optional C-u prompts for path to the interpreter. "]

		    ["Jython dedicated" jython-dedicated
		     :help " `jython-dedicated'

Start an unique Jython interpreter in another window.

Optional C-u prompts for path to the interpreter. "]
		    )

		   ("Switch"
		    :help "Switch to shell"
		    ["Python switch" python-switch
		     :help " `python-switch'

Switch to Python interpreter in another window.

Optional C-u prompts for path to the interpreter. "]

		    ["Python2 switch" python2-switch
		     :help " `python2-switch'

Switch to Python2 interpreter in another window.

Optional C-u prompts for path to the interpreter. "]

		    ["Python3 switch" python3-switch
		     :help " `python3-switch'

Switch to Python3 interpreter in another window.

Optional C-u prompts for path to the interpreter. "]

		    ["Ipython switch" ipython-switch
		     :help " `ipython-switch'

Switch to IPython interpreter in another window.

Optional C-u prompts for path to the interpreter. "]

		    ["Ipython2.7 switch" ipython2.7-switch
		     :help " `ipython2.7-switch'

Switch to IPython2.7 interpreter in another window.

Optional C-u prompts for path to the interpreter. "]

		    ["Ipython3 switch" ipython3-switch
		     :help " `ipython3-switch'

Switch to IPython3 interpreter in another window.

Optional C-u prompts for path to the interpreter. "]

		    ["Jython switch" jython-switch
		     :help " `jython-switch'

Switch to Jython interpreter in another window.

Optional C-u prompts for path to the interpreter. "]
		    ))

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
`py-switch-buffers-on-execute-p' and `py-split-window-on-execute'"            ;; switch

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
`py-switch-buffers-on-execute-p' and `py-split-window-on-execute'"            ;; switch

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
`py-switch-buffers-on-execute-p' and `py-split-window-on-execute'"            ;; switch

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
`py-switch-buffers-on-execute-p' and `py-split-window-on-execute'"            ;; switch

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
`py-switch-buffers-on-execute-p' and `py-split-window-on-execute'"            ;; switch

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
`py-switch-buffers-on-execute-p' and `py-split-window-on-execute'"

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

		  ["Flycheck mode" py-flycheck-mode
		   :help " `py-flycheck-mode'

Toggle `flycheck-mode'\.

See menu \"Tools/Syntax Checking\""]

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

;;  avoid errors from ipython.el - which isn't needed anymore
(defvaralias 'py-mode-map 'python-mode-map)

(defvar py-python-shell-mode-map
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

Optional C-u prompts for options to pass to the Jython interpreter. See `py-python-command-args'."])
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
Complete symbol before point using Pymacs . "])

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
Use pydoc on symbol at point"])

	     ("Debugger"

	      ["pdb" pdb
	       :help "`pdb' Run pdb under GUD"])

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

	       ("Edit"

		("Completion"

		 ["Set Pymacs-based complete keymap "
		  (setq py-set-complete-keymap-p
			(not py-set-complete-keymap-p))
		  :help "If `py-complete-initialize', which sets up enviroment for Pymacs based py-complete, should load it's keys into `py-shell-mode-map'

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
    map)

  "Used inside a Python-shell")

(defvar py-ipython-shell-mode-map py-python-shell-mode-map
  "Unless setting of ipython-shell-mode needs to be different, let's save some lines of code and copy py-python-shell-mode-map here.")

(defvar py-shell-map py-python-shell-mode-map)

(when py-org-cycle-p
  (define-key python-mode-map (kbd "<backtab>") 'org-cycle))

(defun py-load-skeletons ()
  "Load skeletons from extensions. "
  (interactive)
  (load (concat py-install-directory "/extensions/python-components-skeletons.el")))

(defun py--kill-emacs-hook ()
  "Delete files in `py-file-queue'.
These are Python temporary files awaiting execution."
  (mapc #'(lambda (filename)
            (ignore-errors (delete-file filename)))
        py-file-queue))

;;  Add a designator to the minor mode strings
(or (assq 'py-pdbtrack-is-tracking-p minor-mode-alist)
    (push '(py-pdbtrack-is-tracking-p py-pdbtrack-minor-mode-string)
          minor-mode-alist))

;;  bottle.py
;;  py   = sys.version_info
;;  py3k = py >= (3,0,0)
;;  py25 = py <  (2,6,0)
;;  py31 = (3,1,0) <= py < (3,2,0)

;;  sys.version_info[0]
(defun py-python-version (&optional executable verbose)
  "Returns versions number of a Python EXECUTABLE, string.

If no EXECUTABLE given, `py-shell-name' is used.
Interactively output of `--version' is displayed. "
  (interactive)
  (let* ((executable (or executable py-shell-name))
         (erg (py--string-strip (shell-command-to-string (concat executable " --version")))))
    (when (interactive-p) (message "%s" erg))
    (unless verbose (setq erg (cadr (split-string erg))))
    erg))

(defun py-version ()
  "Echo the current version of `python-mode' in the minibuffer."
  (interactive)
  (message "Using `python-mode' version %s" py-version)
  (py-keep-region-active))

;;  Utility stuff
(declare-function compilation-shell-minor-mode "compile" (&optional arg))

;; dereived from shipped python.el
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

;;  Miscellany.
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

;;  Hooks
;;  arrange to kill temp files when Emacs exists
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

;;  credits to python.el
;;  (defun py-beg-of-defun-function ()
;;    (set (make-local-variable 'beginning-of-defun-function)
;;         'py-beginning-of-def-or-class))
;;
;;  (defun py-end-of-defun-function ()
;;    (set (make-local-variable 'end-of-defun-function) 'py-end-of-def-or-class))

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

;;  backward compatibility
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
                     (py--string-strip
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

;;  FixMe: for unknown reasons this is not done by mode
(if (file-readable-p abbrev-file-name)
    (add-hook 'python-mode-hook
              (lambda ()
                (setq py-this-abbrevs-changed abbrevs-changed)
                (load abbrev-file-name nil t)
                (setq abbrevs-changed py-this-abbrevs-changed)))
  (message "Warning: %s" "no abbrev-file found, customize `abbrev-file-name' in order to make mode-specific abbrevs work. "))

;; ;
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

;; ;

(defun py--input-filter (str)
  "`comint-input-filter' function for Python.

Don't save anything for STR matching `py-input-filter-re' "
  (not (string-match py-input-filter-re str)))

(make-obsolete 'jpython-mode 'jython-mode nil)

(add-to-list 'same-window-buffer-names (purecopy "*Python*"))
(add-to-list 'same-window-buffer-names (purecopy "*IPython*"))

(add-to-list 'auto-mode-alist (cons (purecopy "\\.py\\'")  'python-mode))

;;  (add-to-list 'interpreter-mode-alist
;;  (cons (purecopy "[bi]*python[0-9.]*") 'python-mode))
;;
;;  (add-to-list 'interpreter-mode-alist
;;  (cons (purecopy "jython[0-9.]*") 'jython-mode))

(add-to-list 'magic-mode-alist
	     '("!#[ \t]*/.*[jp]ython[0-9.]*" . python-mode))

;;  lp:1355458, what about using `magic-mode-alist'?

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

(defun py--unfontify-banner-intern ()
  (save-excursion
    (goto-char (point-min))
    (let ((erg (or (ignore-errors (car comint-last-prompt))
		   (and
		    (re-search-forward py-fast-filter-re nil t 1)
		    (match-beginning 0))
		   (progn
		     (forward-paragraph)
		     (point)))))
      ;; (sit-for 1 t)
      (if erg
	  (progn
	    (font-lock-unfontify-region (point-min) erg)
	    (goto-char (point-max)))
	(progn (and py-debug-p (message "%s" (concat "py--unfontify-banner: Don't see a prompt in buffer " (buffer-name buffer)))))))))

(defun py--unfontify-banner (&optional buffer)
  "Unfontify the shell banner-text.

Cancels `py--timer'
Expects being called by `py--run-unfontify-timer' "
  (interactive)
    (let ((buffer (or buffer (current-buffer))))
      (if (ignore-errors (buffer-live-p (get-buffer buffer)))
	  (with-current-buffer buffer
	    (py--unfontify-banner-intern)
	    (and (timerp py--timer)(cancel-timer py--timer)))
	(and (timerp py--timer)(cancel-timer py--timer)))))

(defun py--run-unfontify-timer (&optional buffer)
  "Unfontify the shell banner-text "
  (when py--shell-unfontify
    (let ((buffer (or buffer (current-buffer)))
	  done)
      (if (and
	   (buffer-live-p buffer)
	   (or
	    (eq major-mode 'py-python-shell-mode)
	    (eq major-mode 'py-ipython-shell-mode)))
	  (unless py--timer
	    (setq py--timer
		  (run-with-idle-timer
		   (if py--timer-delay (setq py--timer-delay 3)
		     (setq py--timer-delay 0.1))
		   nil
		   #'py--unfontify-banner buffer)))
	(cancel-timer py--timer)))))

;;  unconditional Hooks
;;  (orgstruct-mode 1)
(add-hook 'python-mode-hook
	  (lambda ()
	    (setq imenu-create-index-function py--imenu-create-index-function)
	    (setq indent-tabs-mode py-indent-tabs-mode)))

(remove-hook 'python-mode-hook 'python-setup-brm)
;; ;

(defun py-complete-auto ()
  "Auto-complete function using py-complete. "
  ;; disable company
  ;; (when company-mode (company-mode))
  (let ((modified (buffer-chars-modified-tick)))
    ;; don't try completion if buffer wasn't modified
    (unless (eq modified py-complete-last-modified)
      (if py-auto-completion-mode-p
	  (if (string= "*PythonCompletions*" (buffer-name (current-buffer)))
	      (sit-for 0.1 t)
	    (if
		(eq py-auto-completion-buffer (current-buffer))
		;; not after whitespace, TAB or newline
		(unless (member (char-before) (list 32 9 10))
		  (py-complete)
		  (setq py-complete-last-modified (buffer-chars-modified-tick)))
	      (setq py-auto-completion-mode-p nil
		    py-auto-completion-buffer nil)
	      (cancel-timer py--auto-complete-timer)))))))

(defun py-set-command-args (arguments)
  "Set Python arguments on the fly, override defaults in this session.

Use `defcustom' to keep value across sessions "
  (interactive
   (list
    (read-from-minibuffer "Command args: " py-python-command-args)))
    (setq py-python-command-args arguments))

(defun py---emacs-version-greater-23 ()
  "Return `t' if emacs major version is above 23"
  (< 23 (string-to-number (car (split-string emacs-version "\\.")))))

(defun py-beginning-of-commented-section (&optional last)
  "Leave upwards comments and/or empty lines. "
  (interactive)
  (let ((pps (syntax-ppss))
        (last (or last (point))))
    (if (and (or (and (nth 4 pps)(goto-char (nth 8 pps)))(looking-at comment-start))
             (looking-back "^[ \t]*")(not (bobp)))
        (progn
          (skip-chars-backward " \t\r\n\f")
          (py-beginning-of-commented-section last))
      (goto-char last))))

(defun py--empty-arglist-indent (nesting py-indent-offset indent-offset)
  "Internally used by `py-compute-indentation'"
  (if
      (and (eq 1 nesting)
           (save-excursion
             (back-to-indentation)
             (looking-at py-extended-block-or-clause-re)))
      (progn
        (back-to-indentation)
        (+ (current-column) (* 2 (or indent-offset py-indent-offset))))
    (+ (current-indentation) py-indent-offset)))

(defun py-symbol-at-point ()
  "Return the current Python symbol."
  (interactive)
  (let ((erg (with-syntax-table
                 py-dotted-expression-syntax-table
               (current-word))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-kill-buffer-unconditional (buffer)
  "Kill buffer unconditional, kill buffer-process if existing. "
  (interactive
   (list (current-buffer)))
  (let ((buffer (or (and (bufferp buffer) buffer)
		    (get-buffer buffer)))
	proc kill-buffer-query-functions)

    (ignore-errors
      (setq proc (get-buffer-process buffer))
      (and proc (kill-process proc))
      (set-buffer buffer)
      (set-buffer-modified-p 'nil)
      (kill-buffer (current-buffer)))))

(defun py--line-backward-maybe ()
  (let ((orig (point)))
    (skip-chars-backward " \t\f" (line-beginning-position))
    (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
      (setq line t))))

(defun py--after-empty-line ()
  "Return `t' if line before contains only whitespace characters. "
  (save-excursion
    (beginning-of-line)
    (forward-line -1)
    (beginning-of-line)
    (looking-at "\\s-*$")))

(defalias 'py-count-indentation 'py-compute-indentation)
(defun py-compute-indentation (&optional orig origline closing line nesting repeat indent-offset liep)
  "Compute Python indentation.

When HONOR-BLOCK-CLOSE-P is non-nil, statements such as `return',
`raise', `break', `continue', and `pass' force one level of dedenting.

Optional arguments are flags resp. values set and used by `py-compute-indentation' internally:
ORIG keeps original position
ORIGLINE keeps line where compute started
CLOSING is t when started at a char delimiting a list as \"]})\"
LINE indicates being not at origline now
NESTING tells repeated executing was started from inside a list
REPEAT counter enables checks against `py-max-specpdl-size'
INDENT-OFFSET allows calculation of block-local values
LIEP stores line-end-position at point-of-interest
"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      ;; in shell, narrow from previous prompt
      ;; needed by closing
      (unless orig (unless (bobp) (back-to-indentation)))
      (let* ((orig (or orig (point)))
             (origline (or origline (py-count-lines (point-min) (point))))
             ;; closing indicates: when started, looked
             ;; at a single closing parenthesis
             ;; line: moved already a line backward
             (liep (or liep (line-end-position)))
             (line line)
             (pps (syntax-ppss))
             (closing
              (or closing
                  (and (nth 1 pps)
                       (looking-at ".*\\(\\s)\\)")(nth 0 pps)
                       ;; char doesn't matter for now, maybe drop
                       (string-to-char (match-string-no-properties 1)))))
             ;; in a recursive call already
             (repeat (if repeat
			 (setq repeat (1+ repeat))
		       0))
             ;; nesting: started nesting a list
             (nesting nesting)
             (indent-offset (or indent-offset py-indent-offset))
             (cubuf (current-buffer))
             erg indent this-line)
        (if (and (< repeat 1)
                 (and (comint-check-proc (current-buffer))
                      (re-search-backward (concat py-shell-prompt-regexp "\\|" py-ipython-output-prompt-re "\\|" py-ipython-input-prompt-re) nil t 1)))
            ;; common recursion not suitable because of prompt
            (with-temp-buffer
	      ;; (when py-debug-p (switch-to-buffer (current-buffer)))
              (insert-buffer-substring cubuf (match-end 0) orig)
              (setq indent (py-compute-indentation)))
	  (if (< py-max-specpdl-size repeat)
	      (error "`py-compute-indentation' reached loops max.")
	    (setq nesting (nth 0 pps))
	    (setq indent
		  (cond
		   ((and (bobp) (eq liep (line-end-position)))
		    0)
		   ((and (bobp)(py--statement-opens-block-p py-extended-block-or-clause-re))
		    (+ (if py-smart-indentation (py-guess-indent-offset) indent-offset) (current-indentation)))
		   ((and (bobp)(not (py--statement-opens-block-p py-extended-block-or-clause-re)))
		    (current-indentation))
		   ;; in string
		   ((and (nth 3 pps)(nth 8 pps))
		    (if
			;; still at original line
			(eq origline (line-end-position))
			(progn
			  (forward-line -1)
			  (end-of-line)
			  (skip-chars-backward " \t\r\n\f")
			  (if (ignore-errors (< (nth 8 (syntax-ppss)) (line-beginning-position)))
			      (current-indentation)
			    (ignore-errors (goto-char (nth 8 pps)))
			    (py--line-backward-maybe)
			    (back-to-indentation)
			    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep)))
		      (goto-char (nth 8 pps))
		      (current-indentation)))
		   ((and (looking-at "\"\"\"\\|'''")(not (bobp)))
		    (py-beginning-of-statement)
		    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		   ;; comments
		   ((nth 8 pps)
		    (if (eq liep (line-end-position))
			(progn
			  (goto-char (nth 8 pps))
			  (py--line-backward-maybe)
			  (skip-chars-backward " \t")
			  (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		      (goto-char (nth 8 pps))
		      (if
			  line
			  (if py-indent-honors-inline-comment
			      (current-column)
			    (if py-indent-comments
				(progn
				  (py-beginning-of-commented-section)
				  (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
			      0))
			(forward-char -1)
			(py-compute-indentation orig origline closing line nesting repeat indent-offset liep))))
		   ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*")(not line)
			 (eq liep (line-end-position)))
		    (if py-indent-comments
			(progn
			  (setq line t)
			  (skip-chars-backward " \t\r\n\f")
			  ;; as previous comment-line might
			  ;; be wrongly unindented, travel
			  ;; whole commented section
			  (py-beginning-of-commented-section)
			  (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		      0))
		   ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*")(not
									 (eq liep (line-end-position))))
		    (current-indentation))
		   ((and (eq ?\# (char-after)) line py-indent-honors-inline-comment)
		    (current-column))
		   ;; lists
		   ((nth 1 pps)
		    (if
			;; ((and nesting (not line))
			nesting
			;; still at original line
			(save-excursion
			  (goto-char (nth 1 pps))
			  (setq this-line (py-count-lines))
			  (cond
			   ((< 0 (- origline this-line))
			    (if (< 1 (- origline this-line))
				(cond
				 (closing
				  (cond
				   (py-closing-list-dedents-bos
				    (goto-char (nth 1 pps))
				    (current-indentation))
				   ((looking-back "^[ \t]*")
				    (current-column))
				   ((and (looking-at "\\s([ \t]*$") py-closing-list-keeps-space)
				    (+ (current-column) py-closing-list-space))
				   ((looking-at "\\s([ \t]*$")
				    (py--empty-arglist-indent nesting py-indent-offset indent-offset))
				   ((looking-at "\\s([ \t]*\\([^ \t]+.*\\)$")
				    (goto-char (match-beginning 1))
				    (if py-indent-paren-spanned-multilines-p
					(+ (current-column) py-indent-offset)
				      (current-column)))
				   (t (py--fetch-previous-indent orig))))
				 ;; already behind a dedented element in list
				 ((<= 2 (- origline this-line))
				  (py--fetch-previous-indent orig))
				 ((< (current-indentation) (current-column))
				  (+ (current-indentation) py-indent-offset))
				 (t (py--fetch-previous-indent orig)))
			      (cond ((looking-at "\\s([ \t]*$")
				     (py--empty-arglist-indent nesting py-indent-offset indent-offset))
				    ((looking-at "\\s([ \t]*\\([^ \t]+.*\\)$")
				     (goto-char (match-beginning 1))
				     (if py-indent-paren-spanned-multilines-p
					 (+ (current-column) py-indent-offset)
				       (current-column)))
				    (t (+ (current-column) (* (nth 0 pps)))))))
			   ((nth 1 (syntax-ppss))
			    (goto-char (nth 1 (syntax-ppss)))
			    (setq line
				  ;; should be faster
				  (< (line-end-position) liep))
			    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
			   ((not (py--beginning-of-statement-p))
			    (py-beginning-of-statement)
			    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
			   (t (1+ (current-column)))))
		      (if line
			  (progn
			    (py-beginning-of-statement)
			    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
			(goto-char (+ py-lhs-inbound-indent (nth 1 pps)))
			(when (looking-at "[ \t]+")
			  (goto-char (match-end 0)))
			(current-column))))
		   ((and (eq (char-after) (or ?\( ?\{ ?\[)) line)
		    (1+ (current-column)))
		   ((py-preceding-line-backslashed-p)
		    (progn
		      (py-beginning-of-statement)
		      (setq this-line (py-count-lines))
		      (if (< 1 (- origline this-line))
			  (py--fetch-previous-indent orig)
			(if (looking-at "from +\\([^ \t\n]+\\) +import")
			    py-backslashed-lines-indent-offset
			  (+ (current-indentation) py-continuation-offset)))))
		   ((and (looking-at py-block-closing-keywords-re)
			 (eq liep (line-end-position)))
		    (skip-chars-backward "[ \t\r\n\f]")
		    (py-beginning-of-statement)
		    (cond ((looking-at py-extended-block-or-clause-re)
			   (+
			    (if py-smart-indentation (py-guess-indent-offset) indent-offset)
			    (current-indentation)))
			  ((looking-at py-block-closing-keywords-re)
			   (- (current-indentation) py-indent-offset))
			  (t (current-column))))
		   ((looking-at py-block-closing-keywords-re)
		    (if (< (line-end-position) orig)
			(- (current-indentation) py-indent-offset)
		      (py-beginning-of-block-or-clause (current-indentation))
		      (current-indentation)))
		   ((looking-at py-no-outdent-re)
		    (if
			(eq liep (line-end-position))
			(progn
			  (back-to-indentation)
			  (py--line-backward-maybe)
			  (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		      (current-indentation)))
		   ((and (looking-at py-elif-re) (eq (py-count-lines) origline))
		    (py--line-backward-maybe)
		    (car (py--clause-lookup-keyword py-elif-re -1 nil orig origline)))
		   ((and (looking-at py-clause-re)(not line)
			 (eq liep (line-end-position)))
		    (cond ((looking-at py-finally-re)
			   (car (py--clause-lookup-keyword py-finally-re -1 nil orig origline)))
			  ((looking-at py-except-re)
			   (car (py--clause-lookup-keyword py-except-re -1 nil orig origline)))
			  ((looking-at py-else-re)
			   (car (py--clause-lookup-keyword py-else-re -1 nil orig origline)))
			  ((looking-at py-elif-re)
			   (car (py--clause-lookup-keyword py-elif-re -1 nil orig origline)))
			  ;; maybe at if, try, with
			  (t (car (py--clause-lookup-keyword py-block-or-clause-re -1 nil orig origline)))))
		   ((looking-at py-extended-block-or-clause-re)
		    (cond ((and (not line)
				(eq liep (line-end-position)))
			   (py--line-backward-maybe)
			   (setq line t)
			   (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
			  (t (+
			      (cond (indent-offset)
				    (py-smart-indentation
				     (py-guess-indent-offset))
				    (t py-indent-offset))
			      (current-indentation)))))
		   ((and
		     (< (line-end-position) liep)
		     (eq (current-column) (current-indentation)))
		    (and
		     (looking-at py-assignment-re)
		     (goto-char (match-end 0)))
		    ;; multiline-assignment
		    (if (and nesting (looking-at " *[[{(]")(not (looking-at ".+[]})][ \t]*$")))
			(+ (current-indentation) py-indent-offset)
		      (current-indentation)))
		   ((looking-at py-assignment-re)
		    (py-beginning-of-statement)
		    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		   ((and (< (current-indentation) (current-column))(not line))
		    (back-to-indentation)
		    (unless line
		      (setq nesting (nth 0 (syntax-ppss))))
		    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		   ((and (not (py--beginning-of-statement-p)) (not (and line (eq ?\# (char-after)))))
		    (if (bobp)
			(current-column)
		      (if (eq (point) orig)
			  (progn
			    (py--line-backward-maybe)
			    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
			(py-beginning-of-statement)
			(py-compute-indentation orig origline closing line nesting repeat indent-offset liep))))
		   ((or (py--statement-opens-block-p py-extended-block-or-clause-re)(looking-at "@"))
		    (if (< (py-count-lines) origline)
			(+ (if py-smart-indentation (py-guess-indent-offset) indent-offset) (current-indentation))
		      (skip-chars-backward " \t\r\n\f")
		      (setq line t)
		      (back-to-indentation)
		      (py-compute-indentation orig origline closing line nesting repeat indent-offset liep)))
		   ((and py-empty-line-closes-p (py--after-empty-line))
		    (progn (py-beginning-of-statement)
			   (- (current-indentation) py-indent-offset)))
		   ;; still at orignial line
		   ((and (eq liep (line-end-position))
			 (save-excursion
			   (and (setq erg (py--go-to-keyword py-extended-block-or-clause-re))
				(if py-smart-indentation (setq indent-offset (py-guess-indent-offset)) t)
				(ignore-errors (< orig (or (py-end-of-block-or-clause)(point)))))))
		    (+ (car erg) (if py-smart-indentation
				     (or indent (py-guess-indent-offset))
				   indent-offset)))
		   ((and (not line)
			 (eq liep (line-end-position))
			 (py--beginning-of-statement-p))
		    (py-beginning-of-statement)
		    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		   (t (current-indentation))))
	    (when (and py-verbose-p (interactive-p)) (message "%s" indent))
	    indent))))))

(defun py--fetch-previous-indent (orig)
  "Report the preceding indent. "
  (save-excursion
    (goto-char orig)
    (forward-line -1)
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (current-indentation)))

(defun py-continuation-offset (&optional arg)
  "With numeric ARG different from 1 py-continuation-offset is set to that value; returns py-continuation-offset. "
  (interactive "p")
  (let ((erg (if (eq 1 arg)
                 py-continuation-offset
               (when (numberp arg)
                 (prog1
                     arg
                   (setq py-continuation-offset arg))))))
    (when (and py-verbose-p (interactive-p)) (message "%s" py-continuation-offset))
    py-continuation-offset))

(defalias 'pios 'py-indentation-of-statement)
(defalias 'ios 'py-indentation-of-statement)
(defun py-indentation-of-statement ()
  "Returns the indenation of the statement at point. "
  (interactive)
  (let ((erg (save-excursion
               (back-to-indentation)
               (or (py--beginning-of-statement-p)
                   (py-beginning-of-statement))
               (current-indentation))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-in-list-p 'py-list-beginning-position)
(defun py-list-beginning-position (&optional start)
  "Return lists beginning position, nil if not inside.

Optional ARG indicates a start-position for `parse-partial-sexp'."
  (interactive)
  (let* ((ppstart (or start (point-min)))
         (erg (nth 1 (syntax-ppss))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-end-of-list-position (&optional arg)
  "Return end position, nil if not inside.

Optional ARG indicates a start-position for `parse-partial-sexp'."
  (interactive)
  (let* ((ppstart (or arg (point-min)))
         (erg (syntax-ppss))
         (beg (nth 1 erg))
         end)
    (when beg
      (save-excursion
        (goto-char beg)
        (forward-list 1)
        (setq end (point))))
    (when (and py-verbose-p (interactive-p)) (message "%s" end))
    end))

(defun py--in-comment-p ()
  "Return the beginning of current line's comment, if inside. "
  (save-restriction
    (widen)
    (let* ((pps (syntax-ppss))
           (erg (when (nth 4 pps) (nth 8 pps))))
      (unless erg
        (when (looking-at (concat "^[ \t]*" comment-start-skip))
          (setq erg (point))))
      erg)))

(defun py-in-triplequoted-string-p ()
  "Returns character address of start tqs-string, nil if not inside. "
  (interactive)
  (let* ((pps (syntax-ppss))
         (erg (when (and (nth 3 pps) (nth 8 pps))(nth 2 pps))))
    (save-excursion
      (unless erg (setq erg
                        (progn
                          (when (looking-at "\"\"\"\\|''''")
                            (goto-char (match-end 0))
                            (setq pps (syntax-ppss))
                            (when (and (nth 3 pps) (nth 8 pps)) (nth 2 pps)))))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-in-string-p ()
  "Returns character address of start of string, nil if not inside. "
  (interactive)
  (let* ((pps (syntax-ppss))
         (erg (when (nth 3 pps) (nth 8 pps))))
    (save-excursion
      (unless erg (setq erg
                        (progn
                          (when (looking-at "\"\\|'")
                            (forward-char 1)
                            (setq pps (syntax-ppss))
                            (when (nth 3 pps) (nth 8 pps)))))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-in-statement-p ()
  "Returns list of beginning and end-position if inside.

Result is useful for booleans too: (when (py-in-statement-p)...)
will work.
"
  (interactive)
  (let ((orig (point))
        beg end erg)
    (save-excursion
      (setq end (py-end-of-statement))
      (setq beg (py-beginning-of-statement))
      (when (and (<= beg orig)(<= orig end))
        (setq erg (cons beg end))
        (when (interactive-p) (message "%s" erg))
        erg))))

;;  Beginning-of- p
(defun py-beginning-of-top-level-p ()
  "Returns position, if cursor is at the beginning of a top-level, nil otherwise. "
  (interactive)
  (let (erg)
    (and (py--beginning-of-statement-p)
         (eq 0 (current-column))
         (setq erg (point))
      erg)))

(defun py--beginning-of-line-p ()
  "Returns position, if cursor is at the beginning of a line, nil otherwise. "
  (when (bolp)(point)))

(defun py--beginning-of-buffer-p ()
  "Returns position, if cursor is at the beginning of buffer, nil otherwise. "
  (when (bobp)(point)))

(defun py--beginning-of-paragraph-p ()
  "Returns position, if cursor is at the beginning of a paragraph, nil otherwise. "
  (let ((orig (point))
        erg)
    (if (and (bolp) (looking-at paragraph-separate))
        (setq erg (point))
      (save-excursion
        (py-end-of-paragraph)
        (py-beginning-of-paragraph)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

;;  End-of- p
(defun py--end-of-line-p ()
  "Returns position, if cursor is at the end of a line, nil otherwise. "
  (when (eolp)(point)))

(defun py--end-of-paragraph-p ()
  "Returns position, if cursor is at the end of a paragraph, nil otherwise. "
  (let ((orig (point))
         erg)
     (if (and (eolp) (looking-at paragraph-separate))
         (setq erg (point))
     (save-excursion
       (py-beginning-of-paragraph)
       (py-end-of-paragraph)
       (when (eq orig (point))
         (setq erg orig)))
       erg)))

;;  Opens
(defun py--statement-opens-block-p (&optional regexp)
  "Return position if the current statement opens a block
in stricter or wider sense.

For stricter sense specify regexp. "
  (let* ((regexp (or regexp py-block-or-clause-re))
         (erg (py--statement-opens-base regexp)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py--statement-opens-base (regexp)
  (let ((orig (point))
        erg)
    (save-excursion
      (back-to-indentation)
      (py-end-of-statement)
      (py-beginning-of-statement)
      (when (and
             (<= (line-beginning-position) orig)(looking-back "^[ \t]*")(looking-at regexp))
        (setq erg (point))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py--statement-opens-clause-p ()
  "Return position if the current statement opens block or clause. "
  (py--statement-opens-base py-clause-re))

(defun py--statement-opens-block-or-clause-p ()
  "Return position if the current statement opens block or clause. "
  (py--statement-opens-base py-block-or-clause-re))

(defun py--statement-opens-class-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (py--statement-opens-base py-class-re))

(defun py--statement-opens-def-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (py--statement-opens-base py-def-re))

(defun py--statement-opens-def-or-class-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (py--statement-opens-base py-def-or-class-re))

(defun py--record-list-error (pps)
  "When encountering a missing parenthesis, store its line, position. `py-verbose-p'  must be t

Unclosed-string errors are not handled here, as made visible by fontification already.
"
  (let ((this-err
         (save-excursion
           (list
            (nth 1 pps)
            (progn
              (goto-char (nth 1 pps))
              (py-count-lines (point-min) (point)))))))
    this-err))

(defun py--message-error (err)
  "Receives a list (position line) "
  (message "Closing paren missed: line %s pos %s" (cadr err) (car err)))

;;  py-look-downward-for-clause
(defun py--end-base (regexp &optional orig decorator)
  "Used internal by functions going to the end forms. "
  (unless (eobp)
    (catch 'exit
      (let* ((orig (or orig (point)))
             (regexp (or regexp 'py-extended-block-or-clause-re))
             (thisregexp
              (cond ((eq regexp 'py-def-or-class-re)
                     py-def-or-class-re)
                    ((eq regexp 'py-def-re)
                     py-def-re)
		    ((eq regexp 'py-class-re)
		     py-class-re)
		    ((eq regexp 'py-minor-block-re)
		     py-minor-block-re)
		    (t py-extended-block-or-clause-re)))
             bofst
             (this (progn (back-to-indentation)
                          (setq bofst (py--beginning-of-statement-p))
                          (cond ((and bofst (eq regexp 'py-clause-re)(looking-at py-extended-block-or-clause-re))
                                 (point))
                                ((and bofst (looking-at thisregexp))
                                 (point))
                                (t
                                 (when
                                     (cdr-safe
                                      (py--go-to-keyword
                                       thisregexp))
                                   (when (py--statement-opens-block-p py-extended-block-or-clause-re)
                                     (point)))))))
             ind erg last pps thisindent done err)
        (cond (this
               (setq thisindent (current-indentation))
               (cond ((and py-close-provides-newline
                           (or (eq regexp 'py-def-re)(eq regexp 'py-class-re)(eq regexp 'py-def-or-class-re)))
                      (while
                          (and
                           ;; lp:1294478 py-mark-def hangs
                           (if last
                               (if (< last (point))
                                   t
                                 (when (nth 1 pps)
                                   (if py-verbose-p
                                       (throw 'exit (py--message-error (py--record-list-error pps)))
                                     (throw 'exit nil))))

                             t)
                           (setq last (point))(re-search-forward "^$" nil t)(skip-chars-forward " \t\r\n\f")(or (nth 8 (setq pps (syntax-ppss))) (nth 1 pps) (< thisindent (current-column)))))
                      ;; (goto-char last)
                      (skip-chars-backward " \t\r\n\f")
                      (setq done t)
                      (and (nth 8 (setq pps (syntax-ppss)))
                           (py-beginning-of-statement)
                           (py-end-of-statement)))
                     (t (while
                            (and (py-down-statement)
                                 (or (< thisindent (current-indentation))
                                     (and (eq thisindent (current-indentation))
                                          (or (eq regexp 'py-minor-block-re)
                                              (eq regexp 'py-block-re))
                                          (looking-at py-clause-re)))
                                 (py-end-of-statement)(setq last (point))))
                        (and last (goto-char last)))))
              (t (goto-char orig)))
        (when (and (<= (point) orig)(not (looking-at thisregexp)))
          ;; found the end above
          ;; py--travel-current-indent will stop of clause at equal indent
          (when (py--look-downward-for-beginning thisregexp)
            (py--end-base regexp orig)))
        (setq pps (syntax-ppss))
        ;; (catch 'exit)
        (and err py-verbose-p (py--message-error err))
        (if (and (< orig (point)) (not (or (looking-at comment-start) (nth 8 pps) (nth 1 pps))))
            (point)
          (goto-char (point-max))
          nil)))))

(defun py--look-downward-for-beginning (regexp)
  "When above any beginning of FORM, search downward. "
  (let* ((orig (point))
         (erg orig)
         (last orig)
         pps)
    (while (and (setq last (point)) (not (eobp)) (re-search-forward regexp nil t 1)(setq erg (match-beginning 0)) (setq pps (syntax-ppss))
                (or (nth 8 pps) (nth 1 pps))))
    (cond ((not (or (nth 8 pps) (nth 1 pps) (or (looking-at comment-start))))
           (when (ignore-errors (< orig erg))
             erg)))))

(defun py-look-downward-for-clause (&optional ind orig regexp)
  "If beginning of other clause exists downward in current block.

If succesful return position. "
  (interactive)
  (unless (eobp)
    (let ((ind (or ind
                   (save-excursion
                     (py-beginning-of-statement)
                     (if (py--statement-opens-block-p)
                         (current-indentation)
                       (- (current-indentation) py-indent-offset)))))
          (orig (or orig (point)))
          (regexp (or regexp py-extended-block-or-clause-re))
          erg last)
      (end-of-line)
      (when (re-search-forward regexp nil t 1)
        (when (nth 8 (syntax-ppss))
          (while (and (re-search-forward regexp nil t 1)
                      (nth 8 (syntax-ppss)))))
        (setq last (point))
        (back-to-indentation)
        (unless (and (looking-at py-clause-re)
                     (not (nth 8 (syntax-ppss))) (eq (current-indentation) ind))
          (progn (setq ind (current-indentation))
                 (while (and (py-end-of-statement-bol)(not (looking-at py-clause-re))(<= ind (current-indentation)))))
          (if (and (looking-at py-clause-re)
                   (not (nth 8 (syntax-ppss)))
                   (< orig (point)))
              (setq erg (point))
            (goto-char orig))))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-current-defun (&optional iact)
  "Go to the outermost method or class definition in current scope.

Python value for `add-log-current-defun-function'.
This tells add-log.el how to find the current function/method/variable.
Returns name of class or methods definition, if found, nil otherwise.

See customizable variables `py-current-defun-show' and `py-current-defun-delay'."
  (interactive "p")
  (save-restriction
    (widen)
    (save-excursion
      (let ((erg (when (py-beginning-of-def-or-class)
                   (forward-word 1)
                   (skip-chars-forward " \t")
                   (prin1-to-string (symbol-at-point)))))
        (when (and erg py-current-defun-show (push-mark (point) t t) (skip-chars-forward "^ (")
                   (exchange-point-and-mark)
                   (sit-for py-current-defun-delay)))
        (when iact (message (prin1-to-string erg)))
        erg))))

(defun py-sort-imports ()
  "Sort multiline imports.

Put point inside the parentheses of a multiline import and hit
\\[py-sort-imports] to sort the imports lexicographically"
  (interactive)
  (save-excursion
    (let ((open-paren (save-excursion (progn (up-list -1) (point))))
          (close-paren (save-excursion (progn (up-list 1) (point))))
          sorted-imports)
      (goto-char (1+ open-paren))
      (skip-chars-forward " \n\t")
      (setq sorted-imports
            (sort
             (delete-dups
              (split-string (buffer-substring
                             (point)
                             (save-excursion (goto-char (1- close-paren))
                                             (skip-chars-backward " \n\t")
                                             (point)))
                            ", *\\(\n *\\)?"))
             ;; XXX Should this sort case insensitively?
             'string-lessp))
      ;; Remove empty strings.
      (delete-region open-paren close-paren)
      (goto-char open-paren)
      (insert "(\n")
      (insert (py--join-words-wrapping (remove "" sorted-imports) "," "    " 78))
      (insert ")"))))

(defun py--in-literal (&optional lim)
  "Return non-nil if point is in a Python literal (a comment or string).
Optional argument LIM indicates the beginning of the containing form,
i.e. the limit on how far back to scan."
  (let* ((lim (or lim (point-min)))
         (state (syntax-ppss)))
    (cond
     ((nth 3 state) 'string)
     ((nth 4 state) 'comment))))

(defun py-count-lines (&optional beg end)
  "Count lines in accessible part until current line.

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=7115"
  (interactive)
  (save-excursion
    (let ((count 0)
          (orig (point))
	  (beg (or beg (point-min)))
	  (end (or end (point))))
      (save-match-data
	(if (or (eq major-mode 'comint-mode)
		(eq major-mode 'py-shell-mode))
	    (if
		(re-search-backward py-fast-filter-re nil t 1)
		(goto-char (match-end 0))
	      (when py-debug-p (message "%s"  "py-count-lines: Don't see a prompt here"))
	      (goto-char beg))
	  (goto-char beg)))
      (while (and (< (point) end)(not (eobp)) (skip-chars-forward "^\n" end))
        (setq count (1+ count))
        (unless (or (not (< (point) end)) (eobp)) (forward-char 1)
                (setq count (+ count (abs (skip-chars-forward "\n" end))))))
      (when (bolp) (setq count (1+ count)))
      (when (and py-debug-p (interactive-p)) (message "%s" count))
      count)))

(defun py-which-function ()
  "Return the name of the function or class, if curser is in, return nil otherwise. "
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let ((orig (point))
            (erg (if (and (looking-at (concat py-def-or-class-re " +\\([^(]+\\)(.+")) (not (py-in-string-or-comment-p)))
                     (match-string-no-properties 2)
                   (progn
                     (py-beginning-of-def-or-class)
                     (when (looking-at (concat py-def-or-class-re " +\\([^(]+\\)(.+"))
                       (match-string-no-properties 2))))))
        (if (and erg (< orig (py-end-of-def-or-class)))
            (when (interactive-p) (message "%s" erg))
          (setq erg nil)
          (when (interactive-p) (message "%s" "Not inside a function or class"))
          erg)))))

(defconst py-help-address "python-mode@python.org"
  "List dealing with usage and developing python-mode.

Also accepts submission of bug reports, whilst a ticket at
http://launchpad.net/python-mode
is preferable for that. ")

;;  Utilities
(defun py--point (position)
  "Returns the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:

  bol -- beginning of line
  eol -- end of line
  bod -- beginning of def or class
  eod -- end of def or class
  bob -- beginning of buffer
  eob -- end of buffer
  boi -- back to indentation
  bos -- beginning of statement

This function does not modify point or mark."
  (let (erg)
    (save-excursion
      (setq erg
            (progn
              (cond
               ((eq position 'bol) (beginning-of-line))
               ((eq position 'eol) (end-of-line))
               ((eq position 'bod) (py-beginning-of-def-or-class))
               ((eq position 'eod) (py-end-of-def-or-class))
               ;; Kind of funny, I know, but useful for py-up-exception.
               ((eq position 'bob) (goto-char (point-min)))
               ((eq position 'eob) (goto-char (point-max)))
               ((eq position 'boi) (back-to-indentation))
               ((eq position 'bos) (py-beginning-of-statement))
               (t (error "Unknown buffer position requested: %s" position))) (point))))
    erg))

(defun py-install-search-local ()
  (interactive)
  (let ((erg (split-string (shell-command-to-string (concat "find " default-directory " -maxdepth 9 -type f -name \"*python\"")))))))

;;  (defun py-install-local-epdfree ()
;;    (interactive)
;;    (py-install-local-shells "MY-PATH/epdfree"))

(defun py-install-local-shells (&optional local path-prefix)
  "Builds Python-shell commands from executable found in LOCAL.

If LOCAL is empty, shell-command `find' searches beneath current directory.
Eval resulting buffer to install it, see customizable `py-extensions'. "
  (interactive)
  (let* ((local-dir (if local
                        (expand-file-name local)
                      (read-from-minibuffer "Virtualenv directory: " default-directory)))
         (path-separator (if (string-match "/" local-dir)
                             "/"
                           "\\" t))
         (shells (split-string (shell-command-to-string (concat "find " local-dir " -maxdepth 9 -type f -executable -name \"*python\""))))
         erg newshell prefix akt end orig curexe aktpath)
    (set-buffer (get-buffer-create py-extensions))
    (erase-buffer)
    (dolist (elt shells)
      (setq prefix "")
      (setq curexe (substring elt (1+ (string-match "/[^/]+$" elt))))
      (setq aktpath (substring elt 0 (1+ (string-match "/[^/]+$" elt))))
      (dolist (prf (split-string aktpath (regexp-quote path-separator)))
        (unless (string= "" prf)
          (setq prefix (concat prefix (substring prf 0 1)))))
      (setq orig (point))
      (insert py-shell-template)
      (setq end (point))
      (goto-char orig)
      (when (re-search-forward "\\<NAME\\>" end t 1)
        (replace-match (concat prefix "-" (substring elt (1+ (save-match-data (string-match "/[^/]+$" elt)))))t))
      (goto-char orig)
      (while (search-forward "DOCNAME" end t 1)
        (replace-match (if (string= "ipython" curexe)
                           "IPython"
                         (capitalize curexe)) t))
      (goto-char orig)
      (when (search-forward "FULLNAME" end t 1)
        (replace-match elt t))
      (goto-char (point-max)))
    (emacs-lisp-mode)
    (if (file-readable-p (concat py-install-directory "/" py-extensions))
        (find-file (concat py-install-directory "/" py-extensions)))))

(defun py-end-of-string (&optional beginning-of-string-position)
  "Go to end of string at point if any, if successful return position. "
  (interactive)
  ;; (when py-debug-p (message "(current-buffer): %s" (current-buffer)))
  ;; (when py-debug-p (message "major-mode): %s" major-mode))
  (let ((orig (point))
	(beginning-of-string-position (or beginning-of-string-position (and (nth 3 (parse-partial-sexp 1 (point)))(nth 8 (parse-partial-sexp 1 (point))))
                                          (and (looking-at "\"\"\"\\|'''\\|\"\\|\'")(match-beginning 0))))
        erg)
    (if beginning-of-string-position
        (progn
          (goto-char beginning-of-string-position)
	  (when
	      ;; work around parse-partial-sexp error
	      (and (nth 3 (parse-partial-sexp 1 (point)))(nth 8 (parse-partial-sexp 1 (point))))
	    (goto-char (nth 3 (parse-partial-sexp 1 (point)))))
          (if (ignore-errors (setq erg (scan-sexps (point) 1)))
			      (goto-char erg)
	    (goto-char orig)))

      (error (concat "py-end-of-string: don't see end-of-string at " (buffer-name (current-buffer)) "at pos " (point))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

;;  (goto-char (match-end 0))
;;  (search-forward (match-string-no-properties 0))))

(defun py--until-found (search-string liste)
  "Search liste for search-string until found. "
  (let ((liste liste) element)
    (while liste
      (if (member search-string (car liste))
          (setq element (car liste) liste nil))
      (setq liste (cdr liste)))
    (when element
      (while (and element (not (numberp element)))
        (if (member search-string (car element))
            (setq element (car element))
          (setq element (cdr element))))
      element)))

;;  (defun py-shell-send-string (string &optional process msg filename)
;;    "Send STRING to Python PROCESS.
;;  When `py-verbose-p' and MSG is non-nil messages the first line of STRING."
;;    (interactive "sPython command: ")
;;    (let* ((process (or process (get-buffer-process (py-shell))))
;;           (lines (split-string string "\n"))
;;           (temp-file-name (concat (with-current-buffer (process-buffer process)
;;                                     (file-remote-p default-directory))
;;                                   (py--normalize-directory py-temp-directory)
;;  				 ;; (md5 (user-login-name))
;;                                   (md5 (concat (user-login-name)(prin1-to-string (current-time))))
;;  				 "-psss-temp.py"))
;;           (file-name (or filename (buffer-file-name) temp-file-name)))
;;      (if (> (length lines) 1)
;;  	(with-temp-file temp-file-name
;;  	  (insert string)
;;  	  (delete-trailing-whitespace)
;;  	  (py-send-file temp-file-name process temp-file-name))
;;        (comint-send-string process string)
;;        (when (or (not (string-match "\n$" string))
;;                  (string-match "\n[ \t].*\n?$" string))
;;          (comint-send-string process "\n")))
;;      (unless py-debug-p (when (file-readable-p temp-file-name)(delete-file temp-file-name)))))

(defun py--delay-process-dependent (process)
  "Call a `py-ipython-send-delay' or `py-python-send-delay' according to process"
  (if (string-match "ipython" (prin1-to-string process))
      (sit-for py-ipython-send-delay t)
    (sit-for py-python-send-delay t)))

(defun py--send-string-no-output (string &optional process msg)
  "Send STRING to PROCESS and inhibit output display.
When MSG is non-nil messages the first line of STRING.  Return
the output."
  (let* (output
         (process (or process (get-buffer-process (py-shell))))
         (comint-preoutput-filter-functions
          (append comint-preoutput-filter-functions
                  '(ansi-color-filter-apply
                    (lambda (string)
                      (setq output string)
                      "")))))
    (py-send-string string process)
    (sit-for 0.1 t)
    ;; (py--delay-process-dependent process)
    (when (and output (not (string= "" output)))
	    (py--string-strip
	     (format "[ \n]*%s[ \n]*" py-fast-filter-re)))))

(defun py--send-string-return-output (string &optional process msg)
  "Send STRING to PROCESS and return output.

When MSG is non-nil messages the first line of STRING.  Return
the output."
  (with-current-buffer (process-buffer process)
    (let* (erg
	   (process (or process (get-buffer-process (py-shell))))
	   (comint-preoutput-filter-functions
	    (append comint-preoutput-filter-functions
		    '(ansi-color-filter-apply
		      (lambda (string)
			(setq erg (concat erg string))
			"")))))
      (py-send-string string process)
      (accept-process-output process 5)
      (sit-for 0.1 t)
      (when (and erg (not (string= "" erg)))
	(setq erg
	      (replace-regexp-in-string
	       (format "[ \n]*%s[ \n]*" py-fast-filter-re)
	       "" erg)))
      ;; (sit-for 0.1 t)
      erg)))

(defun py-which-def-or-class ()
  "Returns concatenated `def' and `class' names in hierarchical order, if cursor is inside.

Returns \"???\" otherwise
Used by variable `which-func-functions' "
  (interactive)
  (let* ((orig (point))
         (first t)
         def-or-class
         done last erg name)
    (and first (looking-at "[ \t]*\\_<\\(def\\|class\\)\\_>[ \n\t]\\([[:alnum:]_]+\\)")(not (nth 8 (syntax-ppss)))
         (add-to-list 'def-or-class (match-string-no-properties 2)))
    (while
        (and (not (bobp)) (not done) (or (< 0 (current-indentation)) first))
      (py-beginning-of-def-or-class)
      (looking-at "[ \t]*\\_<\\(def\\|class\\)\\_>[ \n\t]\\([[:alnum:]_]+\\)")
      (setq last (point))
      (setq name (match-string-no-properties 2))
      (if first
          (progn
            (setq first nil)
            (py-end-of-def-or-class)
            (if
                (<= orig (point))
                (goto-char last)
              (setq done t)
              (goto-char orig)))
        t)
      (unless done (add-to-list 'def-or-class name)))
    (unless done (setq def-or-class (mapconcat 'identity def-or-class ".")))
    (goto-char orig)
    (or def-or-class (setq def-or-class "???"))
    (when (interactive-p) (message "%s" def-or-class))
    def-or-class))

(defun py--beginning-of-form-intern (regexp &optional iact indent orig lc)
  "Go to beginning of FORM.

With INDENT, go to beginning one level above.
Whit IACT, print result in message buffer.

Returns beginning of FORM if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let (erg)
    (unless (bobp)
      (let* ((orig (or orig (point)))
             (indent (or indent (progn
                                  (back-to-indentation)
                                  (or (py--beginning-of-statement-p)
                                      (py-beginning-of-statement))
                                  (current-indentation)))))
        (setq erg (cond ((and (< (point) orig) (looking-at (symbol-value regexp)))
                         (point))
                        ((and (eq 0 (current-column)) (numberp indent) (< 0 indent))
                         (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
                           (py-beginning-of-statement)
                           (unless (looking-at (symbol-value regexp))
                             (cdr (py--go-to-keyword (symbol-value regexp) (current-indentation))))))
                        ((numberp indent)
			 (cdr (py--go-to-keyword (symbol-value regexp) indent)))
                        (t (ignore-errors
                             (cdr (py--go-to-keyword (symbol-value regexp)
                                                    (- (progn (if (py--beginning-of-statement-p) (current-indentation) (save-excursion (py-beginning-of-statement) (current-indentation)))) py-indent-offset)))))))
        (when lc (beginning-of-line) (setq erg (point)))))
    (when (and py-verbose-p iact) (message "%s" erg))
    erg))

(defun py--beginning-of-prepare (indent final-re &optional inter-re iact lc)
  (let ((orig (point))
        (indent
         (or indent
             (progn (back-to-indentation)
                    (or (py--beginning-of-statement-p)
                        (py-beginning-of-statement))
                    (cond ((eq 0 (current-indentation))
                           (current-indentation))
                          ((looking-at (symbol-value inter-re))
                           (current-indentation))
                          (t
                           (if (<= py-indent-offset (current-indentation))
                               (- (current-indentation) (if py-smart-indentation (py-guess-indent-offset) py-indent-offset))
                             py-indent-offset))))))
        erg)
    (if (and (< (point) orig) (looking-at (symbol-value final-re)))
        (progn
          (and lc (beginning-of-line))
          (setq erg (point))
          (when (and py-verbose-p iact) (message "%s" erg))
          erg)
      (py--beginning-of-form-intern final-re iact indent orig lc))))

(defun py--fetch-first-python-buffer ()
  "Returns first (I)Python-buffer found in `buffer-list'"
  (let ((buli (buffer-list))
        erg)
    (while (and buli (not erg))
      (if (string-match "Python" (prin1-to-string (car buli)))
          (setq erg (car buli))
        (setq buli (cdr buli))))
    erg))

(defun py-unload-python-el ()
  "Unloads python-mode delivered by shipped python.el

Removes python-skeleton forms from abbrevs.
These would interfere when inserting forms heading a block"
  (interactive)
  (let (done)
    (when (featurep 'python) (unload-feature 'python t))
    (when (file-readable-p abbrev-file-name)
      (find-file abbrev-file-name)
      (goto-char (point-min))
      (while (re-search-forward "^.+python-skeleton.+$" nil t 1)
	(setq done t)
	(delete-region (match-beginning 0) (1+ (match-end 0))))
      (when done (write-file abbrev-file-name)
	    ;; now reload
	    (read-abbrev-file abbrev-file-name))
      (kill-buffer (file-name-nondirectory abbrev-file-name)))))

(defmacro py--kill-buffer-unconditional (buffer)
  "Kill buffer unconditional, kill buffer-process if existing. "
  `(let ((proc (get-buffer-process ,buffer))
	 kill-buffer-query-functions)
     (ignore-errors
       (and proc (kill-process proc))
       (set-buffer ,buffer)
       (set-buffer-modified-p 'nil)
       (kill-buffer (current-buffer)))))

(defun py--skip-to-semicolon-backward (&optional limit)
  "Fetch the beginning of statement after a semicolon.

Returns position reached if point was moved. "
  (let ((orig (point)))
    (and (< 0 (abs (skip-chars-backward "^;" (or limit (line-beginning-position)))))
	 (skip-chars-forward " \t" (line-end-position))
	 (setq done t)
	 (and (< (point) orig) (point)))))

(defun py--eos-in-string (pps)
  "Return stm, i.e. if string is part of a (print)-statement. "
  (let ((orig (point))
        pos stm)
    (goto-char (nth 8 pps))
    (unless (looking-back "^[ \t]*")
      (setq stm t))
    ;; go to end of string
    (and (member (char-after) (list ?' ?\"))
         (ignore-errors (setq pos (scan-sexps (point) 1)))
         (goto-char pos))
    ;; if no closing string delimiter, pos doesn't exist
    (unless (or stm (not pos))
      (setq done t)
      (unless (eq 10 (char-after))
        (and (< 0 (abs (skip-chars-forward "^;#" (line-end-position))))
             (eq ?\; (char-after))
             (skip-chars-forward ";"))))
    stm))

(defun py--end-of-comment-intern (pos)
  (while (and (not (eobp))
              (forward-comment 99999)))
  ;; forward-comment fails sometimes
  (and (eq pos (point)) (prog1 (forward-line 1) (back-to-indentation))
       (while (member (char-after) (list ?# 10))(forward-line 1)(back-to-indentation))))

(defun py--skip-to-comment-or-semicolon ()
  "Returns position if comment or semicolon found. "
  (let ((orig (point)))
    (cond ((and done (< 0 (abs (skip-chars-forward "^#;" (line-end-position))))
		(member (char-after) (list ?# ?\;)))
	   (when (eq ?\; (char-after))
	     (skip-chars-forward ";" (line-end-position))))
	  ((and (< 0 (abs (skip-chars-forward "^#;" (line-end-position))))
		(member (char-after) (list ?# ?\;)))
	   (when (eq ?\; (char-after))
	     (skip-chars-forward ";" (line-end-position))))
	  ((not done)
	   (end-of-line)))
    (skip-chars-backward " \t" (line-beginning-position))
    (and (< orig (point))(setq done t)
	 done)))

(defalias 'py-beginning-of-top-level-bol 'py-beginning-of-top-level)
(defun py-beginning-of-top-level ()
  "Go up to beginning of statments until level of indentation is null.

Returns position if successful, nil otherwise "
  (interactive)
  (let (erg)
    (unless (bobp)
      (while (and (not (bobp)) (setq erg (py-beginning-of-statement))
                  (< 0 (current-indentation))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defalias 'py-end-of-top-level-bol 'py-end-of-top-level)
(defun py-end-of-top-level ()
  "Go to end of top-level form at point.

Returns position if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((orig (point))
        erg)
    (unless (eobp)
      (unless (py--beginning-of-statement-p)
        (py-beginning-of-statement))
      (unless (eq 0 (current-column))
        (py-beginning-of-top-level))
      (cond ((looking-at py-def-re)
             (setq erg (py-end-of-def)))
            ((looking-at py-class-re)
             (setq erg (py-end-of-class)))
            ((looking-at py-block-re)
             (setq erg (py-end-of-block)))
             (t (setq erg (py-end-of-statement))))
      (unless (< orig (point))
        (while (and (not (eobp)) (py-down-statement)(< 0 (current-indentation))))
        (if (looking-at py-block-re)
            (setq erg (py-end-of-block))
          (setq erg (py-end-of-statement))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defalias 'py-end-of-top-level-lc 'py-end-of-top-level-bol)
(defun py-end-of-top-level-bol ()
  "Go to end of top-level form at point, stop at next beginning-of-line.

Returns position successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let (erg)
    (py-end-of-top-level)
    (unless (or (eobp) (bolp))
      (forward-line 1)
      (beginning-of-line)
      (setq erg (point)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-up (&optional indent)
  "Go up or to beginning of form if inside.

If inside a delimited form --string or list-- go to its beginning.
If not at beginning of a statement or block, go to its beginning.
If at beginning of a statement or block, go to beginning one level above of compound statement or definition at point.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let ((pps (syntax-ppss)))
    (cond ((nth 8 pps) (goto-char (nth 8 pps)))
          ((nth 1 pps) (goto-char (nth 1 pps)))
          ((py--beginning-of-statement-p) (py--beginning-of-form-intern 'py-extended-block-or-clause-re (interactive-p) t))
          (t (py-beginning-of-statement)))))

(defun py-down (&optional indent)

  "Go to beginning one level below of compound statement or definition at point.

If no statement or block below, but a delimited form --string or list-- go to its beginning. Repeated call from there will behave like down-list.

Returns position if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         erg
         (indent (if
                     (py--beginning-of-statement-p)
                     (current-indentation)
                   (progn
                     (py-beginning-of-statement)
                     (current-indentation))))
         last)
    (while (and (setq last (point)) (py-end-of-statement) (py-end-of-statement) (py-beginning-of-statement) (eq (current-indentation) indent)))
    (if (< indent (current-indentation))
        (setq erg (point))
      (goto-char last))
    (when (< (point) orig)
      (goto-char orig))
    (when (and (eq (point) orig)
               (progn (forward-char 1)
                      (skip-chars-forward "^\"'[({" (line-end-position))
                      (member (char-after) (list ?\( ?\" ?\' ?\[ ?\{)))
               (setq erg (point))))
    (unless erg
      (goto-char orig))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py--beginning-of-line-form ()
  "Internal use: Go to beginning of line following end of form. "
  (when erg
    (unless (eobp)
      (forward-line 1)
      (beginning-of-line)
      (setq erg (point)))))

(defun py--mark-base (form &optional py-mark-decorators)
  "Returns boundaries of FORM, a cons.

If PY-MARK-DECORATORS, `def'- and `class'-forms include decorators
If BOL is t, mark from beginning-of-line"
  (let* ((begform (intern-soft (concat "py-beginning-of-" form)))
         (endform (intern-soft (concat "py-end-of-" form)))
         (begcheckform (intern-soft (concat "py--beginning-of-" form "-p")))
         (orig (point))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (and py-mark-decorators
         (and (setq erg (py-beginning-of-decorator))
              (setq beg erg)))
    (push-mark)
    (setq end (funcall endform))
    (unless end (when (< beg (point))
                  (setq end (point))))
    (if (and beg end (<= beg orig) (<= orig end))
	(progn
	  (when (interactive-p) (message "%s %s" beg end))
	  (cons beg end))
      (when (interactive-p) (message "%s" "nil"))
      ;; (goto-char orig)
      nil)))

(defun py--mark-base-bol (form &optional py-mark-decorators)
  (let* ((begform (intern-soft (concat "py-beginning-of-" form "-bol")))
         (endform (intern-soft (concat "py-end-of-" form "-bol")))
         (begcheckform (intern-soft (concat "py--beginning-of-" form "-bol-p")))
         (orig (point))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (when py-mark-decorators
      (save-excursion
        (when (setq erg (py-beginning-of-decorator-bol))
          (setq beg erg))))
    (setq end (funcall endform))
    (push-mark beg t t)
    (unless end (when (< beg (point))
                  (setq end (point))))
    (when (interactive-p) (message "%s %s" beg end))
    (cons beg end)))

(defun py-mark-base (form &optional py-mark-decorators)
  "Calls py--mark-base, returns bounds of form, a cons. "
  (let* ((bounds (py--mark-base form py-mark-decorators))
         (beg (car bounds)))
    (push-mark beg t t)
    bounds))

(defun py-beginning (&optional indent)
 "Go to beginning of compound statement or definition at point.

With \\[universal-argument], go to beginning one level above.
Returns position if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (py--beginning-of-form-intern py-extended-block-or-clause-re (interactive-p) indent))

(defun py-end (&optional indent)
 "Go to end of of compound statement or definition at point.

Returns position block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
    (let* ((orig (point))
           (erg (py--end-base 'py-extended-block-or-clause-re orig)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg))

;;  Buffer
(defun py-beginning-of-buffer ()
  "Go to beginning-of-buffer, return position. "
  (let ((erg (unless (bobp)
               (goto-char (point-min)))))
    erg))

(defun py-end-of-buffer ()
  "Go to end-of-buffer, return position.

  If already at end-of-buffer and not at EOB, go to end of next line. "
  (let ((erg (unless (eobp)
               (goto-char (point-max)))))
    erg))

(defun py-backward-same-level ()
  "Go form backward keeping indent level if possible.

If inside a delimited form --string or list-- go to its beginning.
If not at beginning of a statement or block, go to its beginning.
If at beginning of a statement or block, go to previous beginning of compound statement or definition at point.
If no further element at same level, go one level up.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((pps (syntax-ppss)))
    (cond ((nth 8 pps) (goto-char (nth 8 pps)))
          ((nth 1 pps) (goto-char (nth 1 pps)))
          ((py--beginning-of-statement-p) (py--beginning-of-form-intern 'py-extended-block-or-clause-re (interactive-p)))
          (t (py-beginning-of-statement)))))

(defun py--end-of-line-p ()
  "Returns position, if cursor is at the end of a line, nil otherwise. "
  (when (eolp)(point)))

(defun py--end-of-buffer-p ()
  "Returns position, if cursor is at the end of buffer, nil otherwise. "
  (when (eobp)(point)))

(defun py--bounds-of-region ()
  "Returns bounds of region at point.

Returns a list, whose car is beg, cdr - end."
  (save-excursion
    (save-restriction
      (widen)
      (let ((beg (region-beginning))
            (end (region-end)))
        (if (and beg end)
            (when (interactive-p) (message "%s" (list beg end)))
          (list beg end))))))

(defun py--beginning-of-paragraph-position ()
  "Returns beginning of paragraph position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
		 (py-beginning-of-paragraph)
		 (point))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--end-of-paragraph-position ()
  "Returns end of paragraph position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
                 (when (looking-at "[ \\t\\r\\n\\f]*$")
                   (skip-chars-backward " \t\r\n\f")
                   (forward-char -1))
                 (py-end-of-paragraph)
		 (point))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-comment-position ()
  "Returns beginning of comment position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-comment)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--end-of-comment-position ()
  "Returns end of comment position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
                 (when (looking-at "[ \\t\\r\\n\\f]*$")
                   (skip-chars-backward " \t\r\n\f")
                   (forward-char -1))
                 (py-end-of-comment))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

;; from gud.el
;; (defun pdb (command-line)
;;   "Run pdb on program FILE in buffer `*gud-FILE*'.
;; The directory containing FILE becomes the initial working directory
;; and source-file directory for your debugger."
;;   (interactive
;;    (list (gud-query-cmdline 'pdb)))

(defun py--pdb-versioned ()
  "Guess existing pdb version from py-shell-name

Return \"pdb[VERSION]\" if executable found, just \"pdb\" otherwise"
  (interactive)
  (let ((erg (when (string-match "[23]" py-shell-name)
	       ;; versions-part
	       (substring py-shell-name (string-match "[23]" py-shell-name)))))
    (if erg
      (cond ((executable-find (concat "pdb" erg))
	     (concat "pdb" erg))
	    ((and (string-match "\\." erg)
		  (executable-find (concat "pdb" (substring erg 0 (string-match "\\." erg)))))
	     (concat "pdb" (substring erg 0 (string-match "\\." erg)))))
      "pdb")))

(defun py-pdb (command-line)
  "Run pdb on program FILE in buffer `*gud-FILE*'.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

At GNU Linux systems required pdb version should be detected by `py--pdb-version', at Windows configure `py-python-ms-pdb-command'

lp:963253"
  (interactive
   (list (gud-query-cmdline
	  (if (or (eq system-type 'ms-dos)(eq system-type 'windows-nt))
	      (car (read-from-string py-python-ms-pdb-command))
	    ;; sys.version_info[0]
	    (car (read-from-string (py--pdb-version)))) "asdf")))
  (pdb command-line (buffer-file-name)))

;; /usr/lib/python2.7/pdb.py eyp.py

(defalias 'py-forward-block 'py-end-of-block)
(defalias 'py-forward-block-or-clause 'py-end-of-block-or-clause)
(defalias 'py-forward-class 'py-end-of-class)
(defalias 'py-forward-clause 'py-end-of-clause)
(defalias 'end-of-def-or-class 'py-end-of-def-or-class)
(defalias 'py-forward-def-or-class 'py-end-of-def-or-class)
(defalias 'py-previous-block 'py-beginning-of-block)
(defalias 'py-goto-block-up 'py-beginning-of-block)
(defalias 'py-backward-block 'py-beginning-of-block)
(defalias 'py-previous-block-or-clause 'py-beginning-of-block-or-clause)
(defalias 'py-goto-block-or-clause-up 'py-beginning-of-block-or-clause)
(defalias 'py-backward-block-or-clause 'py-beginning-of-block-or-clause)
(defalias 'beginning-of-class 'py-beginning-of-class)
(defalias 'py-backward-class 'py-beginning-of-class)
(defalias 'py-previous-class 'py-beginning-of-class)
(defalias 'py-previous-clause 'py-beginning-of-clause)
(defalias 'py-goto-clause-up 'py-beginning-of-clause)
(defalias 'py-backward-clause 'py-beginning-of-clause)
(defalias 'py-backward-def-or-class 'py-beginning-of-def-or-class)
(defalias 'py-previous-def-or-class 'py-beginning-of-def-or-class)
(defalias 'py-goto-beyond-block 'py-end-of-block-bol)
(defalias 'py-goto-beyond-final-line 'py-end-of-statement-bol)
(defalias 'py-kill-minor-expression 'py-kill-partial-expression)

(defalias 'Python 'python)
(defalias 'pyhotn 'python)
(defalias 'pyhton 'python)
(defalias 'pyt 'python)
(defalias 'Python2 'python2)
(defalias 'Python3 'python3)
(defalias 'IPython 'ipython)
(defalias 'Ipython 'ipython)
(defalias 'iyp 'ipython)
(defalias 'ipy 'ipython)

(provide 'python-components-intern)
;;;  python-components-intern.el ends here
