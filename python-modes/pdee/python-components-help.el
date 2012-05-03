;;; python-components-help.el --- help functions

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
(require 'python-components-macros) 

(defun py-warn-tmp-files-left ()
  "Detect and warn about file of form \"py11046IoE\" in py-temp-directory. "
  (let ((erg1 (file-readable-p (concat py-temp-directory (py-separator-char)  (car (directory-files  py-temp-directory nil "py[[:alnum:]]+$"))))))
    (when erg1
      (message "py-warn-tmp-files-left: %s ?" (concat py-temp-directory (py-separator-char) (car (directory-files  py-temp-directory nil "py[[:alnum:]]*$")))))))

(defun py-fetch-docu ()
  "Lookup in current buffer for the doku for the symbol at point.

Useful for newly defined symbol, not known to python yet. "
  (interactive)
  (let* ((symb (prin1-to-string (symbol-at-point)))
         (args (py-expression))
         erg)
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward (concat py-def-or-class-re " *" symb) nil (quote move) 1)
        (forward-line 1)
        (when (looking-at "[ \t]*\"\"\"\\|[ \t]*'''\\|[ \t]*'[^]+\\|[ \t]*\"[^\"]+")
          (goto-char (match-end 0))
          (setq erg (buffer-substring-no-properties (match-beginning 0) (re-search-forward "\"\"\"\\|'''" nil 'move)))
          (when erg
            (set-buffer (get-buffer-create "*Python-Help*"))
            (erase-buffer)
            (when (interactive-p) (switch-to-buffer (current-buffer)))
            (insert erg)))))))

(defun ar-py-find-imports ()
  (let* (imports
         (erg
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward
                    "^import *[A-Za-z_][A-Za-z_0-9].*\\|^from +[A-Za-z_][A-Za-z_0-9]+ +import .*" nil t)
              (setq imports
                    (concat
                     imports
                     (buffer-substring-no-properties (match-beginning 0) (match-end 0)) "\n"))))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-help-at-point 'py-describe-symbol)
(defun py-describe-symbol ()
  "Print help on symbol at point. "
  (interactive)
  (let* ((sym (prin1-to-string (symbol-at-point)))
         (origfile (buffer-file-name))
         (temp (make-temp-name (buffer-name)))
         (file (concat (expand-file-name temp py-temp-directory) ".py"))
         (cmd (py-find-imports))
         (no-quotes (save-excursion
                      (skip-chars-backward "A-Za-z_0-9.")
                      (and (looking-at "[A-Za-z_0-9.]+")
                           (string-match "\\." (match-string-no-properties 0))))))
    (setq cmd (concat "import pydoc\n"
                      cmd))
    (if no-quotes
        (setq cmd (concat cmd
                          "try: pydoc.help(" sym ")\n"))
      (setq cmd (concat cmd "try: pydoc.help('" sym "')\n")))
    (setq cmd (concat cmd
                      "except:
    print 'No help available on:', \"" sym "\""))
    (with-temp-buffer
      (insert cmd)
      (write-file file))
    (py-process-file file "*Python-Help*")
    (when (file-readable-p file)
      (delete-file file))))


;; Documentation functions

;; dump the long form of the mode blurb; does the usual doc escapes,
;; plus lines of the form ^[vc]:name\$ to suck variable & command docs
;; out of the right places, along with the keys they're on & current
;; values

(defun py-dump-help-string (str)
  (with-output-to-temp-buffer "*Help*"
    (let ((locals (buffer-local-variables))
          funckind funcname func funcdoc
          (start 0) mstart end
          keys)
      (while (string-match "^%\\([vc]\\):\\(.+\\)\n" str start)
        (setq mstart (match-beginning 0) end (match-end 0)
              funckind (substring str (match-beginning 1) (match-end 1))
              funcname (substring str (match-beginning 2) (match-end 2))
              func (intern funcname))
        (princ (substitute-command-keys (substring str start mstart)))
        (cond
         ((equal funckind "c")          ; command
          (setq funcdoc (documentation func)
                keys (concat
                      "Key(s): "
                      (mapconcat 'key-description
                                 (where-is-internal func python-mode-map)
                                 ", "))))
         ((equal funckind "v")          ; variable
          (setq funcdoc (documentation-property func 'variable-documentation)
                keys (if (assq func locals)
                         (concat
                          "Local/Global values: "
                          (prin1-to-string (symbol-value func))
                          " / "
                          (prin1-to-string (default-value func)))
                       (concat
                        "Value: "
                        (prin1-to-string (symbol-value func))))))
         (t                             ; unexpected
          (error "Error in py-dump-help-string, tag `%s'" funckind)))
        (princ (format "\n-> %s:\t%s\t%s\n\n"
                       (if (equal funckind "c") "Command" "Variable")
                       funcname keys))
        (princ funcdoc)
        (terpri)
        (setq start end))
      (princ (substitute-command-keys (substring str start))))
    (if (featurep 'xemacs) (print-help-return-message)
      (help-print-return-message))
    ))

(defun py-describe-mode ()
  "Dump long form of `python-mode' docs."
  (interactive)
  (py-dump-help-string "Major mode for editing Python files.
Knows about Python indentation, tokens, comments and continuation lines.
Paragraphs are separated by blank lines only.

Major sections below begin with the string `@'; specific function and
variable docs begin with `->'.

@EXECUTING PYTHON CODE

\\[py-execute-import-or-reload]\timports or reloads the file in the Python interpreter
\\[py-execute-buffer]\tsends the entire buffer to the Python interpreter
\\[py-execute-region]\tsends the current region
\\[py-execute-def-or-class]\tsends the current function or class definition
\\[py-execute-string]\tsends an arbitrary string
\\[py-shell]\tstarts a Python interpreter window; this will be used by
\tsubsequent Python execution commands
%c:py-execute-import-or-reload
%c:py-execute-buffer
%c:py-execute-region
%c:py-execute-def-or-class
%c:py-execute-string
%c:py-shell

@VARIABLES

py-install-directory\twherefrom `python-mode' looks for extensions
py-indent-offset\tindentation increment
py-block-comment-prefix\tcomment string used by comment-region

py-shell-name\tshell command to invoke Python interpreter
py-temp-directory\tdirectory used for temp files (if needed)

py-beep-if-tab-change\tring the bell if tab-width is changed
%v:py-install-directory
%v:py-indent-offset
%v:py-block-comment-prefix
%v:py-shell-name
%v:py-temp-directory
%v:py-beep-if-tab-change

@KINDS OF LINES

Each physical line in the file is either a `continuation line' (the
preceding line ends with a backslash that's not part of a comment, or
the paren/bracket/brace nesting level at the start of the line is
non-zero, or both) or an `initial line' (everything else).

An initial line is in turn a `blank line' (contains nothing except
possibly blanks or tabs), a `comment line' (leftmost non-blank
character is `#'), or a `code line' (everything else).

Comment Lines

Although all comment lines are treated alike by Python, Python mode
recognizes two kinds that act differently with respect to indentation.

An `indenting comment line' is a comment line with a blank, tab or
nothing after the initial `#'.  The indentation commands (see below)
treat these exactly as if they were code lines: a line following an
indenting comment line will be indented like the comment line.  All
other comment lines (those with a non-whitespace character immediately
following the initial `#') are `non-indenting comment lines', and
their indentation is ignored by the indentation commands.

Indenting comment lines are by far the usual case, and should be used
whenever possible.  Non-indenting comment lines are useful in cases
like these:

\ta = b # a very wordy single-line comment that ends up being
\t #... continued onto another line

\tif a == b:
##\t\tprint 'panic!' # old code we've `commented out'
\t\treturn a

Since the `#...' and `##' comment lines have a non-whitespace
character following the initial `#', Python mode ignores them when
computing the proper indentation for the next line.

Continuation Lines and Statements

The `python-mode' commands generally work on statements instead of on
individual lines, where a `statement' is a comment or blank line, or a
code line and all of its following continuation lines (if any)
considered as a single logical unit.  The commands in this mode
generally (when it makes sense) automatically move to the start of the
statement containing point, even if point happens to be in the middle
of some continuation line.

@INDENTATION

Primarily for entering new code:
\t\\[indent-for-tab-command]\t indent line appropriately
\t\\[py-newline-and-indent]\t insert newline, then indent
\t\\[py-electric-backspace]\t reduce indentation, or delete single character

Primarily for reindenting existing code:
\t\\[py-guess-indent-offset]\t guess py-indent-offset from file content; change locally
\t\\[universal-argument] \\[py-guess-indent-offset]\t ditto, but change globally

\t\\[py-indent-region]\t reindent region to match its context
\t\\[py-shift-left]\t shift line or region left by py-indent-offset
\t\\[py-shift-right]\t shift line or region right by py-indent-offset

Unlike most programming languages, Python uses indentation, and only
indentation, to specify block structure.  Hence the indentation supplied
automatically by `python-mode' is just an educated guess:  only you know
the block structure you intend, so only you can supply correct
indentation.

The \\[indent-for-tab-command] and \\[py-newline-and-indent] keys try to suggest plausible indentation, based on
the indentation of preceding statements.  E.g., assuming
py-indent-offset is 4, after you enter
\tif a > 0: \\[py-newline-and-indent]
the cursor will be moved to the position of the `_' (_ is not a
character in the file, it's just used here to indicate the location of
the cursor):
\tif a > 0:
\t _
If you then enter `c = d' \\[py-newline-and-indent], the cursor will move
to
\tif a > 0:
\t c = d
\t _
`python-mode' cannot know whether that's what you intended, or whether
\tif a > 0:
\t c = d
\t_
was your intent.  In general, `python-mode' either reproduces the
indentation of the (closest code or indenting-comment) preceding
statement, or adds an extra py-indent-offset blanks if the preceding
statement has `:' as its last significant (non-whitespace and non-
comment) character.  If the suggested indentation is too much, use
\\[py-electric-backspace] to reduce it.

Continuation lines are given extra indentation.  If you don't like the
suggested indentation, change it to something you do like, and Python-
mode will strive to indent later lines of the statement in the same way.

If a line is a continuation line by virtue of being in an unclosed
paren/bracket/brace structure (`list', for short), the suggested
indentation depends on whether the current line contains the first item
in the list.  If it does, it's indented py-indent-offset columns beyond
the indentation of the line containing the open bracket.  If you don't
like that, change it by hand.  The remaining items in the list will mimic
whatever indentation you give to the first item.

If a line is a continuation line because the line preceding it ends with
a backslash, the third and following lines of the statement inherit their
indentation from the line preceding them.  The indentation of the second
line in the statement depends on the form of the first (base) line:  if
the base line is an assignment statement with anything more interesting
than the backslash following the leftmost assigning `=', the second line
is indented two columns beyond that `='.  Else it's indented to two
columns beyond the leftmost solid chunk of non-whitespace characters on
the base line.

Warning:  indent-region should not normally be used!  It calls \\[indent-for-tab-command]
repeatedly, and as explained above, \\[indent-for-tab-command] can't guess the block
structure you intend.
%c:indent-for-tab-command
%c:py-newline-and-indent
%c:py-electric-backspace

The next function may be handy when editing code you didn't write:
%c:py-guess-indent-offset

The remaining `indent' functions apply to a region of Python code.  They
assume the block structure (equals indentation, in Python) of the region
is correct, and alter the indentation in various ways while preserving
the block structure:
%c:py-indent-region
%c:py-shift-left
%c:py-shift-right

@MARKING & MANIPULATING REGIONS OF CODE

\\[py-mark-block]\t mark block of lines
\\[py-mark-def-or-class]\t mark smallest enclosing def
\\[universal-argument] \\[py-mark-def-or-class]\t mark smallest enclosing class
\\[comment-region]\t comment out region of code
\\[universal-argument] \\[comment-region]\t uncomment region of code
%c:py-mark-block
%c:py-mark-def-or-class
%c:comment-region

@MOVING POINT

\\[py-previous-statement]\t move to statement preceding point
\\[py-next-statement]\t move to statement following point
\\[py-goto-block-up]\t move up to start of current block
\\[py-beginning-of-def-or-class]\t move to start of def
\\[universal-argument] \\[py-beginning-of-def-or-class]\t move to start of class
\\[py-end-of-def-or-class]\t move to end of def
\\[universal-argument] \\[py-end-of-def-or-class]\t move to end of class

The first two move to one statement beyond the statement that contains
point.  A numeric prefix argument tells them to move that many
statements instead.  Blank lines, comment lines, and continuation lines
do not count as `statements' for these commands.  So, e.g., you can go
to the first code statement in a file by entering
\t\\[beginning-of-buffer]\t to move to the top of the file
\t\\[py-next-statement]\t to skip over initial comments and blank lines
Or do `\\[py-previous-statement]' with a huge prefix argument.
%c:py-previous-statement
%c:py-next-statement
%c:py-goto-block-up
%c:py-beginning-of-def-or-class
%c:py-end-of-def-or-class

@LITTLE-KNOWN EMACS COMMANDS PARTICULARLY USEFUL IN PYTHON MODE

`\\[indent-new-comment-line]' is handy for entering a multi-line comment.

`\\[set-selective-display]' with a `small' prefix arg is ideally suited for viewing the
overall class and def structure of a module.

`\\[back-to-indentation]' moves point to a line's first non-blank character.

`\\[indent-relative]' is handy for creating odd indentation.

@OTHER EMACS HINTS

If you don't like the default value of a variable, change its value to
whatever you do like by putting a `setq' line in your .emacs file.
E.g., to set the indentation increment to 4, put this line in your
.emacs:
\t(setq py-indent-offset 4)
To see the value of a variable, do `\\[describe-variable]' and enter the variable
name at the prompt.

When entering a key sequence like `C-c C-n', it is not necessary to
release the CONTROL key after doing the `C-c' part -- it suffices to
press the CONTROL key, press and release `c' (while still holding down
CONTROL), press and release `n' (while still holding down CONTROL), &
then release CONTROL.

Entering Python mode calls with no arguments the value of the variable
`python-mode-hook', if that value exists and is not nil; for backward
compatibility it also tries `py-mode-hook'; see the `Hooks' section of
the Elisp manual for details.

Obscure:  When python-mode is first loaded, it looks for all bindings
to newline-and-indent in the global keymap, and shadows them with
local bindings to py-newline-and-indent."))

;; (require 'info-look)
;; The info-look package does not always provide this function (it
;; appears this is the case with XEmacs 21.1)
(when (fboundp 'info-lookup-maybe-add-help)
  (info-lookup-maybe-add-help
   :mode 'python-mode
   :regexp "[a-zA-Z0-9_]+"
   :doc-spec '(("(python-lib)Module Index")
               ("(python-lib)Class-Exception-Object Index")
               ("(python-lib)Function-Method-Variable Index")
               ("(python-lib)Miscellaneous Index"))))

;; Find function stuff, lifted from python.el

(defvar python-imports nil
  "Set by `python-find-imports'.")
(make-variable-buffer-local 'python-imports)

(defun ar-py-find-function (name)
  "Find source of definition of function NAME.
Interactively, prompt for name."
  (interactive
   (let* ((symbol (with-syntax-table py-dotted-expression-syntax-table
                    (current-word)))
          (enable-recursive-minibuffers t)

          (list (read-string (if symbol
                                 (format "Find location of (default %s): " symbol)
                               "Find location of: ")
                             nil nil symbol))
          (sourcefile (py-send-string (concat "inspect.getsourcefile (inspect.getmodule (" symbol ")))")))
          (sourceline (py-send-string (concat "inspect.getsourcelines (" symbol ")))")))))))

;; (let* ((loc (py-send-receive (format "emacs.location_of (%S, %s)"
;;       				   name python-imports)))
;;        (loc (car (read-from-string loc)))
;;        (file (car loc))
;;        (line (cdr loc)))
;;   (unless file (error "Don't know where `%s' is defined" name))
;;   (pop-to-buffer (find-file-noselect file))
;;   (when (integerp line)
;;     (goto-char (point-min))
;;     (forward-line (1- line)))))

;; (python-find-template "#! /bin/env python
;;  # -*- coding: utf-8 -*-
;;
;; def location_of (name, imports):
;;     \"\"\"Get location at which NAME is defined (or nil).
;;     Provides a pair (PATH, LINE), where LINE is the start of the definition
;;     in path name PATH.
;;     Exec IMPORTS first.\"\"\"
;;     locls = {}
;;     if imports:
;;         try: execit (imports, locls)
;;         except: pass
;;     try:
;;         obj = eval (name, globals (), locls)
;;         # Bug: (in Python 2.5) `getsourcefile' only works with modules,
;;         # hence the `getmodule' here.
;;         srcfile = inspect.getsourcefile (inspect.getmodule (obj))
;;         _, line = inspect.getsourcelines (obj)
;;         printit ('_emacs_out (\"%s\" . %d)' % (srcfile, line))
;;     except:
;;         printit (\"_emacs_out ()\")
;; ")

(defun py-find-function (name)
  "Find source of definition of function NAME.

Interactively, prompt for name."
  (interactive
   (let ((symbol (with-syntax-table py-dotted-expression-syntax-table
		   (current-word)))
	 (enable-recursive-minibuffers t))
     (list (read-string (if symbol
			    (format "Find location of (default %s): " symbol)
			  "Find location of: ")
			nil nil symbol))))
  (unless python-imports
    (error "Not called from buffer visiting Python file"))
  (let* ((loc (py-send-receive (format "emacs.location_of (%S, %s)"
                                       name python-imports)))
	 (loc (car (read-from-string loc)))
	 (file (car loc))
	 (line (cdr loc)))
    (unless file (error "Don't know where `%s' is defined" name))
    (pop-to-buffer (find-file-noselect file))
    (when (integerp line)
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun py-find-imports ()
  "Find top-level imports, updating `python-imports'."
  (interactive)
  (save-excursion
    (let (lines)
      (goto-char (point-min))
      (while (re-search-forward "^import\\>\\|^from\\>" nil t)
        (unless (syntax-ppss-context (syntax-ppss))
          (let ((start (line-beginning-position)))
            ;; Skip over continued lines.
            (while (and (eq ?\\ (char-before (line-end-position)))
                        (= 0 (forward-line 1)))
              t)
            (push (buffer-substring start (line-beginning-position 2))
                  lines))))
      (setq python-imports
            (if lines
                (apply #'concat
                       (nreverse lines))
              "None"))
      (when lines
        (set-text-properties 0 (length python-imports) nil python-imports)
        ;; The output ends up in the wrong place if the string we
        ;; send contains newlines (from the imports).
        (setq python-imports
              (replace-regexp-in-string "\n" "\\n"
                                        (format "%S" python-imports) t t)))))
  (when (interactive-p) (message "%s" (car (read-from-string python-imports))))
  python-imports)

(defun py-update-imports ()
  "Returns `python-imports'.

Imports done are displayed in message buffer. "
  (interactive)
  (save-excursion
    (let ((oldbuf (current-buffer))
          (orig (point))
          erg)
      (mapc 'py-execute-string (split-string (car (read-from-string (py-find-imports))) "\n" t))
      (setq erg (car (read-from-string python-imports)))
      (set-buffer oldbuf)
      (goto-char orig)
      (when (interactive-p)
        (switch-to-buffer (current-buffer))
        (message "%s" erg))
      erg)))

;;; Pychecker
;; hack for GNU Emacs
;; (unless (fboundp 'read-shell-command)
;; (defalias 'read-shell-command 'read-string))

(defun py-pychecker-run (command)
  "*Run pychecker (default on the file currently visited)."
  (interactive
   (let ((default
           (if (buffer-file-name)
               (format "%s %s %s" py-pychecker-command
                       (mapconcat 'identity py-pychecker-command-args " ")
                       (buffer-file-name))
             (format "%s %s" py-pychecker-command
                     (mapconcat 'identity py-pychecker-command-args " "))))
         (last (when py-pychecker-history
                 (let* ((lastcmd (car py-pychecker-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (buffer-file-name) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pychecker like this: "
                              (if last
                                  last
                                default)
                              'py-pychecker-history)
        (read-string "Run pychecker like this: "
                     (if last
                         last
                       default)
                     'py-pychecker-history)))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

;; the python-el way
(defcustom python-check-command "pychecker --stdlib"
  "Command used to check a Python file."
  :type 'string
  :group 'python)

(defvar python-saved-check-command nil
  "Internal use.")

;; After `sgml-validate-command'.
(defun python-check (command)
  "Check a Python file (default current buffer's file).
Runs COMMAND, a shell command, as if by `compile'.
See `python-check-command' for the default."
  (interactive
   (list (read-string "Checker command: "
		      (or python-saved-check-command
			  (concat python-check-command " "
				  (let ((name (buffer-file-name)))
				    (if name
					(file-name-nondirectory name))))))))
  (setq python-saved-check-command command)
  (require 'compile)                    ;To define compilation-* variables.
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((compilation-error-regexp-alist
	 (cons '("(\\([^,]+\\), line \\([0-9]+\\))" 1 2)
	       compilation-error-regexp-alist)))
    (compilation-start command)))

;;; from string-strip.el --- Strip CHARS from STRING

;; (setq strip-chars-before  "[ \t\r\n]*")
(defcustom strip-chars-before  "[ \t\r\n]*"
  "Regexp indicating which chars shall be stripped before STRING - which is defined by `string-chars-preserve'."

  :type 'string
  :group 'convenience)

;; (setq strip-chars-after  "[ \t\r\n]*")
(defcustom strip-chars-after  "[ \t\r\n]*\\'"
  "Regexp indicating which chars shall be stripped after STRING - which is defined by `string-chars-preserve'."

  :type 'string
  :group 'convenience)

(defcustom string-chars-preserve "\\(.*?\\)"
  "Chars preserved of STRING.
`strip-chars-after' and
`strip-chars-before' indicate what class of chars to strip."
  :type 'string
  :group 'convenience)

(defun string-strip (str &optional chars-before chars-after chars-preserve)
  "Return a copy of STR, CHARS removed.
`CHARS-BEFORE' and `CHARS-AFTER' default is \"[ \t\r\n]*\",
i.e. spaces, tabs, carriage returns, newlines and newpages.
`CHARS-PRESERVE' must be a parentized expression,
it defaults to \"\\(.*?\\)\""
  (let ((s-c-b (or chars-before
                   strip-chars-before))
        (s-c-a (or chars-after
                   strip-chars-after))
        (s-c-p (or chars-preserve
                   string-chars-preserve)))
    (string-match
     (concat "\\`[" s-c-b"]*" s-c-p "[" s-c-a "]*\\'") str)
    (match-string 1 str)))

(provide 'python-components-help)
;;; python-components-help.el ends here
