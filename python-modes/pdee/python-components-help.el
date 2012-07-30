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

(defun python-eldoc-function ()
  "`eldoc-documentation-function' for Python.
Only works when point is in a function name, not its arg list, for
instance.  Assumes an inferior Python is running."
  (let ((symbol (with-syntax-table python-dotty-syntax-table
                  (current-word))))
    ;; This is run from timers, so inhibit-quit tends to be set.
    (with-local-quit
      ;; First try the symbol we're on.
      (or (and symbol
               (py-send-receive (format "emacs.eargs(%S, %s)"
                                            symbol python-imports)))
          ;; Try moving to symbol before enclosing parens.
          (let ((s (syntax-ppss)))
            (unless (zerop (car s))
              (when (eq ?\( (char-after (nth 1 s)))
                (save-excursion
                  (goto-char (nth 1 s))
                  (skip-syntax-backward "-")
                  (let ((point (point)))
                    (skip-chars-backward "a-zA-Z._")
                    (if (< (point) point)
                        (python-send-receive
                         (format "emacs.eargs(%S, %s)"
                                 (buffer-substring-no-properties (point) point)
                                 python-imports))))))))))))

;;; Info-look functionality.

(declare-function info-lookup-maybe-add-help "info-look" (&rest arg))

(defun python-after-info-look ()
  "Set up info-look for Python.
Used with `eval-after-load'."
  (let* ((version (let ((s (shell-command-to-string (concat py-shell-name
                                                            " -V"))))
                    (string-match "^Python \\([0-9]+\\.[0-9.]+\\_>\\)" s)
                    (match-string 1 s)))
         ;; Whether info files have a Python version suffix, e.g. in Debian.
         (versioned
          (with-temp-buffer
            (with-no-warnings (Info-mode))
            (condition-case ()
                ;; Don't use `info' because it would pop-up a *info* buffer.
                (with-no-warnings
                  (Info-goto-node (format "(python%s-lib)Miscellaneous Index"
                                          version))
                  t)
              (error nil)))))
    (info-lookup-maybe-add-help
     :mode 'python-mode
     :regexp "[[:alnum:]_]+"
     :doc-spec
     ;; Fixme: Can this reasonably be made specific to indices with
     ;; different rules?  Is the order of indices optimal?
     ;; (Miscellaneous in -ref first prefers lookup of keywords, for
     ;; instance.)
     (if versioned
         ;; The empty prefix just gets us highlighted terms.
         `((,(concat "(python" version "-ref)Miscellaneous Index") nil "")
           (,(concat "(python" version "-ref)Module Index" nil ""))
           (,(concat "(python" version "-ref)Function-Method-Variable Index"
                     nil ""))
           (,(concat "(python" version "-ref)Class-Exception-Object Index"
                     nil ""))
           (,(concat "(python" version "-lib)Module Index" nil ""))
           (,(concat "(python" version "-lib)Class-Exception-Object Index"
                     nil ""))
           (,(concat "(python" version "-lib)Function-Method-Variable Index"
                     nil ""))
           (,(concat "(python" version "-lib)Miscellaneous Index" nil "")))
       '(("(python-ref)Miscellaneous Index" nil "")
         ("(python-ref)Module Index" nil "")
         ("(python-ref)Function-Method-Variable Index" nil "")
         ("(python-ref)Class-Exception-Object Index" nil "")
         ("(python-lib)Module Index" nil "")
         ("(python-lib)Class-Exception-Object Index" nil "")
         ("(python-lib)Function-Method-Variable Index" nil "")
         ("(python-lib)Miscellaneous Index" nil ""))))))

(eval-after-load "info-look" '(python-after-info-look))

(defun py-warn-tmp-files-left ()
  "Detect and warn about file of form \"py11046IoE\" in py-temp-directory. "
  (let ((erg1 (file-readable-p (concat py-temp-directory (char-to-string py-separator-char)  (car (directory-files  py-temp-directory nil "py[[:alnum:]]+$"))))))
    (when (and py-verbose-p erg1)
      (message "py-warn-tmp-files-left: %s ?" (concat py-temp-directory (char-to-string py-separator-char) (car (directory-files  py-temp-directory nil "py[[:alnum:]]*$")))))))

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

(defalias 'py-help-at-point 'py-describe-symbol)
(defun py-describe-symbol (&optional arg)
  "Print help on symbol at point.

Optional \\[universal-argument] used for debugging, will prevent deletion of temp file. "
  (interactive "P")
  (let* ((orig (point))
         (beg (progn (when (and (looking-back "(")(not (looking-at "\\sw"))) (forward-char -1)) (skip-chars-backward "a-zA-Z0-9_." (line-beginning-position))(point)))
         (end (progn (skip-chars-forward "a-zA-Z0-9_." (line-end-position))(point)))
         (sym (buffer-substring-no-properties beg end))
         (origfile (buffer-file-name))
         (temp (make-temp-name (buffer-name)))
         (file (concat (expand-file-name temp py-temp-directory) ".py"))
         (cmd (py-find-imports)))
    (goto-char orig)
    (when cmd
      (setq cmd (mapconcat
                 (lambda (arg) (concat "try: " arg "\nexcept: pass\n"))
                 (split-string cmd ";" t)
                 "")))
    (setq cmd (concat "import pydoc\n"
                      cmd))
    (when (not py-remove-cwd-from-path)
      (setq cmd (concat cmd "import sys\n"
                        "sys.path.insert(0, '"
                        (file-name-directory origfile) "')\n")))
    (setq cmd (concat cmd "pydoc.help('" sym "')\n"))
    (with-temp-buffer
      (insert cmd)
      (write-file file))
    (py-process-file file "*Python-Help*")
    (when (file-readable-p file)
      (unless (eq 4 (prefix-numeric-value arg)) (delete-file file)))))

(defun py-eldoc-function ()
  "Print help on symbol at point. "
  (interactive)
  (if (unless (looking-at " ")
        (or

         (eq (get-char-property (point) 'face) 'font-lock-keyword-face)
         (eq (get-char-property (point) 'face) 'py-builtins-face)
         (eq (get-char-property (point) 'face) 'py-exception-name-face)
         (eq (get-char-property (point) 'face) 'py-class-name-face)

         ))

      (lexical-let* ((sym (prin1-to-string (symbol-at-point)))
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
          (delete-file file)))
    (delete-other-windows)))

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
  "Find top-level imports, updating `python-imports'.

Returns python-imports"
  (interactive)
  (let (imports)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^import *[A-Za-z_][A-Za-z_0-9].*\\|^from +[A-Za-z_][A-Za-z_0-9.]+ +import .*" nil t)
        (unless (py-end-of-statement-p)
          (py-end-of-statement))
        (setq imports
              (concat
               imports
               (replace-regexp-in-string
                "[\\]\r?\n?\s*" ""
                (buffer-substring-no-properties (match-beginning 0) (point))) ";"))))
    (when (and py-verbose-p (interactive-p)) (message "%s" imports))
    (setq python-imports imports)
    imports))

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

;;; pep8
(defalias 'pep8 'py-pep8-run)
(defun py-pep8-run (command)
  "*Run pep8, check formatting (default on the file currently visited).
"
  (interactive
   (let ((default
           (if (buffer-file-name)
               (format "%s %s %s" py-pep8-command
                       (mapconcat 'identity py-pep8-command-args " ")
                       (buffer-file-name))
             (format "%s %s" py-pep8-command
                     (mapconcat 'identity py-pep8-command-args " "))))
         (last (when py-pep8-history
                 (let* ((lastcmd (car py-pep8-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (buffer-file-name) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pep8 like this: "
                              (if last
                                  last
                                default)
                              'py-pep8-history)
        (read-string "Run pep8 like this: "
                     (if last
                         last
                       default)
                     'py-pep8-history)))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

(defun py-pep8-help ()
  "Display pep8 command line help messages. "
  (interactive)
  (set-buffer (get-buffer-create "*pep8-Help*"))
  (erase-buffer)
  (shell-command "pep8 --help" "*pep8-Help*"))

;;; Pylint
(defalias 'pylint 'py-pylint-run)
(defun py-pylint-run (command)
  "*Run pylint (default on the file currently visited).

For help see M-x pylint-help resp. M-x pylint-long-help.
Home-page: http://www.logilab.org/project/pylint "
  (interactive
   (let ((default
           (if (buffer-file-name)
               (format "%s %s %s" py-pylint-command
                       (mapconcat 'identity py-pylint-command-args " ")
                       (buffer-file-name))
             (format "%s %s" py-pylint-command
                     (mapconcat 'identity py-pylint-command-args " "))))
         (last (when py-pylint-history
                 (let* ((lastcmd (car py-pylint-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (buffer-file-name) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pylint like this: "
                              (if last
                                  last
                                default)
                              'py-pylint-history)
        (read-string "Run pylint like this: "
                     (if last
                         last
                       default)
                     'py-pylint-history)))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

(defalias 'pylint-help 'py-pylint-help)
(defun py-pylint-help ()
  "Display Pylint command line help messages.

Let's have this until more Emacs-like help is prepared "
  (interactive)
  (set-buffer (get-buffer-create "*Pylint-Help*"))
  (erase-buffer)
  (shell-command "pylint --long-help" "*Pylint-Help*"))

(defalias 'pylint-doku 'py-pylint-doku)
(defun py-pylint-doku ()
  "Display Pylint Documentation.

Calls `pylint --full-documentation'"
  (interactive)
  (set-buffer (get-buffer-create "*Pylint-Documentation*"))
  (erase-buffer)
  (shell-command "pylint --full-documentation" "*Pylint-Documentation*"))

;;; Pyflakes
(defalias 'pyflakes 'py-pyflakes-run)
(defun py-pyflakes-run (command)
  "*Run pyflakes (default on the file currently visited).

For help see M-x pyflakes-help resp. M-x pyflakes-long-help.
Home-page: http://www.logilab.org/project/pyflakes "
  (interactive
   (let ((default
           (if (buffer-file-name)
               (format "%s %s %s" py-pyflakes-command
                       (mapconcat 'identity py-pyflakes-command-args " ")
                       (buffer-file-name))
             (format "%s %s" py-pyflakes-command
                     (mapconcat 'identity py-pyflakes-command-args " "))))
         (last (when py-pyflakes-history
                 (let* ((lastcmd (car py-pyflakes-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (buffer-file-name) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pyflakes like this: "
                              (if last
                                  last
                                default)
                              'py-pyflakes-history)
        (read-string "Run pyflakes like this: "
                     (if last
                         last
                       default)
                     'py-pyflakes-history)))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

(defalias 'pyflakes-help 'py-pyflakes-help)
(defun py-pyflakes-help ()
  "Display Pyflakes command line help messages.

Let's have this until more Emacs-like help is prepared "
  (interactive)
  ;; (set-buffer (get-buffer-create "*Pyflakes-Help*"))
  ;; (erase-buffer)
  (with-help-window "*Pyflakes-Help*"
    (with-current-buffer standard-output
      (insert "       pyflakes [file-or-directory ...]

       Pyflakes is a simple program which checks Python
       source files for errors. It is similar to
       PyChecker in scope, but differs in that it does
       not execute the modules to check them. This is
       both safer and faster, although it does not
       perform as many checks. Unlike PyLint, Pyflakes
       checks only for logical errors in programs; it
       does not perform any checks on style.

       All commandline arguments are checked, which
       have to be either regular files or directories.
       If a directory is given, every .py file within
       will be checked.

       When no commandline arguments are given, data
       will be read from standard input.

       The exit status is 0 when no warnings or errors
       are found. When errors are found the exit status
       is 2. When warnings (but no errors) are found
       the exit status is 1.

Extracted from http://manpages.ubuntu.com/manpages/natty/man1/pyflakes.1.html
"))))

;;; Pyflakes-pep8
(defalias 'pyflakespep8 'py-pyflakespep8-run)
(defun py-pyflakespep8-run (command)
  "*Run pyflakespep8, check formatting (default on the file currently visited).
"
  (interactive
   (let ((default
           (if (buffer-file-name)
               (format "%s %s %s" py-pyflakespep8-command
                       (mapconcat 'identity py-pyflakespep8-command-args " ")
                       (buffer-file-name))
             (format "%s %s" py-pyflakespep8-command
                     (mapconcat 'identity py-pyflakespep8-command-args " "))))
         (last (when py-pyflakespep8-history
                 (let* ((lastcmd (car py-pyflakespep8-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (buffer-file-name) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pyflakespep8 like this: "
                              (if last
                                  last
                                default)
                              'py-pyflakespep8-history)
        (read-string "Run pyflakespep8 like this: "
                     (if last
                         last
                       default)
                     'py-pyflakespep8-history)))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

(defun py-pyflakespep8-help ()
  "Display pyflakespep8 command line help messages. "
  (interactive)
  (set-buffer (get-buffer-create "*pyflakespep8-Help*"))
  (erase-buffer)
  (shell-command "pyflakespep8 --help" "*pyflakespep8-Help*"))

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
(defcustom strip-chars-before  "\\`[ \t\r\n]*"
  "Regexp indicating which chars shall be stripped before STRING - which is defined by `string-chars-preserve'."

  :type 'string
  :group 'convenience)

(defcustom strip-chars-after  "[ \t\r\n]*\\'"
  "Regexp indicating which chars shall be stripped after STRING - which is defined by `string-chars-preserve'."

  :type 'string
  :group 'convenience)

(defun string-strip (str &optional chars-before chars-after)
  "Return a copy of STR, CHARS removed.
`CHARS-BEFORE' and `CHARS-AFTER' default is \"[ \t\r\n]*\",
i.e. spaces, tabs, carriage returns, newlines and newpages. "
  (let ((s-c-b (or chars-before
                   strip-chars-before))
        (s-c-a (or chars-after
                   strip-chars-after))
        (erg str))
    (setq erg (replace-regexp-in-string  s-c-b "" erg))
    (setq erg (replace-regexp-in-string  s-c-a "" erg))
    erg))

(provide 'python-components-help)
;;; python-components-help.el ends here
