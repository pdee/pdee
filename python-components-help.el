;;; python-components-help.el --- help functions -*- lexical-binding: t; -*-

;; URL: https://gitlab.com/python-mode-devs

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

;;  Info-look functionality.
(require 'info-look)
(eval-when-compile (require 'info))

(defun py-info-lookup-symbol ()
  "Call ‘info-lookup-symbol’.

Sends help if stuff is missing."
  (interactive)
  (if (functionp 'pydoc-info-add-help)
      (call-interactively 'info-lookup-symbol)
    (message "pydoc-info-add-help not found. Please check INSTALL-INFO-FILES")))

(info-lookup-add-help
 :mode 'python-mode
 :regexp "[[:alnum:]_]+"
 :doc-spec
'(("(python)Index" nil "")))

(defun python-after-info-look ()
  "Set up info-look for Python.

Tries to take account of versioned Python Info files, e.g. Debian's
python2.5-ref.info.gz.
Used with ‘eval-after-load’."
  (let* ((version (let ((s (shell-command-to-string (concat py-python-command
                                                            " -V"))))
                    (string-match "^Python \\([0-9]+\\.[0-9]+\\>\\)" s)
                    (match-string 1 s)))
         ;; Whether info files have a Python version suffix, e.g. in Debian.
         (versioned
          (with-temp-buffer
            (Info-mode)
            ;; First look for Info files corresponding to the version
            ;; of the interpreter we're running.
            (condition-case ()
                ;; Do not use ‘info’ because it would pop-up a *info* buffer.
                (progn
                  (Info-goto-node (format "(python%s-lib)Miscellaneous Index"
                                          version))
                  t)
              (error
               ;; Otherwise see if we actually have an un-versioned one.
               (condition-case ()
                   (progn
                     (Info-goto-node
                      (format "(python%s-lib)Miscellaneous Index" version))
                     nil)
                 (error
                  ;; Otherwise look for any versioned Info file.
                  (condition-case ()
                      (let (found)
                        (dolist (dir (or Info-directory-list
                                         Info-default-directory-list))
                          (unless found
                            (let ((file (car (file-expand-wildcards
                                              (expand-file-name "python*-lib*"
                                                                dir)))))
                              (if (and file
                                       (string-match
                                        "\\<python\\([0-9]+\\.[0-9]+\\>\\)-"
                                        file))
                                  (setq version (match-string 1 file)
                                        found t)))))
                        found)
                    (error)))))))))
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
         `((,(concat "(python" version "-ref)Miscellaneous Index"))
           (,(concat "(python" version "-ref)Module Index"))
           (,(concat "(python" version "-ref)Function-Method-Variable Index"))
           (,(concat "(python" version "-ref)Class-Exception-Object Index"))
           (,(concat "(python" version "-lib)Module Index"))
           (,(concat "(python" version "-lib)Class-Exception-Object Index"))
           (,(concat "(python" version "-lib)Function-Method-Variable Index"))
           (,(concat "(python" version "-lib)Miscellaneous Index")))
       '(("(python-ref)Miscellaneous Index")
         ("(python-ref)Module Index")
         ("(python-ref)Function-Method-Variable Index")
         ("(python-ref)Class-Exception-Object Index")
         ("(python-lib)Module Index")
         ("(python-lib)Class-Exception-Object Index")
         ("(python-lib)Function-Method-Variable Index")
         ("(python-lib)Miscellaneous Index"))))))

;;  (if (featurep 'info-look)
;;      (python-after-info-look))

;;  (eval-after-load "info-look" '(python-after-info-look))

;; ;

(defun py-fetch-docu ()
  "Lookup in current buffer for the doku for the symbol at point.

Useful for newly defined symbol, not known to python yet."
  (interactive)
  (let* ((symb (prin1-to-string (symbol-at-point)))
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
            ;; (when (called-interactively-p 'interactive)
            ;;   (switch-to-buffer (current-buffer)))
            (insert erg)))))))

(defun py-info-current-defun (&optional include-type)
  "Return name of surrounding function.

Use Python compatible dotted expression syntax
Optional argument INCLUDE-TYPE indicates to include the type of the defun.
This function is compatible to be used as
‘add-log-current-defun-function’ since it returns nil if point is
not inside a defun."
  (interactive)
  (let ((names '())
        (min-indent)
        (first-run t))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (line-end-position))
        (forward-comment -9999)
        (setq min-indent (current-indentation))
        (while (py-backward-def-or-class)
          (when (or (< (current-indentation) min-indent)
                    first-run)
            (setq first-run nil)
            (setq min-indent (current-indentation))
            (looking-at py-def-or-class-re)
            (setq names (cons
                         (if (not include-type)
                             (match-string-no-properties 1)
                           (mapconcat 'identity
                                      (split-string
                                       (match-string-no-properties 0)) " "))
                         names))))))
    (when names
      (mapconcat (lambda (strg) strg) names "."))))

(defalias (quote py-describe-symbol) (quote py-help-at-point))
(defun py--help-at-point-intern (sym orig)
  (let* ((origfile (py--buffer-filename-remote-maybe))
         (cmd (py-find-imports))
         (oldbuf (current-buffer))
         )
    (when (not py-remove-cwd-from-path)
      (setq cmd (concat cmd "import sys\n"
                        "sys.path.insert(0, '"
                        (file-name-directory origfile) "')\n")))
    ;; (setq cmd (concat cmd "pydoc.help('" sym "')\n"))
    (py-execute-string (concat cmd "help('" sym "')\n") nil t nil orig nil nil nil nil nil nil oldbuf t)
    (display-buffer oldbuf)))
    ;; (with-help-window "Hilfe" (insert py-result))))

(defun py-help-at-point ()
  "Print help on symbol at point.

If symbol is defined in current buffer, jump to its definition"
  (interactive)
  (let* ((orig (point))
         (beg (and (use-region-p) (region-beginning)))
         (end (and (use-region-p) (region-end)))
         (symbol
          (or (and beg end
                   (buffer-substring-no-properties beg end))
              ;; (thing-at-point 'symbol t)
              (py-symbol-at-point))))
    (and symbol (unless (string= "" symbol)
                  (py--help-at-point-intern symbol orig))
         )))

(defun py--dump-help-string (str)
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
          (error "Error in py--dump-help-string, tag %s" funckind)))
        (princ (format "\n-> %s:\t%s\t%s\n\n"
                       (if (equal funckind "c") "Command" "Variable")
                       funcname keys))
        (princ funcdoc)
        (terpri)
        (setq start end))
      (princ (substitute-command-keys (substring str start)))
      ;; (and comint-vars-p (py-report-comint-variable-setting))
      )
    (if (featurep 'xemacs) (print-help-return-message)
      (help-print-return-message))))

(defun py-describe-mode ()
  "Dump long form of ‘python-mode’ docs."
  (interactive)
  (py--dump-help-string "Major mode for editing Python files.
Knows about Python indentation, tokens, comments and continuation lines.
Paragraphs are separated by blank lines only.

Major sections below begin with the string ‘@’; specific function and
variable docs begin with ->.

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

py-install-directory\twherefrom ‘python-mode’ looks for extensions
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
character is `#’), or a ‘code line' (everything else).

Comment Lines

Although all comment lines are treated alike by Python, Python mode
recognizes two kinds that act differently with respect to indentation.

An `indenting comment line' is a comment line with a blank, tab or
nothing after the initial ‘#’.  The indentation commands (see below)
treat these exactly as if they were code lines: a line following an
indenting comment line will be indented like the comment line.  All
other comment lines (those with a non-whitespace character immediately
following the initial `#’) are ‘non-indenting comment lines', and
their indentation is ignored by the indentation commands.

Indenting comment lines are by far the usual case, and should be used
whenever possible.  Non-indenting comment lines are useful in cases
like these:

\ta = b # a very wordy single-line comment that ends up being
\t #... continued onto another line

\tif a == b:
##\t\tprint 'panic!' # old code we've `commented out'
\t\treturn a

Since the `#...’ and ‘##' comment lines have a non-whitespace
character following the initial ‘#’, Python mode ignores them when
computing the proper indentation for the next line.

Continuation Lines and Statements

The ‘python-mode’ commands generally work on statements instead of on
individual lines, where a ‘statement’ is a comment or blank line, or a
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
automatically by ‘python-mode’ is just an educated guess:  only you know
the block structure you intend, so only you can supply correct
indentation.

The \\[indent-for-tab-command] and \\[py-newline-and-indent] keys try to suggest plausible indentation, based on
the indentation of preceding statements.  E.g., assuming
py-indent-offset is 4, after you enter
\tif a > 0: \\[py-newline-and-indent]
the cursor will be moved to the position of the ‘_’ (_ is not a
character in the file, it is just used here to indicate the location of
the cursor):
\tif a > 0:
\t _
If you then enter `c = d' \\[py-newline-and-indent], the cursor will move
to
\tif a > 0:
\t c = d
\t _
‘python-mode’ cannot know whether that's what you intended, or whether
\tif a > 0:
\t c = d
\t_
was your intent.  In general, ‘python-mode’ either reproduces the
indentation of the (closest code or indenting-comment) preceding
statement, or adds an extra py-indent-offset blanks if the preceding
statement has ‘:’ as its last significant (non-whitespace and non-
comment) character.  If the suggested indentation is too much, use
\\[py-electric-backspace] to reduce it.

Continuation lines are given extra indentation.  If you do not like the
suggested indentation, change it to something you do like, and Python-
mode will strive to indent later lines of the statement in the same way.

If a line is a continuation line by virtue of being in an unclosed
paren/bracket/brace structure (‘list’, for short), the suggested
indentation depends on whether the current line contains the first item
in the list.  If it does, it is indented py-indent-offset columns beyond
the indentation of the line containing the open bracket.  If you do not
like that, change it by hand.  The remaining items in the list will mimic
whatever indentation you give to the first item.

If a line is a continuation line because the line preceding it ends with
a backslash, the third and following lines of the statement inherit their
indentation from the line preceding them.  The indentation of the second
line in the statement depends on the form of the first (base) line:  if
the base line is an assignment statement with anything more interesting
than the backslash following the leftmost assigning ‘=’, the second line
is indented two columns beyond that ‘=’.  Else it is indented to two
columns beyond the leftmost solid chunk of non-whitespace characters on
the base line.

Warning:  indent-region should not normally be used!  It calls \\[indent-for-tab-command]
repeatedly, and as explained above, \\[indent-for-tab-command] can not guess the block
structure you intend.
%c:indent-for-tab-command
%c:py-newline-and-indent
%c:py-electric-backspace

The next function may be handy when editing code you did not write:
%c:py-guess-indent-offset

The remaining ‘indent’ functions apply to a region of Python code.  They
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
\\[py-backward-def-or-class]\t move to start of def
\\[universal-argument] \\[py-backward-def-or-class]\t move to start of class
\\[py-forward-def-or-class]\t move to end of def
\\[universal-argument] \\[py-forward-def-or-class]\t move to end of class

The first two move to one statement beyond the statement that contains
point.  A numeric prefix argument tells them to move that many
statements instead.  Blank lines, comment lines, and continuation lines
do not count as ‘statements’ for these commands.  So, e.g., you can go
to the first code statement in a file by entering
\t\\[beginning-of-buffer]\t to move to the top of the file
\t\\[py-next-statement]\t to skip over initial comments and blank lines
Or do \\[py-previous-statement] with a huge prefix argument.
%c:py-previous-statement
%c:py-next-statement
%c:py-goto-block-up
%c:py-backward-def-or-class
%c:py-forward-def-or-class

@LITTLE-KNOWN EMACS COMMANDS PARTICULARLY USEFUL IN PYTHON MODE

\\[indent-new-comment-line] is handy for entering a multi-line comment.

\\[set-selective-display] with a ‘small’ prefix arg is ideally suited for viewing the
overall class and def structure of a module.

‘\\[back-to-indentation]’ moves point to a line's first non-blank character.

‘\\[indent-relative]’ is handy for creating odd indentation.

@OTHER EMACS HINTS

If you do not like the default value of a variable, change its value to
whatever you do like by putting a ‘setq’ line in your .emacs file.
E.g., to set the indentation increment to 4, put this line in your
.emacs:
\t(setq py-indent-offset 4)
To see the value of a variable, do ‘\\[describe-variable]’ and enter the variable
name at the prompt.

When entering a key sequence like `C-c C-n', it is not necessary to
release the CONTROL key after doing the ‘C-c’ part -- it suffices to
press the CONTROL key, press and release ‘c’ (while still holding down
CONTROL), press and release ‘n’ (while still holding down CONTROL), &
then release CONTROL.

Entering Python mode calls with no arguments the value of the variable
‘python-mode-hook’, if that value exists and is not nil; for backward
compatibility it also tries ‘py-mode-hook’; see the ‘Hooks' section of
the Elisp manual for details.

Obscure:  When python-mode is first loaded, it looks for all bindings
to newline-and-indent in the global keymap, and shadows them with
local bindings to py-newline-and-indent."))

;;  (require 'info-look)
;;  The info-look package does not always provide this function (it
;;  appears this is the case with XEmacs 21.1)
(when (fboundp 'info-lookup-maybe-add-help)
  (info-lookup-maybe-add-help
   :mode 'python-mode
   :regexp "[a-zA-Z0-9_]+"
   :doc-spec '(("(python-lib)Module Index")
               ("(python-lib)Class-Exception-Object Index")
               ("(python-lib)Function-Method-Variable Index")
               ("(python-lib)Miscellaneous Index"))))

(defun py--find-definition-in-source (sourcefile symbol)
  (called-interactively-p 'any) (message "sourcefile: %s" sourcefile)
  (when (find-file sourcefile)
    (goto-char (point-min))
    (when
        (or (re-search-forward (concat py-def-or-class-re symbol) nil t 1)
            (progn
              ;; maybe a variable definition?
              (goto-char (point-min))
              (re-search-forward (concat "^.+ " symbol) nil t 1)))
      (push-mark)
      (goto-char (match-beginning 0))
      (exchange-point-and-mark))))

;;  Find function stuff, lifted from python.el
(defalias (quote py-find-function) (quote py-find-definition))
(defun py--find-definition-question-type (symbol imports)
  (let (erg)
    (cond ((setq erg (py-execute-string (concat "import inspect;inspect.isbuiltin(\"" symbol "\")"))))
          (t (setq erg (py-execute-string (concat imports "import inspect;inspect.getmodule(\"" symbol "\")")))))
    erg))

(defun py-find-definition (&optional symbol)
  "Find source of definition of SYMBOL.

Interactively, prompt for SYMBOL."
  (interactive)
  ;; (set-register 98888888 (list (current-window-configuration) (point-marker)))
  (let* (;; end
         ;; (last-window-configuration
         ;;  (current-window-configuration))
         (orig (point))
         ;; (exception-buffer (current-buffer))
         (imports (py-find-imports))
         (symbol-raw (or symbol (with-syntax-table py-dotted-expression-syntax-table
                                  (current-word))))
         ;; (enable-recursive-minibuffers t)
         (symbol (if (called-interactively-p 'interactive)
                     (read-string (format "Find location of (default %s): " symbol-raw)
                                  symbol-raw nil symbol-raw)
                   symbol-raw))
         (local (progn (goto-char (point-min)) (re-search-forward (concat "^[ \t]*" "\\(def\\|class\\)" "[ \t]" symbol) orig t))))
    ;; ismethod(), isclass(), isfunction() or isbuiltin()
    ;; ismethod isclass isfunction isbuiltin)
    (if local
        (progn
          (goto-char orig)
          (split-window-vertically)
          (other-buffer)
          (goto-char local)
          (beginning-of-line)
          (push-mark)
          (message "%s" (current-buffer))
          (exchange-point-and-mark))
      (with-help-window (help-buffer)
        (princ (py--find-definition-question-type symbol imports))))))

(defun py-update-imports ()
  "Return imports.

Imports done are displayed in message buffer."
  (interactive)
  (save-excursion
    (let ((orig (point))
          (erg (py-find-imports)))
      (goto-char orig)
      erg)))

;;  Code-Checker
;;  pep8
(defalias 'pep8 (quote py-pep8-run))
(defun py-pep8-run (command)
  "*Run pep8 using COMMAND, check formatting.
Default on the file currently visited."
  (interactive
   (let ((default
           (if (py--buffer-filename-remote-maybe)
               (format "%s %s %s" py-pep8-command
                       (mapconcat 'identity py-pep8-command-args " ")
                       (py--buffer-filename-remote-maybe))
             (format "%s %s" py-pep8-command
                     (mapconcat 'identity py-pep8-command-args " "))))
         (last (when py-pep8-history
                 (let* ((lastcmd (car py-pep8-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (py--buffer-filename-remote-maybe) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pep8 like this: "
                              (if last
                                  last
                                default)
                              (quote py-pep8-history))
        (read-string "Run pep8 like this: "
                     (if last
                         last
                       default)
                     (quote py-pep8-history))))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

(defun py-pep8-help ()
  "Display pep8 command line help messages."
  (interactive)
  (set-buffer (get-buffer-create "*pep8-Help*"))
  (erase-buffer)
  (shell-command "pep8 --help" "*pep8-Help*"))

;;  Pylint
(defalias 'pylint (quote py-pylint-run))
(defun py-pylint-run (command)
  "Run pylint from COMMAND.

Default on the file currently visited.

For help see \\[pylint-help] resp. \\[pylint-long-help].
Home-page: http://www.logilab.org/project/pylint"
  (interactive
   (let ((default (format "%s %s %s" py-pylint-command
                          (mapconcat 'identity py-pylint-command-args " ")
                          (py--buffer-filename-remote-maybe)))
         (last (and py-pylint-history (car py-pylint-history))))
     (list (funcall (if (fboundp 'read-shell-command)
                        'read-shell-command 'read-string)
                    "Run pylint like this: "
                    (or default last)
                    (quote py-pylint-history)))))
    (save-some-buffers (not py-ask-about-save))
  (set-buffer (get-buffer-create "*Pylint*"))
  (erase-buffer)
  (unless (file-readable-p (car (reverse (split-string command))))
    (message "Warning: %s" "pylint needs a file"))
  (shell-command command "*Pylint*"))

(defalias 'pylint-help (quote py-pylint-help))
(defun py-pylint-help ()
  "Display Pylint command line help messages.

Let's have this until more Emacs-like help is prepared"
  (interactive)
  (with-help-window "*Pylint-Help*"
    (shell-command "pylint --long-help" "*Pylint-Help*")))

(defalias 'pylint-doku (quote py-pylint-doku))
(defun py-pylint-doku ()
  "Display Pylint Documentation.

Calls `pylint --full-documentation'"
  (interactive)
  (set-buffer (get-buffer-create "*Pylint-Documentation*"))
  (erase-buffer)
  (shell-command "pylint --full-documentation" "*Pylint-Documentation*"))

;;  Pyflakes3
(defalias 'pyflakes (quote py-pyflakes3-run))
(defun py-pyflakes3-run (command)
  "Check Python source files for errors."
  (interactive
   (let* ((py-pyflakes3-command
           (if (string= "" py-pyflakes3-command)
               (or (executable-find "pyflakes3")
                   (error "Do not see \"pyflakes3\" on your system.
Consider \"pip install pyflakes3\" resp. visit \"pypi.python.org\""))
             py-pyflakes3-command))
          (default
            (if (py--buffer-filename-remote-maybe)
                (format "%s %s %s" py-pyflakes3-command
                        py-pyflakes3-command-args
                        (py--buffer-filename-remote-maybe))
              (format "%s %s" py-pyflakes3-command
                      py-pyflakes3-command-args)))
          (last
           (when py-pyflakes3-history
             (let* ((lastcmd (car py-pyflakes3-history))
                    (cmd (cdr (reverse (split-string lastcmd))))
                    (newcmd (reverse (cons (py--buffer-filename-remote-maybe) cmd))))
               (mapconcat 'identity newcmd " ")))))
     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pyflakes3 like this: "
                              ;; (if last
                              ;; last
                              default
                              (quote py-pyflakes3-history1))
        (read-string "Run pyflakes3 like this: "
                     (if last
                         last
                       default)
                     (quote py-pyflakes3-history))))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

(defalias 'pyflakes-help (quote py-pyflakes3-help))
(defun py-pyflakes3-help ()
  "Display Pyflakes3 command line help messages."
  (interactive)
  (with-help-window "*pyflakes3-Help*"
    (shell-command "pyflakes3 --help" "*pyflakes3-Help*")))

;;  Pyflakes-pep8
(defalias 'pyflakespep8 (quote py-pyflakespep8-run))
(defun py-pyflakespep8-run (command)
  "*Run COMMAND pyflakespep8, check formatting.

Default on the file currently visited."
  (interactive
   (let ((default
           (if (py--buffer-filename-remote-maybe)
               (format "%s %s %s" py-pyflakespep8-command
                       (mapconcat 'identity py-pyflakespep8-command-args " ")
                       (py--buffer-filename-remote-maybe))
             (format "%s %s" py-pyflakespep8-command
                     (mapconcat 'identity py-pyflakespep8-command-args " "))))
         (last (when py-pyflakespep8-history
                 (let* ((lastcmd (car py-pyflakespep8-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (py--buffer-filename-remote-maybe) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pyflakespep8 like this: "
                              (if last
                                  last
                                default)
                              (quote py-pyflakespep8-history))
        (read-string "Run pyflakespep8 like this: "
                     (if last
                         last
                       default)
                     (quote py-pyflakespep8-history))))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

(defun py-pyflakespep8-help ()
  "Display pyflakespep8 command line help messages."
  (interactive)
  (set-buffer (get-buffer-create "*pyflakespep8-Help*"))
  (erase-buffer)
  (shell-command "pyflakespep8 --help" "*pyflakespep8-Help*"))

;;  Pychecker
;;  hack for GNU Emacs
;;  (unless (fboundp 'read-shell-command)
;;  (defalias 'read-shell-command 'read-string))

(defun py-pychecker-run (command)
  "Run COMMAND pychecker (default on the file currently visited)."
  (interactive
   (let ((default
           (if (py--buffer-filename-remote-maybe)
               (format "%s %s %s" py-pychecker-command
                       py-pychecker-command-args
                       (py--buffer-filename-remote-maybe))
             (format "%s %s" py-pychecker-command py-pychecker-command-args)))
         (last (when py-pychecker-history
                 (let* ((lastcmd (car py-pychecker-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (py--buffer-filename-remote-maybe) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pychecker like this: "
                              (if last
                                  last
                                default)
                              (quote py-pychecker-history))
        (read-string "Run pychecker like this: "
                     (if last
                         last
                       default)
                     (quote py-pychecker-history))))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

;;  After ‘sgml-validate-command’.
(defun py-check-command (command)
  "Check a Python file (default current buffer's file).
Runs COMMAND, a shell command, as if by ‘compile’.
See ‘py-check-command’ for the default."
  (interactive
   (list (read-string "Checker command: "
                      (concat py-check-command " "
                              (let ((name (py--buffer-filename-remote-maybe)))
                                (if name
                                    (file-name-nondirectory name)))))))
  (require 'compile)                    ;To define compilation-* variables.
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((compilation-error-regexp-alist py-compilation-regexp-alist)
        ;; (cons '("(\\([^,]+\\), line \\([0-9]+\\))" 1)
        ;; compilation-error-regexp-alist)
        )
    (compilation-start command)))

;;  flake8
(defalias 'flake8 (quote py-flake8-run))
 (defun py-flake8-run (command)
  "COMMAND Flake8 is a wrapper around these tools:
- PyFlakes
        - pep8
        - Ned Batchelder's McCabe script

        It also adds features:
        - files that contain this line are skipped::
            # flake8: noqa
        - no-warn lines that contain a `# noqa`` comment at the end.
        - a Git and a Mercurial hook.
        - a McCabe complexity checker.
        - extendable through ``flake8.extension`` entry points."
  (interactive
   (let* ((py-flake8-command
           (if (string= "" py-flake8-command)
               (or (executable-find "flake8")
                   (error "Do not see \"flake8\" on your system.
Consider \"pip install flake8\" resp. visit \"pypi.python.org\""))
             py-flake8-command))
          (default
            (if (py--buffer-filename-remote-maybe)
                (format "%s %s %s" py-flake8-command
                        py-flake8-command-args
                        (py--buffer-filename-remote-maybe))
              (format "%s %s" py-flake8-command
                      py-flake8-command-args)))
          (last
           (when py-flake8-history
             (let* ((lastcmd (car py-flake8-history))
                    (cmd (cdr (reverse (split-string lastcmd))))
                    (newcmd (reverse (cons (py--buffer-filename-remote-maybe) cmd))))
               (mapconcat 'identity newcmd " ")))))
     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run flake8 like this: "
                              ;; (if last
                              ;; last
                              default
                              (quote py-flake8-history1))
        (read-string "Run flake8 like this: "
                     (if last
                         last
                       default)
                     (quote py-flake8-history))))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

(defun py-flake8-help ()
  "Display flake8 command line help messages."
  (interactive)
  (with-help-window "*flake8-Help*"
    (shell-command "flake8 --help" "*flake8-Help*")))

;;  from string-strip.el --- Strip CHARS from STRING

(defun py-nesting-level (&optional pps)
  "Accepts the output of ‘parse-partial-sexp’ - PPS."
  (interactive)
  (let* ((pps (or (ignore-errors (nth 0 pps))
                  (if (featurep 'xemacs)
                      (parse-partial-sexp (point-min) (point))
                    (parse-partial-sexp (point-min) (point)))))
         (erg (nth 0 pps)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

;;  Flymake
(defun py-toggle-flymake-intern (name command)
  "Clear flymake allowed file-name masks.

Takes NAME COMMAND"
  (unless (string-match "pyflakespep8" name)
    (unless (executable-find name)
      (when py-verbose-p (message "Do not see %s. Use ‘easy_install’ %s? " name name))))
  (if (py--buffer-filename-remote-maybe)
      (let* ((temp-file (if (functionp 'flymake-proc-init-create-temp-buffer-copy)
                            (flymake-proc-init-create-temp-buffer-copy 'flymake-create-temp-inplace)
                          (flymake-proc-init-create-temp-buffer-copy 'flymake-create-temp-inplace)
                          ))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory (py--buffer-filename-remote-maybe)))))
        (if (boundp 'flymake-proc-allowed-file-name-masks)
            (push (car (read-from-string (concat "(\"\\.py\\'\" flymake-" name ")"))) flymake-proc-allowed-file-name-masks)
          (push (car (read-from-string (concat "(\"\\.py\\'\" flymake-" name ")"))) flymake-proc-allowed-file-name-masks))
        (list command (list local-file)))
    (message "%s" "flymake needs a ‘file-name’. Please save before calling.")))

(defun pylint-flymake-mode ()
  "Toggle ‘pylint’ ‘flymake-mode’."
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode 0)
    (py-toggle-flymake-intern "pylint" "pylint")
    (flymake-mode 1)))

(defun pyflakes-flymake-mode ()
  "Toggle ‘pyflakes’ ‘flymake-mode’."
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode)
    (py-toggle-flymake-intern "pyflakes" "pyflakes")
    (flymake-mode)))

(defun pychecker-flymake-mode ()
  "Toggle ‘pychecker’ ‘flymake-mode’."
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode)
    (py-toggle-flymake-intern "pychecker" "pychecker")
    (flymake-mode)))

(defun pep8-flymake-mode ()
  "Toggle `pep8’ ‘flymake-mode’."
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode)
    (py-toggle-flymake-intern "pep8" "pep8")
    (flymake-mode)))

(defun pyflakespep8-flymake-mode ()
  "Toggle `pyflakespep8’ ‘flymake-mode’.

Joint call to pyflakes and pep8 as proposed by
Keegan Carruthers-Smith"
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode)
    (py-toggle-flymake-intern "pyflakespep8" "pyflakespep8")
    (flymake-mode)))

(defun py-display-state-of-variables ()
  "Read the state of ‘python-mode’ variables.

Assumes vars are defined in current source buffer"
  (interactive)
  (save-restriction
    (let (variableslist)
      (goto-char (point-min))
      ;; (eval-buffer)
      (while (and (not (eobp))(re-search-forward "^(defvar [[:alpha:]]\\|^(defcustom [[:alpha:]]\\|^(defconst [[:alpha:]]" nil t 1))
        (let* ((name (symbol-at-point))
               (state
                (unless
                    (or (eq name (quote py-menu))
                        (eq name 'python-mode-map)
                        (string-match "syntax-table" (prin1-to-string name)))

                  (prin1-to-string (symbol-value name)))))
          (if state
              (push (cons (prin1-to-string name) state) variableslist)
            (message "do not see a state for %s" (prin1-to-string name))))
        (forward-line 1))
      (setq variableslist (nreverse variableslist))
      (set-buffer (get-buffer-create "State-of-Python-mode-variables.org"))
      (erase-buffer)
      ;; org
      (insert "State of python-mode variables\n\n")
      (switch-to-buffer (current-buffer))
      (dolist (ele variableslist)
        (if (string-match "^;;; " (car ele))
            (unless (or (string-match "^;;; Constants\\|^;;; Commentary\\|^;;; Code\\|^;;; Macro definitions\\|^;;; Customization" (car ele)))

              (insert (concat (replace-regexp-in-string "^;;; " "* " (car ele)) "\n")))
          (insert (concat "\n** "(car ele) "\n"))
          (insert (concat "   " (cdr ele) "\n\n")))
        ;; (richten)
        (sit-for 0.01 t))
      (sit-for 0.01 t))))

;; common typo
(defalias 'iypthon 'ipython)
(defalias 'pyhton 'python)

(provide 'python-components-help)
;;;  python-components-help.el ends here
