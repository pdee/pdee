;;; python-components-start1.el --- Edit, debug, develop, run Python programs. -*- lexical-binding: t; -*-

;; Version: 6.3.1

;; URL: https://gitlab.com/groups/python-mode-devs

;; Package-Requires: ((emacs "24"))

;; Author: 2015-2025 https://gitlab.com/groups/python-mode-devs
;;         2003-2014 https://launchpad.net/python-mode
;;         1995-2002 Barry A. Warsaw
;;         1992-1994 Tim Peters
;; Maintainer: python-mode@python.org
;; Created:    Feb 1992
;; Keywords:   python languages oop

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

;; Includes a minor mode for handling a Python/IPython shell, and can
;; take advantage of Pymacs when installed.

;; See documentation in README.org, README.DEVEL.org

;; Please report bugs at
;; https://gitlab.com/python-mode-devs/python-mode/issues

;; available commands are documented in directory "doc" as
;; commands-python-mode.org

;; As for ‘py-add-abbrev’:
;; Similar to ‘add-mode-abbrev’, but uses
;; ‘py-partial-expression’ before point for expansion to
;; store, not ‘word’.  Also provides a proposal for new
;; abbrevs.

;; Proposal for an abbrev is composed from the downcased
;; initials of expansion - provided they are of char-class
;; [:alpha:]
;;
;; For example code below would be recognised as a
;; ‘py-expression’ composed by three
;; py-partial-expressions.
;;
;; OrderedDict.popitem(last=True)
;;
;; Putting the curser at the EOL, M-3 M-x py-add-abbrev
;;
;; would prompt "op" for an abbrev to store, as first
;; ‘py-partial-expression’ beginns with a "(", which is
;; not taken as proposal.

;;; Code:

(require 'ansi-color)
(ignore-errors (require 'subr-x))
(require 'cc-cmds)
(require 'comint)
(require 'compile)
(require 'custom)
(require 'ert)
(require 'flymake)
(require 'hippie-exp)
(require 'hideshow)
(require 'json)
(require 'shell)
(require 'thingatpt)
(require 'which-func)
(require 'tramp)
(require 'tramp-sh)
(require 'org-loaddefs)
(unless (functionp 'mapcan)
  (require 'cl-extra)
  ;; mapcan does not exist in Emacs 25
  (defalias 'mapcan 'cl-mapcan)
  )

;; (require 'org)

(or
 py-install-directory
 (and (buffer-live-p (ignore-errors (set-buffer (get-buffer "python-mode.el")))) ;; mark for a generic mode
      (setq py-install-directory (ignore-errors (file-name-directory (buffer-file-name (get-buffer "python-mode.el"))))))
 (and (buffer-live-p (ignore-errors (set-buffer (get-buffer "python-components-mode.el"))))
      (setq py-install-directory (ignore-errors (file-name-directory (buffer-file-name (get-buffer "python-components-mode.el")))))))

;; credits to python.el

(unless (functionp 'file-local-name)
  (defun file-local-name (file)
    "Return the local name component of FILE.
This function removes from FILE the specification of the remote host
and the method of accessing the host, leaving only the part that
identifies FILE locally on the remote system.
The returned file name can be used directly as argument of
‘process-file’, ‘start-file-process’, or ‘shell-command’."
    (or (file-remote-p file 'localname) file)))

(defun py---emacs-version-greater-23 ()
  "Return ‘t’ if emacs major version is above 23"
  (< 23 (string-to-number (car (split-string emacs-version "\\.")))))

;; (format "execfile(r'%s')\n" file)
(defun py-execute-file-command (filename)
  "Return the command using FILENAME."
  (format "exec(compile(open(r'%s').read(), r'%s', 'exec')) # PYTHON-MODE\n" filename filename)
  )

(defun py--beginning-of-buffer-p ()
  "Returns position, if cursor is at the beginning of buffer.
Return nil otherwise. "
  (when (bobp)(point)))

;;  (setq strip-chars-before  "[ \t\r\n]*")
(defun py--string-strip (str &optional chars-before chars-after)
  "Return a copy of STR, CHARS removed.

Removed chars default to values of ‘py-chars-before’ and ‘py-chars-after’
i.e. spaces, tabs, carriage returns, newlines and newpages

Optional arguments ‘CHARS-BEFORE’ and ‘CHARS-AFTER’ override default"
  (let ((s-c-b (or chars-before
                   py-chars-before))
        (s-c-a (or chars-after
                   py-chars-after))
        (erg str))
    (setq erg (replace-regexp-in-string  s-c-b "" erg))
    (setq erg (replace-regexp-in-string  s-c-a "" erg))
    erg))

(defun py-toggle-session-p (&optional arg)
  "Switch boolean variable ‘py-session-p’.

With optional ARG message state switched to"
  (interactive "p")
  (setq py-session-p (not py-session-p))
  (when arg (message "py-session-p: %s" py-session-p)))

(defun py-toggle-py-return-result-p ()
  "Toggle value of ‘py-return-result-p’."
  (interactive)
  (setq py-return-result-p (not py-return-result-p))
  (when (called-interactively-p 'interactive) (message "py-return-result-p: %s" py-return-result-p)))

;; (defcustom py-autopair-mode nil
;;   "If ‘python-mode’ calls (autopair-mode-on)

;; Default is nil
;; Load ‘autopair-mode’ written by Joao Tavora <joaotavora [at] gmail.com>
;; URL: http://autopair.googlecode.com"
;;   :type 'boolean
;;   :tag "py-autopair-mode"
;;   :group 'python-mode)

(make-variable-buffer-local 'py-indent-list-style)

(make-variable-buffer-local 'py-indent-offset)

(and
 ;; used as a string finally
 ;; kept a character not to break existing customizations
 (characterp py-separator-char)(setq py-separator-char (char-to-string py-separator-char)))

;; (setq py-shells
;; (list
;; ""
;; 'ipython
;; 'ipython2.7
;; 'ipython3
;; 'jython
;; 'python
;; 'python2
;; 'python3
;; 'pypy
;; ))

(defun py-install-named-shells-fix-doc (ele)
  "Internally used by ‘py-load-named-shells’.

Argument ELE: a shell name, a string."
  (cond ((string-match "^i" ele)
         (concat "I" (capitalize (substring ele 1))))
        ((string-match "^pypy" ele)
         "PyPy")
        (t (capitalize ele))))

(make-variable-buffer-local 'py-master-file)

(setq py-pdbtrack-input-prompt "^[(<]*[Ii]?[Pp]y?db[>)]+ *")

;; (setq py-pdbtrack-input-prompt "^[(< \t]*[Ii]?[Pp]y?db[>)]*.*")

(setq py-fast-filter-re
  (concat "\\("
          (mapconcat 'identity
                     (delq nil
                           (list
                            py-shell-input-prompt-1-regexp
                            py-shell-input-prompt-2-regexp
                            py-ipython-input-prompt-re
                            py-ipython-output-prompt-re
                            py-pdbtrack-input-prompt
                            py-pydbtrack-input-prompt
                            "[.]\\{3,\\}:? *"
                            ))
                     "\\|")
          "\\)"))

;; made buffer-local as pdb might need t in all circumstances
(make-variable-buffer-local 'py-switch-buffers-on-execute-p)

;; (defcustom py-shell-name
;;   (if (eq system-type 'windows-nt)
;;       "C:/Python27/python"
;;     "python")

;;   "A PATH/TO/EXECUTABLE or default value ‘py-shell’ may look for.

;; If no shell is specified by command.

;; On Windows default is C:/Python27/python
;; --there is no garantee it exists, please check your system--

;; Else python"
;;   :type 'string
;;   :tag "py-shell-name
;; "
;;   :group 'python-mode)

;; "/usr/bin/python3"

(setq python-mode-syntax-table
      (let ((table (make-syntax-table)))
        ;; Give punctuation syntax to ASCII that normally has symbol
        ;; syntax or has word syntax and is not a letter.
        (let ((symbol (string-to-syntax "_"))
              (sst (standard-syntax-table)))
          (dotimes (i 128)
            (unless (= i ?_)
              (if (equal symbol (aref sst i))
                  (modify-syntax-entry i "." table)))))
        (modify-syntax-entry ?$ "." table)
        (modify-syntax-entry ?% "." table)
        ;; exceptions
        (modify-syntax-entry ?# "<" table)
        (modify-syntax-entry ?\n ">" table)
        (modify-syntax-entry ?' "\"" table)
        (modify-syntax-entry ?` "$" table)
        (if py-underscore-word-syntax-p
            (modify-syntax-entry ?\_ "w" table)
          (modify-syntax-entry ?\_ "_" table))
        table))

(defun py-toggle-py-debug-p ()
  "Toggle value of ‘py-debug-p’."
  (interactive)
  (setq py-debug-p (not py-debug-p))
  (when (called-interactively-p 'interactive) (message "py-debug-p: %s" py-debug-p)))

(make-variable-buffer-local 'py-shell-complete-p)

;; (setq py-colon-labelled-re "[ \\t]*[[:graph:]]* *: *[[:graph:]]+\\|[ \\t]*[\\*-] +[[:graph:]]")

;; "[ \t]+\\c.+"

(setq py-symbol-re "[ \t]*\\c.+[ \t]*")

(setq py-expression-skip-chars "^ [{(=#\t\r\n\f")

(setq py-partial-expression-re (concat "[" py-partial-expression-stop-backward-chars "]+"))

(setq py-indent-re ".+")

;; (setq py-operator-re "[ \t]*\\(\\.\\|+\\|-\\|*\\|//\\|//\\|&\\|%\\||\\|\\^\\|>>\\|<<\\|<\\|<=\\|>\\|>=\\|==\\|!=\\|=\\)[ \t]*")

(setq py-variable-name-face 'py-variable-name-face)

;; (defvar python-font-lock-keywords nil)

;; Constants

(setq py-block-closing-keywords-re
  "[ \t]*\\_<\\(return\\|raise\\|break\\|continue\\|pass\\)\\_>[ \n\t]*")

;; (defconst py-except-re
;;   "[ \t]*\\_<except\\_>[:( \n\t]*"
;;   "Regular expression matching keyword which composes a try-block.")

;; 'name':

(setq py-else-re "else")

;; (defconst py-elif-block-re "[ \t]*\\_<elif\\_> +[[:alpha:]_][[:alnum:]_]* *[: \n\t]"
;;   "Matches the beginning of an ‘elif’ block.")

;; (setq py-def-or-class-re "[ \t]*\\_<\\(async def\\|class\\|def\\)\\_>[ \n\t]")

;; (defconst py-def-re "[ \t]*\\_<\\(async def\\|def\\)\\_>[ \n\t]"

(defun py--arglist-indent (nesting &optional indent-offset)
  "Internally used by ‘py-compute-indentation’"
  (if
      (and (eq 1 nesting)
           (save-excursion
             (back-to-indentation)
             (looking-at py-extended-block-or-clause-re)))
      (progn
        (back-to-indentation)
        (1+ (+ (current-column) (* 2 (or indent-offset py-indent-offset)))))
    (+ (current-indentation) (or indent-offset py-indent-offset))))

(defun py--quote-syntax (n)
  "Put ‘syntax-table’ property correctly on triple quote.
Used for syntactic keywords.  N is the match number (1, 2 or 3)."
  ;; Given a triple quote, we have to check the context to know
  ;; whether this is an opening or closing triple or whether it is
  ;; quoted anyhow, and should be ignored.  (For that we need to do
  ;; the same job as ‘syntax-ppss’ to be correct and it seems to be OK
  ;; to use it here despite initial worries.) We also have to sort
  ;; out a possible prefix -- well, we do not _have_ to, but I think it
  ;; should be treated as part of the string.

  ;; Test cases:
  ;;  ur"""ar""" x='"' # """
  ;; x = ''' """ ' a
  ;; '''
  ;; x '"""' x """ \"""" x
  (save-excursion
    (goto-char (match-beginning 0))
    (cond
     ;; Consider property for the last char if in a fenced string.
     ((= n 3)
      (let* ((syntax (parse-partial-sexp (point-min) (point))))
        (when (eq t (nth 3 syntax))     ; after unclosed fence
          (goto-char (nth 8 syntax))    ; fence position
          ;; (skip-chars-forward "uUrR")        ; skip any prefix
          ;; Is it a matching sequence?
          (if (eq (char-after) (char-after (match-beginning 2)))
              (eval-when-compile (string-to-syntax "|"))))))
     ;; Consider property for initial char, accounting for prefixes.
     ((or (and (= n 2) ; leading quote (not prefix)
               (not (match-end 1)))     ; prefix is null
          (and (= n 1) ; prefix
               (match-end 1)))          ; non-empty
      (unless (eq 'string (syntax-ppss-context (parse-partial-sexp (point-min) (point))))
        (eval-when-compile (string-to-syntax "|"))))
     ;; Otherwise (we're in a non-matching string) the property is
     ;; nil, which is OK.
     )))

;; testing

;; Pdb
;; #62, pdb-track in a shell buffer

(make-variable-buffer-local 'py-pdbtrack-do-tracking-p)

;; subr-x.el might not exist yet

(defun py-toggle-imenu-create-index ()
  "Toggle value of ‘py--imenu-create-index-p’."
  (interactive)
  (setq py--imenu-create-index-p (not py--imenu-create-index-p))
  (when (called-interactively-p 'interactive)
    (message "py--imenu-create-index-p: %s" py--imenu-create-index-p)))

(defun py-toggle-shell-completion ()
  "Switch value of buffer-local var ‘py-shell-complete-p’."
  (interactive)
    (setq py-shell-complete-p (not py-shell-complete-p))
    (when (called-interactively-p 'interactive)
      (message "py-shell-complete-p: %s" py-shell-complete-p)))

(defun py--at-raw-string ()
  "If at beginning of a raw-string."
  (and (looking-at "\"\"\"\\|'''") (member (char-before) (list ?u ?U ?r ?R))))

(defmacro py-preceding-line-backslashed-p ()
  "Return t if preceding line is a backslashed continuation line."
  `(save-excursion
     (beginning-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
          (py-escaped-p))))

(defun py--docstring-p (pos)
  "Check to see if there is a docstring at POS.

If succesful, returns beginning of docstring position in buffer"
  (save-excursion
    (let ((erg
           (progn
             (goto-char pos)
             (and (looking-at "\"\"\"\\|'''")
                  ;; https://github.com/swig/swig/issues/889
                  ;; def foo(rho, x):
                  ;;     r"""Calculate :math:`D^\nu \rho(x)`."""
                  ;;     return True
                  (if (py--at-raw-string)
                      (progn
                        (forward-char -1)
                        (point))
                    (point))))))
      (when (and erg (py-backward-statement))
        (when (or (bobp) (looking-at py-def-or-class-re)(looking-at "\\_<__[[:alnum:]_]+__\\_>"))
          erg)))))

(defun py--font-lock-syntactic-face-function (state)
  "STATE expected as result von (parse-partial-sexp (point-min) (point)."
  (if (nth 3 state)
      (if (py--docstring-p (nth 8 state))
          'font-lock-doc-face
        'font-lock-string-face)
    'font-lock-comment-face))

(and (fboundp 'make-obsolete-variable)
     (make-obsolete-variable 'py-mode-hook 'python-mode-hook nil))

(defun py-choose-shell-by-shebang (&optional shebang)
  "Choose shell by looking at #! on the first line.

If SHEBANG is non-nil, returns the shebang as string,
otherwise the Python resp. Jython shell command name."
  (interactive)
  ;; look for an interpreter specified in the first line
  (let* (erg res)
    (save-excursion
      (goto-char (point-min))
      (when (looking-at py-shebang-regexp)
        (if shebang
            (setq erg (match-string-no-properties 0))
          (setq erg (split-string (match-string-no-properties 0) "[#! \t]"))
          (dolist (ele erg)
            (when (string-match "[bijp]+ython" ele)
              (setq res ele))))))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" res))
    res))

(defun py--choose-shell-by-import ()
  "Choose CPython or Jython mode based imports.

If a file imports any packages in ‘py-jython-packages’, within
‘py-import-check-point-max’ characters from the start of the file,
return ‘jython’, otherwise return nil."
  (let (mode)
    (save-excursion
      (goto-char (point-min))
      (while (and (not mode)
                  (search-forward-regexp
                   "^\\(\\(from\\)\\|\\(import\\)\\) \\([^ \t\n.]+\\)"
                   py-import-check-point-max t))
        (setq mode (and (member (match-string 4) py-jython-packages)
                        'jython))))
    mode))

(defun py-choose-shell-by-path (&optional separator-char)
  "SEPARATOR-CHAR according to system variable ‘path-separator’.

Select Python executable according to version desplayed in path.
Returns versioned string, nil if nothing appropriate found"
  (interactive)
  (let ((path (py--buffer-filename-remote-maybe))
        (separator-char (or separator-char py-separator-char))
                erg)
    (when (and path separator-char
               (string-match (concat separator-char "[iI]?[pP]ython[0-9.]+" separator-char) path))
      (setq erg (substring path
                           (1+ (string-match (concat separator-char "[iI]?[pP]ython[0-9.]+" separator-char) path)) (1- (match-end 0)))))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-which-python (&optional shell)
  "Return version of Python of current environment, a number.
Optional argument SHELL selected shell."
  (interactive)
  (let* ((cmd (or shell (py-choose-shell)))
         (treffer (string-match "\\([23]*\\.?[0-9\\.]*\\)$" cmd))
         version erg)
    (if treffer
        ;; if a number if part of python name, assume its the version
        (setq version (substring-no-properties cmd treffer))
      (setq erg (shell-command-to-string (concat cmd " --version")))
      (setq version (cond ((string-match (concat "\\(on top of Python \\)" "\\([0-9]\\.[0-9]+\\)") erg)
                           (match-string-no-properties 2 erg))
                          ((string-match "\\([0-9]\\.[0-9]+\\)" erg)
                           (substring erg 7 (1- (length erg)))))))
    (when (called-interactively-p 'any)
      (if version
          (when py-verbose-p (message "%s" version))
        (message "%s" "Could not detect Python on your system")))
    (string-to-number version)))

(defun py-python-current-environment ()
  "Return path of current Python installation."
  (interactive)
  (let* ((cmd (py-choose-shell))
         (denv (shell-command-to-string (concat "type " cmd)))
         (erg (substring denv (string-match "/" denv))))
    (when (called-interactively-p 'any)
      (if erg
          (message "%s" erg)
        (message "%s" "Could not detect Python on your system")))
    erg))

 ;; requested by org-mode still
(defalias 'py-toggle-shells 'py-choose-shell)

(defun py--cleanup-process-name (res)
  "Make res ready for use by ‘executable-find’.

Returns RES or substring of RES"
  (if (string-match "<" res)
      (substring res 0 (match-beginning 0))
    res))

(defalias 'py-which-shell 'py-choose-shell)
(defun py-choose-shell (&optional shell)
  "Return an appropriate executable as a string.

Does the following:
 - look for an interpreter with ‘py-choose-shell-by-shebang’
 - examine imports using ‘py--choose-shell-by-import’
 - look if Path/To/File indicates a Python version
 - if not successful, return default value of ‘py-shell-name’

When interactivly called, messages the SHELL name
Return nil, if no executable found."
  (interactive)
  ;; org-babel uses ‘py-toggle-shells’ with arg, just return it
  (or shell
      (let* (done
             (erg
              (cond ((and py-shell-name (executable-find py-shell-name))
                     py-shell-name)
                    (py-force-py-shell-name-p
                     (default-value 'py-shell-name))
                    (py-use-local-default
                     (if (not (string= "" py-shell-local-path))
                         (expand-file-name py-shell-local-path)
                       (message "Abort: ‘py-use-local-default’ is set to ‘t’ but ‘py-shell-local-path’ is empty. Maybe call ‘py-toggle-local-default-use’")))
                    ((and (not py-fast-process-p)
                          (comint-check-proc (current-buffer))
                          (setq done t)
                          (string-match "ython" (process-name (get-buffer-process (current-buffer)))))
                     (py--cleanup-process-name (process-name (get-buffer-process (current-buffer)))))
                    ((py-choose-shell-by-shebang))
                    ((py--choose-shell-by-import))
                    ((py-choose-shell-by-path))
                    (t (or
                        (and py-python-command (executable-find py-python-command) py-python-command)
                        "python3"))))
             (cmd (if (or
                       ;; comint-check-proc was succesful
                       done
                       py-edit-only-p)
                      erg
                    (executable-find erg))))
        (if cmd
            (when (called-interactively-p 'any)
              (message "%s" cmd))
          (when (called-interactively-p 'any) (message "%s" "Could not detect Python on your system. Maybe set ‘py-edit-only-p’?")))
        erg)))

(defun py--normalize-directory (directory)
  "Make sure DIRECTORY ends with a file-path separator char.

Returns DIRECTORY"
  (cond ((string-match (concat py-separator-char "$") directory)
         directory)
        ((not (string= "" directory))
         (concat directory py-separator-char))))

(defun py--normalize-pythonpath (pythonpath)
  "Make sure PYTHONPATH ends with a colon.

Returns PYTHONPATH"
  (let ((erg (cond ((string-match (concat path-separator "$") pythonpath)
                    pythonpath)
                   ((not (string= "" pythonpath))
                    (concat pythonpath path-separator))
                   (t pythonpath))))
    erg))

(defun py-install-directory-check ()
  "Do some sanity check for ‘py-install-directory’.

Returns t if successful."
  (interactive)
  (let ((erg (and (boundp 'py-install-directory) (stringp py-install-directory) (< 1 (length py-install-directory)))))
    (when (called-interactively-p 'any) (message "py-install-directory-check: %s" erg))
    erg))

(defun py--buffer-filename-remote-maybe (&optional file-name)
  "Argument FILE-NAME: the value of variable ‘buffer-file-name’."
  (let ((file-name (or file-name
                       (and
                        (ignore-errors (file-readable-p (buffer-file-name)))
                        (buffer-file-name)))))
    (if (and (featurep 'tramp) (tramp-tramp-file-p file-name))
        (tramp-file-name-localname
         (tramp-dissect-file-name file-name))
      file-name)))

(defun py-guess-py-install-directory ()
  "If `(locate-library \"python-mode\")' is not succesful.

Used only, if ‘py-install-directory’ is empty."
  (interactive)
  (cond (;; do not reset if it already exists
         py-install-directory)
        ;; ((locate-library "python-mode")
        ;;  (file-name-directory (locate-library "python-mode")))
        ((ignore-errors (string-match "python-mode" (py--buffer-filename-remote-maybe)))
         (file-name-directory (py--buffer-filename-remote-maybe)))
        (t (if
               (and (get-buffer "python-mode.el")
                    (set-buffer (get-buffer "python-mode.el"))
                    ;; (setq py-install-directory (ignore-errors (file-name-directory (buffer-file-name (get-buffer  "python-mode.el")))))
                    (buffer-file-name (get-buffer  "python-mode.el")))
               (setq py-install-directory (file-name-directory (buffer-file-name (get-buffer  "python-mode.el"))))
             (if
                 (and (get-buffer "python-components-mode.el")
                      (set-buffer (get-buffer "python-components-mode.el"))
                      (buffer-file-name (get-buffer  "python-components-mode.el")))
                 (setq py-install-directory (file-name-directory (buffer-file-name (get-buffer  "python-components-mode.el"))))))
           )))

(defun py--fetch-pythonpath ()
  "Consider settings of ‘py-pythonpath’."
  (if (string= "" py-pythonpath)
      (getenv "PYTHONPATH")
    (concat (py--normalize-pythonpath (getenv "PYTHONPATH")) py-pythonpath)))

(defun py-load-pymacs ()
  "Load Pymacs as delivered.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.ca"
  (interactive)
  (let ((pyshell (py-choose-shell))
        (path (py--fetch-pythonpath))
        (py-install-directory (cond ((string= "" py-install-directory)
                                     (py-guess-py-install-directory))
                                    (t (py--normalize-directory py-install-directory)))))
    (if (py-install-directory-check)
        (progn
          ;; If Pymacs has not been loaded before, prepend py-install-directory to
          ;; PYTHONPATH, so that the Pymacs delivered with python-mode is used.
          (unless (featurep 'pymacs)
            (setenv "PYTHONPATH" (concat
                                  (expand-file-name py-install-directory)
                                  (if path (concat path-separator path)))))
          (setenv "PYMACS_PYTHON" (if (string-match "IP" pyshell)
                                      "python"
                                    pyshell))
          (require 'pymacs))
      (error "‘py-install-directory’ not set, see INSTALL"))))

(when py-load-pymacs-p (py-load-pymacs))

(when (and py-load-pymacs-p (featurep 'pymacs))
  (defun py-load-pycomplete ()
    "Load Pymacs based pycomplete."
    (interactive)
    (let* ((path (py--fetch-pythonpath))
           (py-install-directory (cond ((string= "" py-install-directory)
                                        (py-guess-py-install-directory))
                                       (t (py--normalize-directory py-install-directory))))
           (pycomplete-directory (concat (expand-file-name py-install-directory) "completion")))
      (if (py-install-directory-check)
          (progn
            ;; If the Pymacs process is already running, augment its path.
            (when (and (get-process "pymacs") (fboundp 'pymacs-exec))
              (pymacs-exec (concat "sys.path.insert(0, '" pycomplete-directory "')")))
            (require 'pymacs)
            (setenv "PYTHONPATH" (concat
                                  pycomplete-directory
                                  (if path (concat path-separator path))))
            (push pycomplete-directory load-path)
            (require 'pycomplete)
            (add-hook 'python-mode-hook 'py-complete-initialize))
        (error "‘py-install-directory’ not set, see INSTALL")))))

(when (functionp 'py-load-pycomplete)
  (py-load-pycomplete))

(defun py-set-load-path ()
  "Include needed subdirs of ‘python-mode’ directory."
  (interactive)
  (let ((install-directory (py--normalize-directory py-install-directory)))
    (if py-install-directory
        (cond ((and (not (string= "" install-directory))(stringp install-directory))
               (push (expand-file-name install-directory) load-path)
               (push (concat (expand-file-name install-directory) "completion")  load-path)
               (push (concat (expand-file-name install-directory) "extensions")  load-path)
               (push (concat (expand-file-name install-directory) "test") load-path)
               )
              (t (error "Please set ‘py-install-directory’, see INSTALL")))
      (error "Please set ‘py-install-directory’, see INSTALL")))
  (when (called-interactively-p 'interactive) (message "%s" load-path)))

(defun py-count-lines (&optional beg end)
  "Count lines in accessible part until current line.

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=7115
Optional argument BEG specify beginning.
Optional argument END specify end."
  (interactive)
  (save-excursion
    (let ((count 0)
          (beg (or beg (point-min)))
          (end (or end (point))))
      (save-match-data
        (if (or (eq major-mode 'comint-mode)
                (eq major-mode 'py-shell-mode))
            (if
                (re-search-backward py-shell-prompt-regexp nil t 1)
                (goto-char (match-end 0))
              ;; (when py-debug-p (message "%s"  "py-count-lines: Do not see a prompt here"))
              (goto-char beg))
          (goto-char beg)))
      (while (and (< (point) end)(not (eobp)) (skip-chars-forward "^\n" end))
        (setq count (1+ count))
        (unless (or (not (< (point) end)) (eobp)) (forward-char 1)
                (setq count (+ count (abs (skip-chars-forward "\n" end))))))
      (when (bolp) (setq count (1+ count)))
      (when (and py-debug-p (called-interactively-p 'any)) (message "%s" count))
      count)))

(defun py--escape-doublequotes (start end)
  "Escape doublequotes in region by START END."
  (let ((end (copy-marker end)))
    (save-excursion
      (goto-char start)
      (while (and (not (eobp)) (< 0 (abs (skip-chars-forward "^\"" end))))
        (when (eq (char-after) ?\")
          (unless (py-escaped-p)
            (insert "\\")
            (forward-char 1)))))))

(defun py--escape-open-paren-col1 (start end)
  "Start from position START until position END."
  (goto-char start)
  (while (re-search-forward "^(" end t 1)
    (insert "\\")
    (end-of-line)))

(and py-company-pycomplete-p (require 'company-pycomplete))

(defun py-empty-line-p ()
  "Return t if cursor is at an empty line, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (looking-at py-empty-line-p-chars)))

(defun py-toggle-closing-list-dedents-bos (&optional arg)
  "Switch boolean variable ‘py-closing-list-dedents-bos’.

With optional ARG message state switched to"
  (interactive "p")
  (setq py-closing-list-dedents-bos (not py-closing-list-dedents-bos))
  (when arg (message "py-closing-list-dedents-bos: %s" py-closing-list-dedents-bos)))

(defun py-comint-delete-output ()
  "Delete all output from interpreter since last input.
Does not delete the prompt."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
        (replacement nil)
        (inhibit-read-only t))
    (save-excursion
      (let ((pmark (progn (goto-char (process-mark proc))
                          (forward-line 0)
                          (point-marker))))
        (delete-region comint-last-input-end pmark)
        (goto-char (process-mark proc))
        (setq replacement (concat "*** output flushed ***\n"
                                  (buffer-substring pmark (point))))
        (delete-region pmark (point))))
    ;; Output message and put back prompt
    (comint-output-filter proc replacement)))

(defun py-in-comment-p ()
  "Return the beginning of current line's comment, if inside. "
  (interactive)
  (let ((pps (parse-partial-sexp (point-min) (point))))
    (and (nth 4 pps) (nth 8 pps))))

;;
(defun py-in-string-or-comment-p ()
  "Returns beginning position if inside a string or comment, nil otherwise. "
  (or (nth 8 (parse-partial-sexp (point-min) (point)))
      (when (or (looking-at "\"") (looking-at "[ \t]*#[ \t]*"))
        (point))))

(when py-org-cycle-p
  (define-key python-mode-map (kbd "<backtab>") 'org-cycle))

(defun py-forward-buffer ()
  "A complementary form used by auto-generated commands.

Returns position reached if successful"
  (interactive)
  (unless (eobp)
    (goto-char (point-max))))

(defun py-backward-buffer ()
  "A complementary form used by auto-generated commands.

Returns position reached if successful"
  (interactive)
  (unless (bobp)
    (goto-char (point-min))))

(defun py--beginning-of-line-form ()
  "Internal use: Go to beginning of line following end of form.

Return position."
  (if (eobp)
      (point)
    (forward-line 1)
    (beginning-of-line)
    (point)))

(defun py--skip-to-semicolon-backward (&optional limit)
  "Fetch the beginning of statement after a semicolon.

Returns ‘t’ if point was moved"
  (prog1
      (< 0 (abs (skip-chars-backward "^;" (or limit (line-beginning-position)))))
    (skip-chars-forward " \t" (line-end-position))))

;; (defun py-forward-comment ()
;;   "Go to the end of comment at point."
;;   (let ((orig (point))
;;         last)
;;     (while (and (not (eobp)) (nth 4 (parse-partial-sexp (line-beginning-position) (point))) (setq last (line-end-position)))
;;       (forward-line 1)
;;       (end-of-line))
;;     (when
;;         (< orig last)
;;       (goto-char last)(point))))

(defun py-forward-comment ()
  "Go to the end of commented section at point."
  (interactive)
  (let (last)
    (while
        (and (not (eobp))
             (or
              (and comment-start (looking-at comment-start))
              (and comment-start-skip (looking-at comment-start-skip))
              (nth 4 (parse-partial-sexp (point-min) (point)))))
      (setq last (line-end-position))
      (forward-line 1)
      (skip-chars-forward " \t\r\n\f")
      (unless (or (eobp) (eq (point) last))
        (back-to-indentation)))
    (when last (goto-char last))))

(defun py--forward-string-maybe (&optional start)
  "Go to the end of string.

Expects START position of string
Return position of moved, nil otherwise."
  (let ((orig (point)))
    (when start (goto-char start)
          (when (looking-at "\"\"\"\\|'''")
            (goto-char (1- (match-end 0)))
            (forward-sexp))
          ;; maybe at the inner fence
          (when (looking-at "\"\"\\|''")
            (goto-char (match-end 0)))
          (and (< orig (point)) (point)))))

(defun py-load-skeletons ()
  "Load skeletons from extensions. "
  (interactive)
  (load (concat py-install-directory "/extensions/python-components-skeletons.el")))

(defun py--kill-emacs-hook ()
  "Delete files in ‘py-file-queue’.
These are Python temporary files awaiting execution."
  (when (and py-file-queue (listp py-file-queue))
    (dolist (ele py-file-queue)
      (ignore-errors (delete-file ele)))))

;; (defun py--kill-emacs-hook ()
;;   "Delete files in ‘py-file-queue’.
;; These are Python temporary files awaiting execution."
;;   (when py-file-queue
;;     (mapc #'(lambda (filename)
;;               (ignore-errors (delete-file filename)))
;;           py-file-queue)))

(add-hook 'kill-emacs-hook 'py--kill-emacs-hook)

;;  Add a designator to the minor mode strings
(or (assq 'py-pdbtrack-is-tracking-p minor-mode-alist)
    (push '(py-pdbtrack-is-tracking-p py-pdbtrack-minor-mode-string)
          minor-mode-alist))

(defun py--update-lighter (shell)
  "Select lighter for mode-line display"
  (setq py-modeline-display
        (cond
         ;; ((eq 2 (prefix-numeric-value argprompt))
         ;; py-python2-command-args)
         ((string-match "^[^-]+3" shell)
          py-python3-modeline-display)
         ((string-match "^[^-]+2" shell)
          py-python2-modeline-display)
         ((string-match "^.[Ii]" shell)
          py-ipython-modeline-display)
         ((string-match "^.[Jj]" shell)
          py-jython-modeline-display)
         (t
          python-mode-modeline-display))))

;;  bottle.py
;;  py   = sys.version_info
;;  py3k = py >= (3,0,0)
;;  py25 = py <  (2,6,0)
;;  py31 = (3,1,0) <= py < (3,2,0)

;;  sys.version_info[0]
(defun py-python-version (&optional executable verbose)
  "Returns versions number of a Python EXECUTABLE, string.

If no EXECUTABLE given, ‘py-shell-name’ is used.
Interactively output of ‘--version’ is displayed. "
  (interactive)
  (let* ((executable (or executable py-shell-name))
         (erg (py--string-strip (shell-command-to-string (concat executable " --version")))))
    (when (called-interactively-p 'any) (message "%s" erg))
    (unless verbose (setq erg (cadr (split-string erg))))
    erg))

(defun py-version ()
  "Echo the current version of ‘python-mode’ in the minibuffer."
  (interactive)
  (message "Using ‘python-mode’ version %s" py-version))

(declare-function compilation-shell-minor-mode "compile" (&optional arg))

(defun py--warn-tmp-files-left ()
  "Detect and warn about file of form \"py11046IoE\" in py-temp-directory."
  (let ((erg1 (file-readable-p (concat py-temp-directory py-separator-char (car (directory-files  py-temp-directory nil "py[[:alnum:]]+$"))))))
    (when erg1
      (message "py--warn-tmp-files-left: %s ?" (concat py-temp-directory py-separator-char (car (directory-files  py-temp-directory nil "py[[:alnum:]]*$")))))))

(defun py--fetch-indent-line-above (&optional orig)
  "Report the preceding indent. "
  (save-excursion
    (when orig (goto-char orig))
    (forward-line -1)
    (current-indentation)))

(defun py-continuation-offset (&optional arg)
  "Set if numeric ARG differs from 1. "
  (interactive "p")
  (and (numberp arg) (not (eq 1 arg)) (setq py-continuation-offset arg))
  (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" py-continuation-offset))
  py-continuation-offset)

(defun py-list-beginning-position (&optional start)
  "Return lists beginning position, nil if not inside.

Optional ARG indicates a start-position for ‘parse-partial-sexp’."
  (nth 1 (parse-partial-sexp (or start (point-min)) (point))))

(defun py-end-of-list-position (&optional arg)
  "Return end position, nil if not inside.

Optional ARG indicates a start-position for ‘parse-partial-sexp’."
  (interactive)
  (let* ((ppstart (or arg (point-min)))
         (erg (parse-partial-sexp ppstart (point)))
         (beg (nth 1 erg))
         end)
    (when beg
      (save-excursion
        (goto-char beg)
        (forward-list 1)
        (setq end (point))))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" end))
    end))

(defun py--in-comment-p ()
  "Return the beginning of current line's comment, if inside or at comment-start. "
  (save-restriction
    (widen)
    (let* ((pps (parse-partial-sexp (point-min) (point)))
           (erg (when (nth 4 pps) (nth 8 pps))))
      (unless erg
        (when (ignore-errors (looking-at (concat "[ \t]*" comment-start)))
          (setq erg (point))))
      erg)))

(defun py-in-triplequoted-string-p ()
  "Returns character address of start tqs-string, nil if not inside. "
  (interactive)
  (let* ((pps (parse-partial-sexp (point-min) (point)))
         (erg (when (and (nth 3 pps) (nth 8 pps))(nth 2 pps))))
    (save-excursion
      (unless erg (setq erg
                        (progn
                          (when (looking-at "\"\"\"\\|''''")
                            (goto-char (match-end 0))
                            (setq pps (parse-partial-sexp (point-min) (point)))
                            (when (and (nth 3 pps) (nth 8 pps)) (nth 2 pps)))))))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-in-string-p-intern (pps)
  (goto-char (nth 8 pps))
  (list (point) (char-after)(skip-chars-forward (char-to-string (char-after)))))

(defun py-in-string-p ()
  "if inside a double- triple- or singlequoted string,

If non-nil, return a list composed of
- beginning position
- the character used as string-delimiter (in decimal)
- and length of delimiter, commonly 1 or 3 "
  (interactive)
  (save-excursion
    (let* ((pps (parse-partial-sexp (point-min) (point)))
           (erg (when (nth 3 pps)
                  (py-in-string-p-intern pps))))
      (unless erg
        (when (looking-at "\"\\|'")
          (forward-char 1)
          (setq pps (parse-partial-sexp (line-beginning-position) (point)))
          (when (nth 3 pps)
            (setq erg (py-in-string-p-intern pps)))))
      erg)))

(defun py-toggle-local-default-use ()
  "Toggle boolean value of ‘py-use-local-default’.

Returns ‘py-use-local-default’

See also ‘py-install-local-shells’
Installing named virualenv shells is the preffered way,
as it leaves your system default unchanged."
  (interactive)
  (setq py-use-local-default (not py-use-local-default))
  (when (called-interactively-p 'any) (message "py-use-local-default set to %s" py-use-local-default))
  py-use-local-default)

(defun py--beginning-of-buffer-position ()
  "Provided for abstract reasons."
  (point-min))

(defun py--end-of-buffer-position ()
  "Provided for abstract reasons."
  (point-max))

(defun py-backward-comment (&optional pos)
  "Got to beginning of a commented section.

Start from POS if specified"
  (interactive)
  (let ((erg pos)
        last)
    (when erg (goto-char erg))
    (while (and (not (bobp)) (setq erg (py-in-comment-p)))
      (when (< erg (point))
        (goto-char erg)
        (setq last (point)))
      (skip-chars-backward " \t\r\n\f"))
    (when last (goto-char last))
    last))

(defun py-go-to-beginning-of-comment ()
  "Go to the beginning of current line's comment, if any.

From a programm use macro ‘py-backward-comment’ instead"
  (interactive)
  (let ((erg (py-backward-comment)))
    (when (and py-verbose-p (called-interactively-p 'any))
      (message "%s" erg))))

(defun py--up-decorators-maybe (indent)
  (let ((last (point)))
    (while (and (not (bobp))
                (py-backward-statement)
                (eq (current-indentation) indent)
                (if (looking-at py-decorator-re)
                    (progn (setq last (point)) nil)
                  t)))
    (goto-char last)))

(defun py-leave-comment-or-string-backward ()
  "If inside a comment or string, leave it backward."
  (interactive)
  (let ((pps
         (if (featurep 'xemacs)
             (parse-partial-sexp (point-min) (point))
           (parse-partial-sexp (point-min) (point)))))
    (when (nth 8 pps)
      (goto-char (1- (nth 8 pps))))))

;;  Decorator
(defun py-backward-decorator ()
  "Go to the beginning of a decorator.

Returns position if succesful"
  (interactive)
  (let ((orig (point)))
    (unless (bobp) (forward-line -1)
            (back-to-indentation)
            (while (and (progn (looking-at "@\\w+")(not (looking-at "\\w+")))
                        (not
                         ;; (py-empty-line-p)
                         (member (char-after) (list 9 10)))
                        (not (bobp))(forward-line -1))
              (back-to-indentation))
            (or (and (looking-at "@\\w+") (match-beginning 0))
                (goto-char orig)))))

(defun py-forward-decorator ()
  "Go to the end of a decorator.

Returns position if succesful"
  (interactive)
  (let ((orig (point)) erg)
    (unless (looking-at "@\\w+")
      (setq erg (py-backward-decorator)))
    (when erg
      (if
          (re-search-forward py-def-or-class-re nil t)
          (progn
            (back-to-indentation)
            (skip-chars-backward " \t\r\n\f")
            (py-leave-comment-or-string-backward)
            (skip-chars-backward " \t\r\n\f")
            (setq erg (point)))
        (goto-char orig)
        (end-of-line)
        (skip-chars-backward " \t\r\n\f")
        (when (ignore-errors (goto-char (py-list-beginning-position)))
          (forward-list))
        (when (< orig (point))
          (setq erg (point))))
      erg)))

(defun py-beginning-of-list-pps (&optional iact last ppstart orig done)
  "Go to the beginning of a list.

IACT - if called interactively
LAST - was last match.
Optional PPSTART indicates a start-position for ‘parse-partial-sexp’.
ORIG - consider original position or point.
DONE - transaktional argument
Return beginning position, nil if not inside."
  (interactive "p")
  (let* ((orig (or orig (point)))
         (ppstart (or ppstart (re-search-backward "^[a-zA-Z]" nil t 1) (point-min)))
         erg)
    (unless done (goto-char orig))
    (setq done t)
    (if
        (setq erg (nth 1 (if (featurep 'xemacs)
                             (parse-partial-sexp ppstart (point))
                           (parse-partial-sexp (point-min) (point)))))
        (progn
          (setq last erg)
          (goto-char erg)
          (py-beginning-of-list-pps iact last ppstart orig done))
      last)))

(defun py--record-list-error (pps)
  "When encountering a missing parenthesis, store its line, position.
‘py-verbose-p’  must be t"
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

(defun py--forward-regexp (regexp)
  "Search forward next regexp not in string or comment.

Return and move to match-beginning if successful"
  (save-match-data
    (let (erg)
      (while (and
              (setq erg (re-search-forward regexp nil 'move 1))
              (nth 8 (parse-partial-sexp (point-min) (point)))))
      (unless
          (nth 8 (parse-partial-sexp (point-min) (point)))
        erg))))

(defun py--forward-regexp-keep-indent-endform (last orig)
  (unless
      (nth 8 (parse-partial-sexp (point-min) (point)))
    (if last (goto-char last)
      (back-to-indentation))
    (and (< orig (point)) (point))))

(defun py--forward-regexp-keep-indent (regexp &optional indent)
  "Search forward next regexp not in string or comment.

Return and move to match-beginning if successful"
  (save-match-data
    (let ((indent (or indent (current-indentation)))
          (regexp (if (stringp regexp)
                      regexp
                    (symbol-value regexp)))
          (orig (point))
          last done)
      (forward-line 1)
      (beginning-of-line)
      (if (<= (current-indentation) indent)
          (py--forward-regexp-keep-indent-endform last orig)
          (while (and
                  (not done)
                  (re-search-forward regexp nil 'move 1)
                  (or (nth 8 (parse-partial-sexp (point-min) (point)))
                      (or (< indent (current-indentation))(setq done t))
                      (setq last (line-end-position)))))
          (py--forward-regexp-keep-indent-endform last orig)))))

(defun py-down-base (regexp &optional indent bol)
  (let ((indent (or indent (current-indentation))))
    (and (py--forward-regexp-keep-indent regexp indent)
         (progn
           (if bol
               (beginning-of-line)
             (back-to-indentation))
           (point)))))

(defun py--beginning-of-statement-p (&optional pps)
  "Return ‘t’, if cursor is at the beginning of a ‘statement’, nil otherwise."
  (interactive)
  (save-excursion
    (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
      (and (not (or (nth 8 pps) (nth 1 pps)))
           (looking-at py-statement-re)
           (looking-back "[^ \t]*" (line-beginning-position))
           (eq (current-column) (current-indentation))
           (eq (point) (progn (py-forward-statement) (py-backward-statement)))
           ))))

(defun py--beginning-of-statement-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘statement’, nil otherwise."
  (save-excursion
    (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
      (and (bolp)
           (not (or (nth 8 pps) (nth 1 pps)))
           (looking-at py-statement-re)
           (looking-back "[^ \t]*" (line-beginning-position))
           (eq (point) (progn (py-forward-statement-bol) (py-backward-statement-bol)))
           (point)))))

(defun py--refine-regexp-maybe (regexp)
  "Use a more specific regexp if possible. "
  (let ((regexpvalue (if (symbolp regexp)(symbol-value regexp) regexp)))
    (if (looking-at regexpvalue)
        (setq regexp
              (cond ((looking-at py-if-re)
                     'py-if-re)
                    ((looking-at py-try-re)
                     'py-try-re)
                    ((looking-at py-def-re)
                     'py-def-re)
                    ((looking-at py-class-re)
                     'py-class-re)
                    (t regexp)))
      regexp)))

(defun py-forward-clause-intern (indent)
  (end-of-line)
  (let (last)
    (while
        (and
         (py-forward-statement)
         (< indent (current-indentation))
         (setq last (point))))
    (when last (goto-char last))))

(defun py--backward-empty-lines-or-comment ()
  "Travel backward"
  (while
      (or (< 0 (abs (skip-chars-backward " \t\r\n\f")))
          (py-backward-comment))))

(defun py--down-end-form ()
  "Return position."
  (progn (py--backward-empty-lines-or-comment)
         (point)))

(defun py--which-delay-process-dependent (buffer)
  "Call a ‘py-ipython-send-delay’ or ‘py-python-send-delay’ according to process"
  (if (string-match "^.[IJ]" buffer)
      py-ipython-send-delay
    py-python-send-delay))

(defun py-temp-file-name (strg)
  (let* ((temporary-file-directory
          (if (file-remote-p default-directory)
              (concat (file-remote-p default-directory) "/tmp")
            temporary-file-directory))
         (temp-file-name (make-temp-file "py")))

    (with-temp-file temp-file-name
      (insert strg)
      (delete-trailing-whitespace))
    temp-file-name))

(defun py--fetch-error (output-buffer &optional origline filename)
  "Highlight exceptions found in BUF.

If an exception occurred return error-string, otherwise return nil.
BUF must exist.

Indicate LINE if code was not run from a file,
thus remember ORIGLINE of source buffer"
  (with-current-buffer output-buffer
    (when py-debug-p (switch-to-buffer (current-buffer)))
    ;; (setq py-error (buffer-substring-no-properties (point) (point-max)))
    (goto-char (point-max))
    (when (re-search-backward "File \"\\(.+\\)\", line \\([0-9]+\\)\\(.*\\)$" nil t)
      (when (and filename (re-search-forward "File \"\\(.+\\)\", line \\([0-9]+\\)\\(.*\\)$" nil t)
                 (replace-match filename nil nil nil 1))
        (when (and origline (re-search-forward "line \\([0-9]+\\)\\(.*\\)$" (line-end-position) t 1))
          (replace-match origline nil nil nil 2)))
      (setq py-error (buffer-substring-no-properties (point) (point-max))))
        py-error))

(defun py--fetch-result (buffer  &optional limit cmd)
  "CMD: some shells echo the command in output-buffer
Delete it here"
  (when py-debug-p (message "(current-buffer): %s" (current-buffer)))
  (cond (python-mode-v5-behavior-p
         (with-current-buffer buffer
           (py--string-trim (buffer-substring-no-properties (point-min) (point-max)) nil "\n")))
        ((and cmd limit (< limit (point-max)))
         (replace-regexp-in-string cmd "" (py--string-trim (replace-regexp-in-string py-shell-prompt-regexp "" (buffer-substring-no-properties limit (point-max))))))
        (t (when (and limit (< limit (point-max)))
             (py--string-trim (replace-regexp-in-string py-shell-prompt-regexp "" (buffer-substring-no-properties limit (point-max))))))))

(defun py--postprocess (output-buffer origline limit &optional cmd filename)
  "Provide return values, check result for error, manage windows.

According to OUTPUT-BUFFER ORIGLINE ORIG"
  ;; py--fast-send-string does not set origline
  (when (or py-return-result-p py-store-result-p)
    (with-current-buffer output-buffer
      (when py-debug-p (switch-to-buffer (current-buffer)))
      (sit-for (py--which-delay-process-dependent (prin1-to-string output-buffer)))
      ;; (catch 'py--postprocess
      (setq py-result (py--fetch-result output-buffer limit cmd))
      ;; (throw 'py--postprocess (error "py--postprocess failed"))
      ;;)
      (if (and py-result (not (string= "" py-result)))
          (if (string-match "^Traceback" py-result)
              (if filename
                  (setq py-error py-result)
                (progn
                  (with-temp-buffer
                    (insert py-result)
                    (sit-for 0.1 t)
                    (setq py-error (py--fetch-error origline filename)))))
            (when py-store-result-p
              (kill-new py-result))
            (when py-verbose-p (message "py-result: %s" py-result))
            py-result)
        (when py-verbose-p (message "py--postprocess: %s" "Do not see any result"))))))

(defun py-fetch-py-master-file ()
  "Lookup if a ‘py-master-file’ is specified.

See also doku of variable ‘py-master-file’"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^ *# Local Variables:" nil (quote move) 1)
        (when
            (re-search-forward (concat "^\\( *# py-master-file: *\\)\"\\([^ \t]+\\)\" *$") nil t 1)
          (setq py-master-file (match-string-no-properties 2))))))
  ;; (when (called-interactively-p 'any) (message "%s" py-master-file))
  )

(defun py-ipython--which-version (shell)
  "Returns IPython version as string"
  (shell-command-to-string (concat (downcase (replace-regexp-in-string  "[[:punct:]+]" "" shell)) " -V")))

(defun py--provide-command-args (shell fast-process)
  "Unbuffered WRT fast-process"
  (let ((erg
         (delq nil
               (cond
                ;; ((eq 2 (prefix-numeric-value argprompt))
                ;; py-python2-command-args)
                ((string-match "^[Ii]" shell)
                 (if (string-match "^[0-4]" (py-ipython--which-version shell))
                     (remove "--simple-prompt"  py-ipython-command-args)
                   (if (member "--simple-prompt"  py-ipython-command-args)
                       py-ipython-command-args
                     (cons "--simple-prompt"  py-ipython-command-args))))
                ((string-match "^[^-]+3" shell)
                 py-python3-command-args)
                ((string-match "^[jy]" shell)
                 py-jython-command-args)
                (t
                 py-python-command-args)))))
    (if (and fast-process (not (member "-u" erg)))
        (cons "-u" erg)
      erg)))

;; This and other stuff from python.el
(defun py-info-encoding-from-cookie ()
  "Detect current buffer's encoding from its coding cookie.
Returns the encoding as a symbol."
  (let ((first-two-lines
         (save-excursion
           (save-restriction
             (widen)
             (goto-char (point-min))
             (forward-line 2)
             (buffer-substring-no-properties
              (point)
              (point-min))))))
    (when (string-match
           ;; (py-rx coding-cookie)
           "^#[[:space:]]*\\(?:coding[:=][[:space:]]*\\(?1:\\(?:[[:word:]]\\|-\\)+\\)\\|-\\*-[[:space:]]*coding:[[:space:]]*\\(?1:\\(?:[[:word:]]\\|-\\)+\\)[[:space:]]*-\\*-\\|vim:[[:space:]]*set[[:space:]]+fileencoding[[:space:]]*=[[:space:]]*\\(?1:\\(?:[[:word:]]\\|-\\)+\\)[[:space:]]*:\\)"
           first-two-lines)
      (intern (match-string-no-properties 1 first-two-lines)))))

(defun py-info-encoding ()
  "Return encoding for file.
Try ‘py-info-encoding-from-cookie’, if none is found then
default to utf-8."
  (or (py-info-encoding-from-cookie)
      'utf-8))

(defun py-indentation-of-statement ()
  "Returns the indenation of the statement at point. "
  (interactive)
  (let ((erg (save-excursion
               (back-to-indentation)
               (or (py--beginning-of-statement-p)
                   (py-backward-statement))
               (current-indentation))))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py--filter-result (strg)
  "Set ‘py-result’ according to ‘py-fast-filter-re’.

Remove trailing newline"
  (py--string-trim
   (replace-regexp-in-string
    py-fast-filter-re
    ""
    (ansi-color-filter-apply strg))))

(defun py--cleanup-shell (orig buffer)
  (with-current-buffer buffer
    (with-silent-modifications
      (sit-for py-python3-send-delay)
      (when py-debug-p (switch-to-buffer (current-buffer)))
      (delete-region orig (point-max)))))

(defun py-shell--save-temp-file (strg)
  (let* ((temporary-file-directory
          (if (file-remote-p default-directory)
              (concat (file-remote-p default-directory) "/tmp")
            temporary-file-directory))
         (temp-file-name (make-temp-file "py"))
         (coding-system-for-write (py-info-encoding)))
    (with-temp-file temp-file-name
      (insert strg)
      (delete-trailing-whitespace))
    temp-file-name))

(defun py--get-process (&optional argprompt args dedicated shell buffer)
  "Get appropriate Python process for current buffer and return it.

Optional ARGPROMPT DEDICATED SHELL BUFFER"
  (interactive)
  (or (and buffer (get-buffer-process buffer))
      (get-buffer-process (current-buffer))
      (get-buffer-process (py-shell argprompt args dedicated shell buffer))))

(defun py-shell-send-file (file-name &optional process temp-file-name
                                     delete)
  "Send FILE-NAME to Python PROCESS.

If TEMP-FILE-NAME is passed then that file is used for processing
instead, while internally the shell will continue to use
FILE-NAME.  If TEMP-FILE-NAME and DELETE are non-nil, then
TEMP-FILE-NAME is deleted after evaluation is performed.  When
optional argument."
  (interactive
   (list
    (read-file-name "File to send: ")))
  (let* ((proc (or process (py--get-process)))
         (encoding (with-temp-buffer
                     (insert-file-contents
                      (or temp-file-name file-name))
                     (py-info-encoding)))
         (file-name (expand-file-name (file-local-name file-name)))
         (temp-file-name (when temp-file-name
                           (expand-file-name
                            (file-local-name temp-file-name)))))
    (py-shell-send-string
     (format
      (concat
       "import codecs, os;"
       "__pyfile = codecs.open('''%s''', encoding='''%s''');"
       "__code = __pyfile.read().encode('''%s''');"
       "__pyfile.close();"
       (when (and delete temp-file-name)
         (format "os.remove('''%s''');" temp-file-name))
       "exec(compile(__code, '''%s''', 'exec'));")
      (or temp-file-name file-name) encoding encoding file-name)
     proc)))

(defun py-shell-send-string (strg &optional process)
  "Send STRING to Python PROCESS.

Uses ‘comint-send-string’."
  (interactive
   (list (read-string "Python command: ") nil t))
  (let ((process (or process (py--get-process))))
    (if (string-match ".\n+." strg)   ;Multiline.
        (let* ((temp-file-name (py-shell--save-temp-file strg))
               (file-name (or (buffer-file-name) temp-file-name)))
          (py-shell-send-file file-name process temp-file-name t))
      (comint-send-string process strg)
      (when (or (not (string-match "\n\\'" strg))
                (string-match "\n[ \t].*\n?\\'" strg))
        (comint-send-string process "\n")))))

(defun py-fast-process (&optional buffer)
  "Connect am (I)Python process suitable for large output.

Output buffer displays \"Fast\"  by default
It is not in interactive, i.e. comint-mode,
as its bookkeepings seem linked to the freeze reported by lp:1253907"
  (interactive)
  (let ((this-buffer
         (set-buffer (or (and buffer (get-buffer-create buffer))
                         (get-buffer-create py-shell-name)))))
    (let ((proc (start-process py-shell-name this-buffer py-shell-name)))
      (with-current-buffer this-buffer
        (erase-buffer))
      proc)))

(defun py-proc (&optional argprompt)
  "Return the current Python process.

Start a new process if necessary. "
  (interactive "P")
  (let ((erg
         (cond ((comint-check-proc (current-buffer))
                (get-buffer-process (buffer-name (current-buffer))))
               (t (py-shell argprompt)))))
    erg))

(defun py-process-file (filename &optional output-buffer error-buffer)
  "Process \"python FILENAME\".

Optional OUTPUT-BUFFER and ERROR-BUFFER might be given."
  (interactive "fDatei:")
  (let ((coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        (output-buffer (or output-buffer (make-temp-name "py-process-file-output")))
        (pcmd (py-choose-shell)))
    (unless (buffer-live-p output-buffer)
      (set-buffer (get-buffer-create output-buffer)))
    (shell-command (concat pcmd " " filename) output-buffer error-buffer)
    (when py-switch-buffers-on-execute-p (switch-to-buffer output-buffer))))

(defun py-remove-overlays-at-point ()
  "Remove overlays as set when ‘py-highlight-error-source-p’ is non-nil."
  (interactive "*")
  (delete-overlay (car (overlays-at (point)))))

(defun py--jump-to-exception-intern (act exception-buffer origline)
  (let (erg)
    (set-buffer exception-buffer)
    (goto-char (point-min))
    (forward-line (1- origline))
    (and (search-forward act (line-end-position) t)
         (and py-verbose-p (message "exception-buffer: %s on line %d" py-exception-buffer origline))
         (and py-highlight-error-source-p
              (setq erg (make-overlay (match-beginning 0) (match-end 0)))
              (overlay-put erg
                           'face 'highlight)))))

(defun py--jump-to-exception (perr origline &optional file)
  "Jump to the PERR Python code at ORIGLINE in optional FILE."
  (with-silent-modifications
    (let (
          ;; (inhibit-point-motion-hooks t)
          (file (or file (car perr)))
          (act (nth 2 perr)))
      (cond ((and py-exception-buffer
                  (buffer-live-p py-exception-buffer))
             ;; (pop-to-buffer procbuf)
             (py--jump-to-exception-intern act py-exception-buffer origline))
            ((ignore-errors (file-readable-p file))
             (find-file file)
             (py--jump-to-exception-intern act (get-buffer (file-name-nondirectory file)) origline))
            ((buffer-live-p (get-buffer file))
             (set-buffer file)
             (py--jump-to-exception-intern act file origline))
            (t (setq file (find-file (read-file-name "Exception file: "
                                                     nil
                                                     file t)))
               (py--jump-to-exception-intern act file origline))))))

(defun py-goto-exception (&optional file line)
  "Go to FILE and LINE indicated by the traceback."
  (interactive)
  (let ((file file)
        (line line))
    (unless (and file line)
      (save-excursion
        (beginning-of-line)
        (if (looking-at py-traceback-line-re)
            (setq file (substring-no-properties (match-string 1))
                  line (string-to-number (match-string 2))))))
    (if (not file)
        (error "Not on a traceback line"))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun py--find-next-exception (start buffer searchdir errwhere)
  "Find the next Python exception and jump to the code that caused it.
START is the buffer position in BUFFER from which to begin searching
for an exception.  SEARCHDIR is a function, either
‘re-search-backward’ or ‘re-search-forward’ indicating the direction
to search.  ERRWHERE is used in an error message if the limit (top or
bottom) of the trackback stack is encountered."
  (let (file line)
    (save-excursion
      (with-current-buffer buffer
        (goto-char start)
        (if (funcall searchdir py-traceback-line-re nil t)
            (setq file (match-string 1)
                  line (string-to-number (match-string 2))))))
    (if (and file line)
        (py-goto-exception file line)
      (error "%s of traceback" errwhere))))

(defun py-down-exception (&optional bottom)
  "Go to the next line down in the traceback.
With \\[univeral-argument] (programmatically, optional argument
BOTTOM), jump to the bottom (innermost) exception in the exception
stack."
  (interactive "P")
  (let* ((buffer py-output-buffer))
    (if bottom
        (py--find-next-exception 'eob buffer 're-search-backward "Bottom")
      (py--find-next-exception 'eol buffer 're-search-forward "Bottom"))))

(defun py-up-exception (&optional top)
  "Go to the previous line up in the traceback.
With \\[universal-argument] (programmatically, optional argument TOP)
jump to the top (outermost) exception in the exception stack."
  (interactive "P")
  (let* ((buffer py-output-buffer))
    (if top
        (py--find-next-exception 'bob buffer 're-search-forward "Top")
      (py--find-next-exception 'bol buffer 're-search-backward "Top"))))

;; ;
;;  obsolete by py--fetch-result
;;  followed by py--fetch-error
;;  still used by py--execute-ge24.3

(defun py--find-next-exception-prepare (direction start)
  "According to DIRECTION and START setup exception regexps.

Depends from kind of Python shell."
  (let* ((name (get-process (substring (buffer-name (current-buffer)) 1 -1)))
         (buffer (cond (name (buffer-name (current-buffer)))
                       ((buffer-live-p (get-buffer py-output-buffer))
                        py-output-buffer)
                       (py-last-exeption-buffer (buffer-name py-last-exeption-buffer))
                       (t (error "Do not see exeption buffer")))))
    (when buffer (set-buffer (get-buffer buffer)))
    (if (eq direction 'up)
        (if (string= start "TOP")
            (py--find-next-exception 'bob buffer 're-search-forward "Top")
          (py--find-next-exception 'bol buffer 're-search-backward "Top"))
      (if (string= start "BOTTOM")
          (py--find-next-exception 'eob buffer 're-search-backward "Bottom")
        (py--find-next-exception 'eol buffer 're-search-forward "Bottom")))))

(defun py-shell-comint-end-of-output-p (output)
  "Return non-nil if OUTPUT ends with input prompt."
  (ignore-errors (string-match
                  ;; XXX: It seems on macOS an extra carriage return is attached
                  ;; at the end of output, this handles that too.
                  (concat
                   "\r?\n?"
                   ;; Remove initial caret from calculated regexp
                   (ignore-errors (replace-regexp-in-string
                                   (rx string-start ?^) ""
                                   py-shell--prompt-calculated-input-regexp))
                   (rx eos))
                  output)))

(defun py-comint-postoutput-scroll-to-bottom (output)
  "Faster version of ‘comint-postoutput-scroll-to-bottom’.
Avoids ‘recenter’ calls until OUTPUT is completely sent."
  (when (and (not (string= "" output))
             (py-shell-comint-end-of-output-p
              (ansi-color-filter-apply output)))
    (comint-postoutput-scroll-to-bottom output))
  output)

(defmacro py-shell--add-to-path-with-priority (pathvar paths)
  "Modify PATHVAR and ensure PATHS are added only once at beginning."
  `(dolist (path (reverse ,paths))
     (setq ,pathvar (cons path (cl-delete path ,pathvar :test #'string=)))))

(defun py-shell-tramp-refresh-remote-path (vec paths)
  "Update VEC's remote-path giving PATHS priority."
  (cl-assert (featurep 'tramp))
  (declare-function tramp-set-remote-path "tramp-sh")
  (declare-function tramp-set-connection-property "tramp-cache")
  (declare-function tramp-get-connection-property "tramp-cache")
  (let ((remote-path (tramp-get-connection-property vec "remote-path" nil)))
    (when remote-path
      ;; FIXME: This part of the Tramp code still knows about Python!
      (py-shell--add-to-path-with-priority remote-path paths)
      (tramp-set-connection-property vec "remote-path" remote-path)
      (tramp-set-remote-path vec))))

(defun py-shell-tramp-refresh-process-environment (vec env)
  "Update VEC's process environment with ENV."
  ;; Stolen from ‘tramp-open-connection-setup-interactive-shell’.
  (let ((env (append (when (fboundp 'tramp-get-remote-locale)
                       ;; Emacs<24.4 compat.
                       (list (tramp-get-remote-locale vec)))
                     (copy-sequence env)))
        (tramp-end-of-heredoc
         (if (boundp 'tramp-end-of-heredoc)
             tramp-end-of-heredoc
           (md5 tramp-end-of-output)))
        unset vars item)
    (while env
      (setq item (split-string (car env) "=" 'omit))
      (setcdr item (mapconcat 'identity (cdr item) "="))
      (if (and (stringp (cdr item)) (not (string-equal (cdr item) "")))
          (push (format "%s %s" (car item) (cdr item)) vars)
        (push (car item) unset))
      (setq env (cdr env)))
    (when vars
      (tramp-send-command
       vec
       (format "while read var val; do export $var=$val; done <<'%s'\n%s\n%s"
               tramp-end-of-heredoc
               (mapconcat 'identity vars "\n")
               tramp-end-of-heredoc)
       t))
    (when unset
      (tramp-send-command
       vec (format "unset %s" (mapconcat 'identity unset " ")) t))))

(defun py-shell-calculate-pythonpath ()
  "Calculate the PYTHONPATH using ‘python-shell-extra-pythonpaths’."
  (let ((pythonpath
         (split-string
          (or (getenv "PYTHONPATH") "") path-separator 'omit)))
    (py-shell--add-to-path-with-priority
     pythonpath py-shell-extra-pythonpaths)
    (mapconcat #'identity pythonpath path-separator)))

(defun py-shell-calculate-exec-path ()
  "Calculate ‘exec-path’.
Prepends ‘py-shell-exec-path’ and adds the binary directory
for virtualenv if ‘py-shell-virtualenv-root’ is set - this
will use the python interpreter from inside the virtualenv when
starting the shell.  If ‘default-directory’ points to a remote host,
the returned value appends ‘py-shell-remote-exec-path’ instead
of ‘exec-path’."
  (let ((new-path (copy-sequence
                   (if (file-remote-p default-directory)
                       py-shell-remote-exec-path
                     exec-path)))

        ;; Windows and POSIX systems use different venv directory structures
        (virtualenv-bin-dir (if (eq system-type 'windows-nt) "Scripts" "bin")))
    (py-shell--add-to-path-with-priority
     new-path py-shell-exec-path)
    (if (not py-shell-virtualenv-root)
        new-path
      (py-shell--add-to-path-with-priority
       new-path
       (list (expand-file-name virtualenv-bin-dir py-shell-virtualenv-root)))
      new-path)))

(defun py-shell-calculate-process-environment ()
  "Calculate ‘process-environment’ or ‘tramp-remote-process-environment’.
Prepends ‘py-shell-process-environment’, sets extra
pythonpaths from ‘py-shell-extra-pythonpaths’ and sets a few
virtualenv related vars.  If ‘default-directory’ points to a
remote host, the returned value is intended for
‘tramp-remote-process-environment’."
  (let* ((remote-p (file-remote-p default-directory))
         (process-environment (if remote-p
                                  tramp-remote-process-environment
                                process-environment))
         (virtualenv (when py-shell-virtualenv-root
                       (directory-file-name py-shell-virtualenv-root))))
    (dolist (env py-shell-process-environment)
      (pcase-let ((`(,key ,value) (split-string env "=")))
        (setenv key value)))
    (when py-shell-unbuffered
      (setenv "PYTHONUNBUFFERED" "1"))
    (when py-shell-extra-pythonpaths
      (setenv "PYTHONPATH" (py-shell-calculate-pythonpath)))
    (if (not virtualenv)
        process-environment
      (setenv "PYTHONHOME" nil)
      (setenv "VIRTUAL_ENV" virtualenv))
    process-environment))

(defun py-shell-prompt-detect ()
  "Detect prompts for the current interpreter.
When prompts can be retrieved successfully from the
interpreter run with
‘py-python-command-args’, returns a list of
three elements, where the first two are input prompts and the
last one is an output prompt.  When no prompts can be detected
shows a warning with instructions to avoid hangs and returns nil.
When ‘py-shell-prompt-detect-p’ is nil avoids any
detection and just returns nil."
  (when py-shell-prompt-detect-p
    (python-shell-with-environment
      (let* ((code (concat
                    "import sys\n"
                    "ps = [getattr(sys, 'ps%s' % i, '') for i in range(1,4)]\n"
                    ;; JSON is built manually for compatibility
                    "ps_json = '\\n[\"%s\", \"%s\", \"%s\"]\\n' % tuple(ps)\n"
                    "print (ps_json)\n"
                    "sys.exit(0)\n"))
             ;; (interpreter py-shell-name)
             ;; (interpreter-arg py-python-command-args)
             (output
              (with-temp-buffer
                ;; TODO: improve error handling by using
                ;; ‘condition-case’ and displaying the error message to
                ;; the user in the no-prompts warning.
                (ignore-errors
                  (let ((code-file
                         ;; Python 2.x on Windows does not handle
                         ;; carriage returns in unbuffered mode.
                         (let ((inhibit-eol-conversion (getenv "PYTHONUNBUFFERED")))
                           (py-shell--save-temp-file code))))
                    (unwind-protect
                        ;; Use ‘process-file’ as it is remote-host friendly.
                        (process-file
                         py-shell-name
                         code-file
                         '(t nil)
                         nil
                         py-python-command-args)
                      ;; Try to cleanup
                      (delete-file code-file))))
                (buffer-string)))
             (prompts
              (catch 'prompts
                (dolist (line (split-string output "\n" t))
                  (let ((res
                         ;; Check if current line is a valid JSON array
                         (and (string= (substring line 0 2) "[\"")
                              (ignore-errors
                                ;; Return prompts as a list, not vector
                                (append (json-read-from-string line) nil)))))
                    ;; The list must contain 3 strings, where the first
                    ;; is the input prompt, the second is the block
                    ;; prompt and the last one is the output prompt.  The
                    ;; input prompt is the only one that can not be empty.
                    (when (and (= (length res) 3)
                               (cl-every #'stringp res)
                               (not (string= (car res) "")))
                      (throw 'prompts res))))
                nil)))
        (if (not prompts)
            (lwarn
             '(python py-shell-prompt-regexp)
             :warning
             (concat
              "Python shell prompts cannot be detected.\n"
              "If your emacs session hangs when starting python shells\n"
              "recover with ‘keyboard-quit’ and then try fixing the\n"
              "interactive flag for your interpreter by adjusting the\n"
              "‘py-python-command-args’ or add regexps\n"
              "matching shell prompts in the directory-local friendly vars:\n"
              "  + ‘py-shell-prompt-regexp’\n"
              "  + ‘py-shell-input-prompt-2-regexp’\n"
              "  + ‘py-shell-prompt-output-regexp’\n"
              "Or alternatively in:\n"
              "  + ‘py-shell-input-prompt-regexps’\n"
              "  + ‘py-shell-prompt-output-regexps’"))
          prompts)))))

(defun python-util-valid-regexp-p (regexp)
  "Return non-nil if REGEXP is valid."
  (ignore-errors (string-match regexp "") t))

(defun py-shell-prompt-validate-regexps ()
  "Validate all user provided regexps for prompts.
Signals ‘user-error’ if any of these vars contain invalid
regexps: ‘py-shell-prompt-regexp’,
‘py-shell-input-prompt-2-regexp’,
‘py-shell-prompt-pdb-regexp’,
‘py-shell-prompt-output-regexp’,
‘py-shell-input-prompt-regexps’,
‘py-shell-prompt-output-regexps’."
  (dolist (symbol (list 'py-shell-input-prompt-1-regexp
                        'py-shell-prompt-output-regexps
                        'py-shell-input-prompt-2-regexp
                        'py-shell-prompt-pdb-regexp))
    (dolist (regexp (let ((regexps (symbol-value symbol)))
                      (if (listp regexps)
                          regexps
                        (list regexps))))
      (when (not (python-util-valid-regexp-p regexp))
        (user-error "Invalid regexp %s in ‘%s’"
                    regexp symbol)))))

(defun py-shell-prompt-set-calculated-regexps ()
  "Detect and set input and output prompt regexps.

Build and set the values for input- and output-prompt regexp
using the values from ‘py-shell-prompt-regexp’,
‘py-shell-input-prompt-2-regexp’, ‘py-shell-prompt-pdb-regexp’,
‘py-shell-prompt-output-regexp’, ‘py-shell-input-prompt-regexps’,
 and detected prompts from ‘py-shell-prompt-detect’."
  (when (not (and py-shell--prompt-calculated-input-regexp
                  py-shell--prompt-calculated-output-regexp))
    (let* ((detected-prompts (py-shell-prompt-detect))
           (input-prompts nil)
           (output-prompts nil)
           (build-regexp
            (lambda (prompts)
              (concat "^\\("
                      (mapconcat #'identity
                                 (sort prompts
                                       (lambda (a b)
                                         (let ((length-a (length a))
                                               (length-b (length b)))
                                           (if (= length-a length-b)
                                               (string< a b)
                                             (> (length a) (length b))))))
                                 "\\|")
                      "\\)"))))
      ;; Validate ALL regexps
      (py-shell-prompt-validate-regexps)
      ;; Collect all user defined input prompts
      (dolist (prompt (append py-shell-input-prompt-regexps
                              (list py-shell-input-prompt-2-regexp
                                    py-shell-prompt-pdb-regexp)))
        (cl-pushnew prompt input-prompts :test #'string=))
      ;; Collect all user defined output prompts
      (dolist (prompt (cons py-shell-prompt-output-regexp
                            py-shell-prompt-output-regexps))
        (cl-pushnew prompt output-prompts :test #'string=))
      ;; Collect detected prompts if any
      (when detected-prompts
        (dolist (prompt (butlast detected-prompts))
          (setq prompt (regexp-quote prompt))
          (cl-pushnew prompt input-prompts :test #'string=))
        (setq py-shell--block-prompt (nth 1 detected-prompts))
        (cl-pushnew (regexp-quote
                     (car (last detected-prompts)))
                    output-prompts :test #'string=))
      ;; Set input and output prompt regexps from collected prompts
      (setq py-shell--prompt-calculated-input-regexp
            (funcall build-regexp input-prompts)
            py-shell--prompt-calculated-output-regexp
            (funcall build-regexp output-prompts)))))

(defun py-shell-output-filter (strg)
  "Filter used in ‘py-shell-send-string-no-output’ to grab output.
STRING is the output received to this point from the process.
This filter saves received output from the process in
‘py-shell-output-filter-buffer’ and stops receiving it after
detecting a prompt at the end of the buffer."
  (let ((py-shell--prompt-calculated-output-regexp
         (or py-shell--prompt-calculated-output-regexp (py-shell-prompt-set-calculated-regexps))))
    (setq
     strg (ansi-color-filter-apply strg)
     py-shell-output-filter-buffer
     (concat py-shell-output-filter-buffer strg))
    (when (py-shell-comint-end-of-output-p
           py-shell-output-filter-buffer)
      ;; Output ends when ‘py-shell-output-filter-buffer’ contains
      ;; the prompt attached at the end of it.
      (setq py-shell-output-filter-in-progress nil
            py-shell-output-filter-buffer
            (substring py-shell-output-filter-buffer
                       0 (match-beginning 0)))
      (when (string-match
             py-shell--prompt-calculated-output-regexp
             py-shell-output-filter-buffer)
        ;; Some shells, like IPython might append a prompt before the
        ;; output, clean that.
        (setq py-shell-output-filter-buffer
              (substring py-shell-output-filter-buffer (match-end 0)))))
    ""))

(defun py--fast-send-string-no-output-intern (strg proc limit output-buffer no-output)
  (let (erg)
    (with-current-buffer output-buffer
      ;; (when py-debug-p (switch-to-buffer (current-buffer)))
      ;; (erase-buffer)
      (process-send-string proc strg)
      (or (string-match "\n$" strg)
          (process-send-string proc "\n")
          (goto-char (point-max))
          )
      (cond (no-output
             (delete-region (field-beginning) (field-end))
             ;; (erase-buffer)
             ;; (delete-region (point-min) (line-beginning-position))
             )
            (t
             (if
                 (setq erg (py--fetch-result output-buffer limit strg))
                 (setq py-result (py--filter-result erg))
               (dotimes (_ 3) (unless (setq erg (py--fetch-result output-buffer limit))(sit-for 1 t)))
               (or (py--fetch-result output-buffer limit))
               (error "py--fast-send-string-no-output-intern: py--fetch-result: no result")))))))

(defun py-execute-string (strg &optional process result no-output orig output-buffer fast argprompt args dedicated shell exception-buffer split switch internal)
  "Evaluate STRG in Python PROCESS.

With optional Arg PROCESS send to process.
With optional Arg RESULT store result in var ‘py-result’, also return it.
With optional Arg NO-OUTPUT do not display any output
With optional Arg ORIG deliver original position.
With optional Arg OUTPUT-BUFFER specify output-buffer"
  (interactive "sPython command: ")
  (save-excursion
    (let* ((buffer (or output-buffer
                       (and process (buffer-name (process-buffer process)))
                       (buffer-name
                        (py-shell argprompt args dedicated shell output-buffer fast exception-buffer split switch internal))))
           (proc (or process (get-buffer-process buffer)))
           (orig (or orig (point)))
           (limit (ignore-errors (marker-position (process-mark proc)))))
      (unless (eq 1 (length (window-list))) (window-configuration-to-register py--windows-config-register))
      (cond ((and no-output fast)
             (py--fast-send-string-no-output-intern strg proc limit buffer no-output))
            (no-output
             (py-send-string-no-output strg proc))
            ((and (string-match ".\n+." strg) (string-match "^[Ii]"
                                                            ;; (buffer-name buffer)
                                                            buffer
                                                            ))  ;; multiline
             (let* ((temp-file-name (py-temp-file-name strg))
                    (file-name (or (buffer-file-name) temp-file-name)))
               (py-execute-file file-name proc)))
            (t
             (comint-send-string proc strg)
             (when (or (not (string-match "\n\\'" strg))
                       (string-match "\n[ \t].*\n?\\'" strg))
               (comint-send-string proc "\n"))
             (cond (result
                    ;; (sit-for py-python-send-delay)
                    (sit-for py-python-send-delay)
                    (setq py-result (py--fetch-result buffer limit strg)))
                   (no-output
                    (and orig (py--cleanup-shell orig buffer))))))
      ;; (message "py-execute-string; current-buffer: %s" (current-buffer))
      (if (eq 1 (length (window-list)))
          (py--shell-manage-windows buffer)
        (when (get-register py--windows-config-register)
          (ignore-errors (jump-to-register (get-register py--windows-config-register))))))))

(defun py--shell-manage-windows (output-buffer &optional exception-buffer split switch)
  "Adapt or restore window configuration from OUTPUT-BUFFER.

Optional EXCEPTION-BUFFER SPLIT SWITCH
Return nil."
  (let* ((exception-buffer (or exception-buffer (other-buffer)))
         (old-window-list (window-list))
         (number-of-windows (length old-window-list))
         (split (or split py-split-window-on-execute))
         (switch
          (or py-switch-buffers-on-execute-p switch py-pdbtrack-tracked-buffer)))
    ;; (output-buffer-displayed-p)
    (cond
     (py-keep-windows-configuration
      (py-restore-window-configuration)
      (set-buffer output-buffer)
      (goto-char (point-max)))
     ((and (eq split 'always)
           switch)
      (if (member (get-buffer-window output-buffer) (window-list))
          ;; (delete-window (get-buffer-window output-buffer))
          (select-window (get-buffer-window output-buffer))
        (py--manage-windows-split exception-buffer)
        ;; otherwise new window appears above
        (save-excursion
          (other-window 1)
          (switch-to-buffer output-buffer))
        (display-buffer exception-buffer)))
     ((and
       (eq split 'always)
       (not switch))
      (if (member (get-buffer-window output-buffer) (window-list))
          (select-window (get-buffer-window output-buffer))
        (py--manage-windows-split exception-buffer)
        (display-buffer output-buffer)
        (pop-to-buffer exception-buffer)))
     ((and
       (eq split 'just-two)
       switch)
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (py--manage-windows-split exception-buffer)
      ;; otherwise new window appears above
      (other-window 1)
      (set-buffer output-buffer)
      (switch-to-buffer (current-buffer)))
     ((and
       (eq split 'just-two)
       (not switch))
      (switch-to-buffer exception-buffer)
      (delete-other-windows)
      (unless
          (member (get-buffer-window output-buffer) (window-list))
        (py--manage-windows-split exception-buffer))
      (save-excursion
        (other-window 1)
        (pop-to-buffer output-buffer)
        (goto-char (point-max))
        (other-window 1)))
     ((and
       split
       (not switch))
      ;; https://bugs.launchpad.net/python-mode/+bug/1478122
      ;; > If the shell is visible in any of the windows it should re-use that window
      ;; > I did double check and py-keep-window-configuration is nil and split is t.
      (py--split-t-not-switch-wm output-buffer number-of-windows exception-buffer))
     ((and split switch)
      (unless
          (member (get-buffer-window output-buffer) (window-list))
        (py--manage-windows-split exception-buffer))
      (set-buffer output-buffer)
      (switch-to-buffer output-buffer)
      (goto-char (point-max)))
     ((not switch)
      (let (pop-up-windows)
        (py-restore-window-configuration))))))

(defun py--execute-file-base (filename &optional proc cmd procbuf origline fast)
  "Send to Python interpreter process PROC.

In Python version 2.. \"execfile('FILENAME')\".

Takes also CMD PROCBUF ORIGLINE NO-OUTPUT.

Make that process's buffer visible and force display.  Also make
comint believe the user typed this string so that
‘kill-output-from-shell’ does The Right Thing.
Returns position where output starts."
  (let* ((filename (expand-file-name filename))
         (buffer (or procbuf (and proc (process-buffer proc)) (py-shell nil nil nil nil nil fast)))
         (proc (or proc (get-buffer-process buffer)))
         (limit (marker-position (process-mark proc)))
         (cmd (or cmd (py-execute-file-command filename)))
         erg)
    (if fast
        (process-send-string proc cmd)
      (py-execute-string cmd proc))
    (with-current-buffer buffer
      (when (or py-return-result-p py-store-result-p)
        (setq erg (py--postprocess buffer origline limit cmd filename))
        (if py-error
            (setq py-error (prin1-to-string py-error))
          erg)))))

(defun py-restore-window-configuration (&optional register)
  "Restore ‘py-restore-window-configuration’."
  (let ((val register))
    (if val
        (jump-to-register (get-register val))
      (and (setq val (get-register py--windows-config-register))
           (consp val) (window-configuration-p (car val))
           (markerp (cadr val))(marker-buffer (cadr val))
           (jump-to-register py--windows-config-register)))))

(defun py-toggle-split-window-function ()
  "If window is splitted vertically or horizontally.

When code is executed and ‘py-split-window-on-execute’ is t,
the result is displays in an output-buffer, \"\*Python\*\" by default.

Customizable variable ‘py-split-windows-on-execute-function’
tells how to split the screen."
  (interactive)
  (if (eq 'split-window-vertically py-split-windows-on-execute-function)
      (setq py-split-windows-on-execute-function'split-window-horizontally)
    (setq py-split-windows-on-execute-function 'split-window-vertically))
  (when (and py-verbose-p (called-interactively-p 'any))
    (message "py-split-windows-on-execute-function set to: %s" py-split-windows-on-execute-function)))

(defun py--manage-windows-set-and-switch (buffer)
  "Switch to output BUFFER, go to ‘point-max’.

Internal use"
  (set-buffer buffer)
  (goto-char (process-mark (get-buffer-process (current-buffer)))))

(defun py--alternative-split-windows-on-execute-function ()
  "Toggle split-window-horizontally resp. vertically."
  (if (eq py-split-windows-on-execute-function 'split-window-vertically)
      'split-window-horizontally
    'split-window-vertically))

(defun py--get-splittable-window ()
  "Search ‘window-list’ for a window suitable for splitting."
  (or (and (window-left-child)(split-window (window-left-child)))
      (and (window-top-child)(split-window (window-top-child)))
      (and (window-parent)(ignore-errors (split-window (window-parent))))
      (and (window-atom-root)(split-window (window-atom-root)))))

(defun py--manage-windows-split (buffer)
  "If one window, split BUFFER.

according to ‘py-split-windows-on-execute-function’."
  (interactive)
  (set-buffer buffer)
  (or
   ;; (split-window (selected-window) nil ’below)
   (ignore-errors (funcall py-split-windows-on-execute-function))
   ;; If call did not succeed according to settings of
   ;; ‘split-height-threshold’, ‘split-width-threshold’
   ;; resp. ‘window-min-height’, ‘window-min-width’
   ;; try alternative split
   (unless (ignore-errors (funcall (py--alternative-split-windows-on-execute-function)))
     ;; if alternative split fails, look for larger window
     (py--get-splittable-window)
     (ignore-errors (funcall (py--alternative-split-windows-on-execute-function))))))

;; (defun py--display-windows (output-buffer)
;;     "Otherwise new window appears above"
;;       (display-buffer output-buffer)
;;       (select-window py-exception-window))

(defun py--split-t-not-switch-wm (output-buffer number-of-windows exception-buffer)
  (unless (window-live-p output-buffer)
    (with-current-buffer (get-buffer exception-buffer)

      (when (< number-of-windows py-split-window-on-execute-threshold)
        (unless
            (member (get-buffer-window output-buffer) (window-list))
          (py--manage-windows-split exception-buffer)))
      (display-buffer output-buffer t)
      (switch-to-buffer exception-buffer)
      )))

(defun py-execute-file (filename &optional proc)
  "When called interactively, user is prompted for FILENAME."
  (interactive "fFilename: ")
  (let (;; postprocess-output-buffer might want origline
        (origline 1)
        (py-exception-buffer filename)
        erg)
    (if (file-readable-p filename)
        (if py-store-result-p
            (setq erg (py--execute-file-base (expand-file-name filename) nil nil nil origline))
          (py--execute-file-base (expand-file-name filename) proc))
      (message "%s not readable. %s" filename "Do you have write permissions?"))
    ;; (py--shell-manage-windows py-output-buffer py-exception-buffer nil (or (called-interactively-p 'interactive)))
    erg))

(defun py-send-string-no-output (strg &optional process buffer-name)
  "Send STRING to PROCESS and inhibit output.

Return the output."
  (let* ((proc (or process (py--get-process)))
         (buffer (or buffer-name (if proc (buffer-name (process-buffer proc)) (py-shell))))
         (comint-preoutput-filter-functions
          '(py-shell-output-filter))
         (py-shell-output-filter-in-progress t)
         (inhibit-quit t)
         (delay (py--which-delay-process-dependent buffer))
         temp-file-name)
    (or
     (with-local-quit
       (if (and (string-match ".\n+." strg) (string-match "^\*[Ii]" buffer))  ;; IPython or multiline
           (let ((file-name (or (buffer-file-name) (setq temp-file-name (py-temp-file-name strg)))))
             (py-execute-file file-name proc)
             (when temp-file-name (delete-file temp-file-name)))
         (py-shell-send-string strg proc))
       ;; (switch-to-buffer buffer)
       ;; (accept-process-output proc 9)
       (while py-shell-output-filter-in-progress
         ;; ‘py-shell-output-filter’ takes care of setting
         ;; ‘py-shell-output-filter-in-progress’ to NIL after it
         ;; detects end of output.
         (accept-process-output proc delay))
       (prog1
           py-shell-output-filter-buffer
         (setq py-shell-output-filter-buffer nil)))
     (with-current-buffer (process-buffer proc)
       (comint-interrupt-subjob)))))

(defun py--leave-backward-string-list-and-comment-maybe (pps)
  (while (or (and (nth 8 pps) (goto-char (nth 8 pps)))
             (and (nth 1 pps) (goto-char (nth 1 pps)))
             (and (nth 4 pps) (goto-char (nth 4 pps))))
    ;; (back-to-indentation)
    (when (or (looking-at comment-start)(member (char-after) (list ?\" ?')))
      (skip-chars-backward " \t\r\n\f"))
    (setq pps (parse-partial-sexp (point-min) (point)))))

(defun py-set-ipython-completion-command-string (shell)
  "Set and return ‘py-ipython-completion-command-string’ according to SHELL."
  (interactive)
  (let* ((ipython-version (py-ipython--which-version shell)))
    (if (string-match "[0-9]" ipython-version)
        (setq py-ipython-completion-command-string
              (cond ((string-match "^[^0].+" ipython-version)
                     py-ipython0.11-completion-command-string)
                    ((string-match "^0.1[1-3]" ipython-version)
                     py-ipython0.11-completion-command-string)
                    ((string= "^0.10" ipython-version)
                     py-ipython0.10-completion-command-string)))
      (error ipython-version))))

(defun py-ipython--module-completion-import (proc)
  "Import module-completion according to PROC."
  (interactive)
  (let ((ipython-version (shell-command-to-string (concat py-shell-name " -V"))))
    (when (and (string-match "^[0-9]" ipython-version)
               (string-match "^[^0].+" ipython-version))
      (process-send-string proc "from IPython.core.completerlib import module_completion"))))

(defun py--compose-buffer-name-initials (liste)
  (let (erg)
    (dolist (ele liste)
      (unless (string= "" ele)
        (setq erg (concat erg (char-to-string (aref ele 0))))))
    erg))

(defun py--remove-home-directory-from-list (liste)
  "Prepare for compose-buffer-name-initials according to LISTE."
  (let ((case-fold-search t)
        (liste liste)
        erg)
    (if (listp (setq erg (split-string (expand-file-name "~") "\/")))
        erg
      (setq erg (split-string (expand-file-name "~") "\\\\")))
     (while erg
      (when (member (car erg) liste)
        (setq liste (cdr (member (car erg) liste))))
      (setq erg (cdr erg)))
    (butlast liste)))

(defun py--prepare-shell-name (erg)
  "Provide a readable shell name by capitalizing etc."
  (cond ((string-match "^ipython" erg)
         (replace-regexp-in-string "ipython" "IPython" erg))
        ((string-match "^jython" erg)
         (replace-regexp-in-string "jython" "Jython" erg))
        ((string-match "^python" erg)
         (replace-regexp-in-string "python" "Python" erg))
        ((string-match "^python2" erg)
         (replace-regexp-in-string "python2" "Python2" erg))
        ((string-match "^python3" erg)
         (replace-regexp-in-string "python3" "Python3" erg))
        ((string-match "^pypy" erg)
         (replace-regexp-in-string "pypy" "PyPy" erg))
        (t erg)))

(defun py--choose-buffer-name (&optional name dedicated fast-process)
  "Return an appropriate NAME to display in modeline.

Optional DEDICATED FAST-PROCESS
SEPCHAR is the file-path separator of your system."
  (let* ((name-first (or name py-shell-name))
         (erg (when name-first (if (stringp name-first) name-first (prin1-to-string name-first))))
         (fast-process (or fast-process py-fast-process-p))
         prefix)
    (when (string-match "^py-" erg)
      (setq erg (nth 1 (split-string erg "-"))))
    ;; remove home-directory from prefix to display
    (unless py-modeline-acronym-display-home-p
      (save-match-data
        (let ((case-fold-search t))
          (when (string-match (concat ".*" (expand-file-name "~")) erg)
            (setq erg (replace-regexp-in-string (concat "^" (expand-file-name "~")) "" erg))))))
    (if (or (and (setq prefix (split-string erg "\\\\"))
                 (< 1 (length prefix)))
            (and (setq prefix (split-string erg "\/"))
                 (< 1 (length prefix))))
        (progn
          ;; exect something like default py-shell-name
          (setq erg (car (last prefix)))
          (unless py-modeline-acronym-display-home-p
            ;; home-directory may still inside
            (setq prefix (py--remove-home-directory-from-list prefix))
            (setq prefix (py--compose-buffer-name-initials prefix))))
      (setq erg (or erg py-shell-name))
      (setq prefix nil))
    (when fast-process (setq erg (concat erg " Fast")))
    (setq erg
          (py--prepare-shell-name erg))
    (when (or dedicated py-dedicated-process-p)
      (setq erg (make-temp-name (concat erg "-"))))
    (cond ((and prefix (string-match "^\*" erg))
           (setq erg (replace-regexp-in-string "^\*" (concat "*" prefix " ") erg)))
          (prefix
           (setq erg (concat "*" prefix " " erg "*")))
          (t (unless (string-match "^\*" erg) (setq erg (concat "*" erg "*")))))
    erg))

(defun py-shell (&optional argprompt args dedicated shell buffer fast exception-buffer split switch internal)
  "Connect process to BUFFER.

Start an interpreter according to ‘py-shell-name’ or SHELL.

Optional ARGPROMPT: with \\[universal-argument] start in a new
dedicated shell.

Optional ARGS: Specify other than default command args.

Optional DEDICATED: start in a new dedicated shell.
Optional string SHELL overrides default ‘py-shell-name’.
Optional string BUFFER allows a name, the Python process is connected to
Optional FAST: no fontification in process-buffer.
Optional EXCEPTION-BUFFER: point to error.
Optional SPLIT: see var ‘py-split-window-on-execute’
Optional SWITCH: see var ‘py-switch-buffers-on-execute-p’
Optional INTERNAL shell will be invisible for users

Reusing existing processes: For a given buffer and same values,
if a process is already running for it, it will do nothing.

Runs the hook ‘py-shell-mode-hook’ after
‘comint-mode-hook’ is run.  (Type \\[describe-mode] in the
process buffer for a list of commands.)"
  (interactive "p")
  ;; Let's use python.el's ‘python-shell-with-environment’
  (require 'python)
  (let* ((interactivep (and argprompt (eq 1 (prefix-numeric-value argprompt))))
         (fast (unless (eq major-mode 'org-mode)
                 (or fast py-fast-process-p)))
         (dedicated (or (eq 4 (prefix-numeric-value argprompt)) dedicated py-dedicated-process-p))
         (shell (if shell
                    (pcase shell
                      ("python"
                       (or (and (executable-find shell) shell)
                           (and (executable-find "python3") "python3")))
                      (_ (if (executable-find shell)
                             shell
                           (error (concat "py-shell: Can not see an executable for `"shell "' on your system. Maybe needs a link?")))))
                  (py-choose-shell)))
         (args (or args (car (py--provide-command-args shell fast))))
         ;; Make sure a new one is created if required
         (this-buffer
          (or (and buffer (stringp buffer) buffer)
              (and buffer (buffer-name buffer))
              (and python-mode-v5-behavior-p (get-buffer-create "*Python Output*"))
              (py--choose-buffer-name shell dedicated fast)))
         (proc (get-buffer-process this-buffer))
         (buffer (or (ignore-errors (process-buffer proc))
                     ;; Use python.el's provision here
                     (python-shell-with-environment
                       (apply #'make-comint-in-buffer shell
                              (set-buffer
                               (get-buffer-create this-buffer))
                              (list shell nil args)))))
         (this-buffer-name (buffer-name buffer))
         delay)
    (setq py-output-buffer (buffer-name (if python-mode-v5-behavior-p (get-buffer "*Python Output*") buffer)))
    (with-current-buffer buffer
      (setq delay (py--which-delay-process-dependent this-buffer-name))
      (unless fast
        (setq py-shell-mode-syntax-table python-mode-syntax-table)
        (when interactivep
          (cond ((string-match "^.I" this-buffer-name)
                 (message "Waiting according to ‘py-ipython-send-delay:’ %s" delay))
                ((string-match "^.+3" this-buffer-name)
                 (message "Waiting according to ‘py-python3-send-delay:’ %s" delay))))
        (setq py-modeline-display (py--update-lighter this-buffer-name))))
    (if (setq proc (get-buffer-process buffer))
        (progn
          (when py-register-shell-buffer-p
            (save-excursion
              (save-restriction
                (with-current-buffer buffer
                  (switch-to-buffer (current-buffer)) 
                  (goto-char (point-max))
                  (sit-for 0.1) 
                  (funcall 'window-configuration-to-register py-register-char)
                  ))))
          (unless fast (py-shell-mode))
          (and internal (set-process-query-on-exit-flag proc nil))
          (when (or interactivep
                    (or switch py-switch-buffers-on-execute-p py-split-window-on-execute))
            (py--shell-manage-windows buffer exception-buffer split (or interactivep switch)))
          buffer)
      (error (concat "py-shell:" (py--fetch-error py-output-buffer))))))

(defun py-kill-buffer-unconditional (&optional buffer)
  "Kill buffer unconditional, kill buffer-process if existing."
  (interactive
   (list (current-buffer)))
  (let ((buffer (or (and (bufferp buffer) buffer)
                    (get-buffer (current-buffer))))
        proc kill-buffer-query-functions)
    (if (buffer-live-p buffer)
        (progn
          (setq proc (get-buffer-process buffer))
          (and proc (kill-process proc))
          (set-buffer buffer)
          (set-buffer-modified-p 'nil)
          (kill-buffer (current-buffer)))
      (message "Can not see a buffer %s" buffer))))

(provide 'python-components-start1)
;;; python-components-start1.el ends here
