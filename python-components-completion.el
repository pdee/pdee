;;; python-components-completion.el --- shipped emacs23 completion

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008  Free Software Foundation, Inc.
;; Copyright (C) 2009, 2010  David Love

;; Original Author: Dave Love <fx@gnu.org>
;; Created: Nov 2003
;; Keywords: languages
;; URL: http://www.loveshack.ukfsn.org/emacs/

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

;;; Commentary: extractions from python.el, see above


;;; Code
(require 'comint)
(require 'python-components-macros)

(defcustom python-remove-cwd-from-path t
  "Whether to allow loading of Python modules from the current directory.
If this is non-nil, Emacs removes '' from sys.path when starting
an inferior Python process.  This is the default, for security
reasons, as it is easy for the Python process to be started
without the user's realization (e.g. to perform completion)."
  :type 'boolean
  :group 'python
  :version "23.3")

(defvar python-pdbtrack-is-tracking-p nil)

(defconst python-pdbtrack-track-range 10000
  "Max number of characters from end of buffer to search for stack entry.")

(defvar python-preoutput-skip-next-prompt nil)

(defvar python-preoutput-result nil
  "Data from last `_emacs_out' line seen by the preoutput filter.")

(defvar python-preoutput-continuation nil
  "If non-nil, funcall this when `python-preoutput-filter' sees `_emacs_ok'.")

(defvar python-preoutput-leftover nil)

(defvar python-version-checked nil)

(defconst python-compilation-regexp-alist
  ;; FIXME: maybe these should move to compilation-error-regexp-alist-alist.
  ;;   The first already is (for CAML), but the second isn't.  Anyhow,
  ;;   these are specific to the inferior buffer.  -- fx
  `((,(rx line-start (1+ (any " \t")) "File \""
	  (group (1+ (not (any "\"<")))) ; avoid `<stdin>' &c
	  "\", line " (group (1+ digit)))
     1 2)
    (,(rx " in file " (group (1+ not-newline)) " on line "
	  (group (1+ digit)))
     1 2)
    ;; pdb stack trace
    (,(rx line-start "> " (group (1+ (not (any "(\"<"))))
	  "(" (group (1+ digit)) ")" (1+ (not (any "("))) "()")
     1 2))
  "`compilation-error-regexp-alist' for inferior Python.")

;;;

(defun python-guess-indent ()
  "Guess step for indentation of current buffer.
Set `python-indent' locally to the value guessed."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (done indent)
	(while (and (not done) (not (eobp)))
	  (when (and (re-search-forward (rx ?: (0+ space)
					    (or (syntax comment-start)
						line-end))
					nil 'move)
		     (python-open-block-statement-p))
	    (save-excursion
	      (python-beginning-of-statement)
	      (let ((initial (current-indentation)))
		(if (zerop (python-next-statement))
		    (setq indent (- (current-indentation) initial)))
		(if (and indent (>= indent 2) (<= indent 8)) ; sanity check
		    (setq done t))))))
	(when done
	  (when (/= indent (default-value 'python-indent))
	    (set (make-local-variable 'python-indent) indent)
	    (unless (= tab-width python-indent)
	      (setq indent-tabs-mode nil)))
	  indent)))))

(defun python-pdbtrack-get-source-buffer (block)
  "Return line number and buffer of code indicated by block's traceback text.

We look first to visit the file indicated in the trace.

Failing that, we look for the most recently visited python-mode buffer
with the same name or having the named function.

If we're unable find the source code we return a string describing the
problem."

  (if (not (string-match python-pdbtrack-stack-entry-regexp block))

      "Traceback cue not found"

    (let* ((filename (match-string 1 block))
           (lineno (string-to-number (match-string 2 block)))
           (funcname (match-string 3 block))
           funcbuffer)

      (cond ((file-exists-p filename)
             (list lineno (find-file-noselect filename)))

            ((setq funcbuffer (python-pdbtrack-grub-for-buffer funcname lineno))
             (if (string-match "/Script (Python)$" filename)
                 ;; Add in number of lines for leading '##' comments:
                 (setq lineno
                       (+ lineno
                          (with-current-buffer funcbuffer
                            (if (equal (point-min)(point-max))
                                0
                              (count-lines
                               (point-min)
                               (max (point-min)
                                    (string-match "^\\([^#]\\|#[^#]\\|#$\\)"
                                                  (buffer-substring
                                                   (point-min) (point-max)))
                                    )))))))
               (list lineno funcbuffer))

            ((= (elt filename 0) ?\<)
             (format "(Non-file source: '%s')" filename))

            (t (format "Not found: %s(), %s" funcname filename)))
      )
    )
  )

(defun python-pdbtrack-overlay-arrow (activation)
  "Activate or deactivate arrow at beginning-of-line in current buffer."
  (if activation
      (progn
        (setq overlay-arrow-position (make-marker)
              overlay-arrow-string "=>"
              python-pdbtrack-is-tracking-p t)
        (set-marker overlay-arrow-position
                    (save-excursion (beginning-of-line) (point))
                    (current-buffer)))
    (setq overlay-arrow-position nil
          python-pdbtrack-is-tracking-p nil)))

(defun python-pdbtrack-track-stack-file (text)
  "Show the file indicated by the pdb stack entry line, in a separate window.

Activity is disabled if the buffer-local variable
`python-pdbtrack-do-tracking-p' is nil.

We depend on the pdb input prompt being a match for
`python-pdbtrack-input-prompt'.

If the traceback target file path is invalid, we look for the
most recently visited python-mode buffer which either has the
name of the current function or class, or which defines the
function or class.  This is to provide for scripts not in the
local filesytem (e.g., Zope's 'Script \(Python)', but it's not
Zope specific).  If you put a copy of the script in a buffer
named for the script and activate python-mode, then pdbtrack will
find it."
  ;; Instead of trying to piece things together from partial text
  ;; (which can be almost useless depending on Emacs version), we
  ;; monitor to the point where we have the next pdb prompt, and then
  ;; check all text from comint-last-input-end to process-mark.
  ;;
  ;; Also, we're very conservative about clearing the overlay arrow,
  ;; to minimize residue.  This means, for instance, that executing
  ;; other pdb commands wipe out the highlight.  You can always do a
  ;; 'where' (aka 'w') PDB command to reveal the overlay arrow.

  (let* ((origbuf (current-buffer))
	 (currproc (get-buffer-process origbuf)))

    (if (not (and currproc python-pdbtrack-do-tracking-p))
        (python-pdbtrack-overlay-arrow nil)

      (let* ((procmark (process-mark currproc))
             (block (buffer-substring (max comint-last-input-end
                                           (- procmark
                                              python-pdbtrack-track-range))
                                      procmark))
             target target_fname target_lineno target_buffer)

        (if (not (string-match (concat python-pdbtrack-input-prompt "$") block))
            (python-pdbtrack-overlay-arrow nil)

          (setq target (python-pdbtrack-get-source-buffer block))

          (if (stringp target)
              (progn
                (python-pdbtrack-overlay-arrow nil)
                (message "pdbtrack: %s" target))

            (setq target_lineno (car target)
                  target_buffer (cadr target)
                  target_fname (buffer-file-name target_buffer))
            (switch-to-buffer-other-window target_buffer)
            (goto-char (point-min))
            (forward-line (1- target_lineno))
            (message "pdbtrack: line %s, file %s" target_lineno target_fname)
            (python-pdbtrack-overlay-arrow t)
            (pop-to-buffer origbuf t)
            ;; in large shell buffers, above stuff may cause point to lag output
            (goto-char procmark)))))))

(defun python-preoutput-filter (s)
  "`comint-preoutput-filter-functions' function: ignore prompts not at bol."
  (when python-preoutput-leftover
    (setq s (concat python-preoutput-leftover s))
    (setq python-preoutput-leftover nil))
  (let ((start 0)
        (res ""))
    ;; First process whole lines.
    (while (string-match "\n" s start)
      (let ((line (substring s start (setq start (match-end 0)))))
        ;; Skip prompt if needed.
        (when (and python-preoutput-skip-next-prompt
                   (string-match comint-prompt-regexp line))
          (setq python-preoutput-skip-next-prompt nil)
          (setq line (substring line (match-end 0))))
        ;; Recognize special _emacs_out lines.
        (if (and (string-match "\\`_emacs_out \\(.*\\)\n\\'" line)
                 (local-variable-p 'python-preoutput-result))
            (progn
              (setq python-preoutput-result (match-string 1 line))
              (set (make-local-variable 'python-preoutput-skip-next-prompt) t))
          (setq res (concat res line)))))
    ;; Then process the remaining partial line.
    (unless (zerop start) (setq s (substring s start)))
    (cond ((and (string-match comint-prompt-regexp s)
                ;; Drop this prompt if it follows an _emacs_out...
                (or python-preoutput-skip-next-prompt
                    ;; ... or if it's not gonna be inserted at BOL.
                    ;; Maybe we could be more selective here.
                    (if (zerop (length res))
                        (not (bolp))
                      (string-match ".\\'" res))))
           ;; The need for this seems to be system-dependent:
           ;; What is this all about, exactly?  --Stef
           ;; (if (and (eq ?. (aref s 0)))
           ;;     (accept-process-output (get-buffer-process (current-buffer)) 1))
           (setq python-preoutput-skip-next-prompt nil)
           res)
          ((let ((end (min (length "_emacs_out ") (length s))))
             (eq t (compare-strings s nil end "_emacs_out " nil end)))
           ;; The leftover string is a prefix of _emacs_out so we don't know
           ;; yet whether it's an _emacs_out or something else: wait until we
           ;; get more output so we can resolve this ambiguity.
           (set (make-local-variable 'python-preoutput-leftover) s)
           res)
          (t (concat res s)))))

(defun python-check-comint-prompt (&optional proc)
  "Return non-nil if and only if there's a normal prompt in the inferior buffer.
If there isn't, it's probably not appropriate to send input to return Eldoc
information etc.  If PROC is non-nil, check the buffer for that process."
  (with-current-buffer (process-buffer (or proc (python-proc)))
    (save-excursion
      (save-match-data
	(re-search-backward (concat python--prompt-regexp " *\\=")
			    nil t)))))


(autoload 'comint-check-proc "comint")


(defun python-check-version (cmd)
  "Check that CMD runs a suitable version of Python."
  ;; Fixme:  Check on Jython.
  (unless (or python-version-checked
	      (equal 0 (string-match (regexp-quote python-python-command)
				     cmd)))
    (unless (shell-command-to-string cmd)
      (error "Can't run Python command `%s'" cmd))
    (let* ((res (shell-command-to-string
                 (concat cmd
                         " -c \"from sys import version_info;\
print version_info >= (2, 2) and version_info < (3, 0)\""))))
      (unless (string-match "True" res)
	(error "Only Python versions >= 2.2 and < 3.0 are supported")))
    (setq python-version-checked t)))

;; Fixme: Loses with quoted whitespace.
(defun python-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (python-args-to-list (substring string (+ 1 where)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if pos (python-args-to-list (substring string pos))))))))

;;;###autoload
(defun run-python (&optional cmd noshow new)
  "Run an inferior Python process, input and output via buffer *Python*.
CMD is the Python command to run.  NOSHOW non-nil means don't
show the buffer automatically.

Interactively, a prefix arg means to prompt for the initial
Python command line (default is `python-command').

A new process is started if one isn't running attached to
`python-buffer', or if called from Lisp with non-nil arg NEW.
Otherwise, if a process is already running in `python-buffer',
switch to that buffer.

This command runs the hook `inferior-python-mode-hook' after
running `comint-mode-hook'.  Type \\[describe-mode] in the
process buffer for a list of commands.

By default, Emacs inhibits the loading of Python modules from the
current working directory, for security reasons.  To disable this
behavior, change `python-remove-cwd-from-path' to nil."
  (interactive (if current-prefix-arg
		   (list (read-string "Run Python: " python-command) nil t)
		 (list python-command)))
  (require 'ansi-color) ; for ipython
  (unless cmd (setq cmd python-command))
  (python-check-version cmd)
  (setq python-command cmd)
  ;; Fixme: Consider making `python-buffer' buffer-local as a buffer
  ;; (not a name) in Python buffers from which `run-python' &c is
  ;; invoked.  Would support multiple processes better.
  (when (or new (not (comint-check-proc python-buffer)))
    (with-current-buffer
	(let* ((cmdlist
		(append (python-args-to-list cmd) '("-i")
			(if python-remove-cwd-from-path
			    '("-c" "import sys; sys.path.remove('')"))))
	       (path (getenv "PYTHONPATH"))
	       (process-environment	; to import emacs.py
		(cons (concat "PYTHONPATH="
			      (if path (concat path path-separator))
			      data-directory)
		      process-environment))
               ;; If we use a pipe, unicode characters are not printed
               ;; correctly (Bug#5794) and IPython does not work at
               ;; all (Bug#5390).
	       (process-connection-type t))
	  (apply 'make-comint-in-buffer "Python"
		 (generate-new-buffer "*Python*")
		 (car cmdlist) nil (cdr cmdlist)))
      (setq-default python-buffer (current-buffer))
      (setq python-buffer (current-buffer))
      (accept-process-output (get-buffer-process python-buffer) 5)
      (inferior-python-mode)
      ;; Load function definitions we need.
      ;; Before the preoutput function was used, this was done via -c in
      ;; cmdlist, but that loses the banner and doesn't run the startup
      ;; file.  The code might be inline here, but there's enough that it
      ;; seems worth putting in a separate file, and it's probably cleaner
      ;; to put it in a module.
      ;; Ensure we're at a prompt before doing anything else.
      (python-send-string "import emacs")
      ;; The following line was meant to ensure that we're at a prompt
      ;; before doing anything else.  However, this can cause Emacs to
      ;; hang waiting for a response, if that Python function fails
      ;; (i.e. raises an exception).
      ;; (python-send-receive "print '_emacs_out ()'")
      ))
  (if (derived-mode-p 'python-mode)
      (setq python-buffer (default-value 'python-buffer))) ; buffer-local
  ;; Without this, help output goes into the inferior python buffer if
  ;; the process isn't already running.
  (sit-for 1 t)        ;Should we use accept-process-output instead?  --Stef
  (unless noshow (pop-to-buffer python-buffer t)))


(defun python-proc ()
  "Return the current Python process.
See variable `python-buffer'.  Starts a new process if necessary."
  ;; Fixme: Maybe should look for another active process if there
  ;; isn't one for `python-buffer'.
  (unless (comint-check-proc python-buffer)
    (run-python nil t))
  (get-buffer-process (if (derived-mode-p 'inferior-python-mode)
			  (current-buffer)
			python-buffer)))

(defun python-send-string (string)
  "Evaluate STRING in inferior Python process."
  (interactive "sPython command: ")
  (comint-send-string (python-proc) string)
  (unless (string-match "\n\\'" string)
    ;; Make sure the text is properly LF-terminated.
    (comint-send-string (python-proc) "\n"))
  (when (string-match "\n[ \t].*\n?\\'" string)
    ;; If the string contains a final indented line, add a second newline so
    ;; as to make sure we terminate the multiline instruction.
    (comint-send-string (python-proc) "\n")))

(defun python-send-receive (string)
  "Send STRING to inferior Python (if any) and return result.
The result is what follows `_emacs_out' in the output.
This is a no-op if `python-check-comint-prompt' returns nil."
  (python-send-string string)
  (let ((proc (python-proc)))
    (with-current-buffer (process-buffer proc)
      (when (python-check-comint-prompt proc)
	(set (make-local-variable 'python-preoutput-result) nil)
	(while (progn
		 (accept-process-output proc 5)
		 (null python-preoutput-result)))
	(prog1 python-preoutput-result
	  (kill-local-variable 'python-preoutput-result))))))

(defun python-symbol-completions (symbol)
  "Return a list of completions of the string SYMBOL from Python process.
The list is sorted.
Uses `python-imports' to load modules against which to complete."
  (when (stringp symbol)
    (let ((completions
	   (condition-case ()
	       (car (read-from-string
		     (python-send-receive
		      (format "emacs.complete(%S,%s)"
			      (substring-no-properties symbol)
			      python-imports))))
	     (error nil))))
      (sort
       ;; We can get duplicates from the above -- don't know why.
       (delete-dups completions)
       #'string<))))

(defun python-completion-at-point ()
  (interactive)
  (let ((end (point))
	(start (save-excursion
		 (and (re-search-backward
		       (rx (or buffer-start (regexp "[^[:alnum:]._]"))
			   (group (1+ (regexp "[[:alnum:]._]"))) point)
		       nil t)
		      (match-beginning 1)))))
    (when start
      (list start end
            (completion-table-dynamic 'python-symbol-completions)))))

(define-derived-mode inferior-python-mode comint-mode "Inferior Python"
  "Major mode for interacting with an inferior Python process.
A Python process can be started with \\[run-python].

Hooks `comint-mode-hook' and `inferior-python-mode-hook' are run in
that order.

You can send text to the inferior Python process from other buffers
containing Python source.
 * \\[python-switch-to-python] switches the current buffer to the Python
    process buffer.
 * \\[python-send-region] sends the current region to the Python process.
 * \\[python-send-region-and-go] switches to the Python process buffer
    after sending the text.
For running multiple processes in multiple buffers, see `run-python' and
`python-buffer'.

\\{inferior-python-mode-map}"
  :group 'python
  (require 'ansi-color) ; for ipython
  (setq mode-line-process '(":%s"))
  (set (make-local-variable 'comint-input-filter) 'python-input-filter)
  (add-hook 'comint-preoutput-filter-functions #'python-preoutput-filter
	    nil t)
  (python--set-prompt-regexp)
  (set (make-local-variable 'compilation-error-regexp-alist)
       python-compilation-regexp-alist)
  (compilation-shell-minor-mode 1))

;;;###autoload
(define-derived-mode python-mode fundamental-mode "Python"
  "Major mode for editing Python files.
Turns on Font Lock mode unconditionally since it is currently required
for correct parsing of the source.
See also `jython-mode', which is actually invoked if the buffer appears to
contain Jython code.  See also `run-python' and associated Python mode
commands for running Python under Emacs.

The Emacs commands which work with `defun's, e.g. \\[beginning-of-defun], deal
with nested `def' and `class' blocks.  They take the innermost one as
current without distinguishing method and class definitions.  Used multiple
times, they move over others at the same indentation level until they reach
the end of definitions at that level, when they move up a level.
\\<python-mode-map>
Colon is electric: it outdents the line if appropriate, e.g. for
an else statement.  \\[python-backspace] at the beginning of an indented statement
deletes a level of indentation to close the current block; otherwise it
deletes a character backward.  TAB indents the current line relative to
the preceding code.  Successive TABs, with no intervening command, cycle
through the possibilities for indentation on the basis of enclosing blocks.

\\[fill-paragraph] fills comments and multi-line strings appropriately, but has no
effect outside them.

Supports Eldoc mode (only for functions, using a Python process),
Info-Look and Imenu.  In Outline minor mode, `class' and `def'
lines count as headers.  Symbol completion is available in the
same way as in the Python shell using the `rlcompleter' module
and this is added to the Hippie Expand functions locally if
Hippie Expand mode is turned on.  Completion of symbols of the
form x.y only works if the components are literal
module/attribute names, not variables.  An abbrev table is set up
with skeleton expansions for compound statement templates.

\\{python-mode-map}"
  :group 'python
  (set (make-local-variable 'font-lock-defaults)
       '(python-font-lock-keywords nil nil nil nil
				   (font-lock-syntactic-keywords
				    . python-font-lock-syntactic-keywords)
				   ;; This probably isn't worth it.
				   ;; (font-lock-syntactic-face-function
				   ;;  . python-font-lock-syntactic-face-function)
				   ))
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'indent-line-function) #'python-indent-line)
  (set (make-local-variable 'indent-region-function) #'python-indent-region)
  (set (make-local-variable 'paragraph-start) "\\s-*$")
  (set (make-local-variable 'fill-paragraph-function) 'python-fill-paragraph)
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  (set (make-local-variable 'add-log-current-defun-function)
       #'python-current-defun)
  (set (make-local-variable 'outline-regexp)
       (rx (* space) (or "class" "def" "elif" "else" "except" "finally"
			 "for" "if" "try" "while" "with")
	   symbol-end))
  (set (make-local-variable 'outline-heading-end-regexp) ":\\s-*\n")
  (set (make-local-variable 'outline-level) #'python-outline-level)
  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)
  (make-local-variable 'python-saved-check-command)
  (set (make-local-variable 'beginning-of-defun-function)
       'python-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'python-end-of-defun)
  (add-hook 'which-func-functions 'python-which-func nil t)
  (setq imenu-create-index-function #'python-imenu-create-index)
  (set (make-local-variable 'eldoc-documentation-function)
       #'python-eldoc-function)
  (add-hook 'eldoc-mode-hook
	    (lambda () (run-python nil t)) ; need it running
	    nil t)
  (add-hook 'completion-at-point-functions
            'python-completion-at-point nil 'local)
  ;; Fixme: should be in hideshow.  This seems to be of limited use
  ;; since it isn't (can't be) indentation-based.  Also hide-level
  ;; doesn't seem to work properly.
  (add-to-list 'hs-special-modes-alist
	       `(python-mode "^\\s-*\\(?:def\\|class\\)\\>" nil "#"
		 ,(lambda (arg)
		    (python-end-of-defun)
		    (skip-chars-backward " \t\n"))
		 nil))
  (set (make-local-variable 'skeleton-further-elements)
       '((< '(backward-delete-char-untabify (min python-indent
						 (current-column))))
	 (^ '(- (1+ (current-indentation))))))
  ;; Python defines TABs as being 8-char wide.
  (set (make-local-variable 'tab-width) 8)
  (when python-guess-indent (python-guess-indent))
  ;; Let's make it harder for the user to shoot himself in the foot.
  (unless (= tab-width python-indent)
    (setq indent-tabs-mode nil))
  (set (make-local-variable 'python-command) python-python-command)
  (python-find-imports)
  (unless (boundp 'python-mode-running)	; kill the recursion from jython-mode
    (let ((python-mode-running t))
      (python-maybe-jython))))

(provide 'python-components-completion)
;;; python-components-completion.el ends here

