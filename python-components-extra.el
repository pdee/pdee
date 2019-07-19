;;; python-components-extra.el --- completion and path update  -*- lexical-binding: t; -*-

;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary: Stuff merged/adapted from python.el

;;

;;; Code:

(defun py-info-encoding ()
  "Return encoding for file.
Try `py-info-encoding-from-cookie', if none is found then
default to utf-8."
  (or (py-info-encoding-from-cookie)
      'utf-8))

(defun py-util-comint-last-prompt ()
  "Return comint last prompt overlay start and end.
This is for compatibility with Emacs < 24.4."
  (cond ((bound-and-true-p comint-last-prompt-overlay)
         (cons (overlay-start comint-last-prompt-overlay)
               (overlay-end comint-last-prompt-overlay)))
        ((bound-and-true-p comint-last-prompt)
         comint-last-prompt)
        (t nil)))

(defun py-shell-accept-process-output (process &optional timeout regexp)
  "Accept PROCESS output with TIMEOUT until REGEXP is found.
Optional argument TIMEOUT is the timeout argument to
`accept-process-output' calls.  Optional argument REGEXP
overrides the regexp to match the end of output, defaults to
`comint-prompt-regexp'.  Returns non-nil when output was
properly captured.

This utility is useful in situations where the output may be
received in chunks, since `accept-process-output' gives no
guarantees they will be grabbed in a single call.  An example use
case for this would be the CPython shell start-up, where the
banner and the initial prompt are received separately."
  (let ((regexp (or regexp comint-prompt-regexp)))
    (catch 'found
      (while t
        (when (not (accept-process-output process timeout))
          (throw 'found nil))
        (when (looking-back
               regexp (car (py-util-comint-last-prompt)))
          (throw 'found t))))))

(defun py-shell-completion-native-get-completions (process import input)
  "Get completions using native readline for PROCESS.
When IMPORT is non-nil takes precedence over INPUT for
completion."
  (with-current-buffer (process-buffer process)
    (let* ((input (or import input))
           (original-filter-fn (process-filter process))
           (redirect-buffer (get-buffer-create
                             py-shell-completion-native-redirect-buffer))
           (trigger "\t")
           (new-input (concat input trigger))
           (input-length
            (save-excursion
              (+ (- (point-max) (comint-bol)) (length new-input))))
           (delete-line-command (make-string input-length ?\b))
           (input-to-send (concat new-input delete-line-command)))
      ;; Ensure restoring the process filter, even if the user quits
      ;; or there's some other error.
      (unwind-protect
          (with-current-buffer redirect-buffer
            ;; Cleanup the redirect buffer
            (erase-buffer)
            ;; Mimic `comint-redirect-send-command', unfortunately it
            ;; can't be used here because it expects a newline in the
            ;; command and that's exactly what we are trying to avoid.
            (let ((comint-redirect-echo-input nil)
                  (comint-redirect-completed nil)
                  (comint-redirect-perform-sanity-check nil)
                  (comint-redirect-insert-matching-regexp t)
                  (comint-redirect-finished-regexp
                   "1__dummy_completion__[[:space:]]*\n")
                  (comint-redirect-output-buffer redirect-buffer))
              ;; Compatibility with Emacs 24.x.  Comint changed and
              ;; now `comint-redirect-filter' gets 3 args.  This
              ;; checks which version of `comint-redirect-filter' is
              ;; in use based on its args and uses `apply-partially'
              ;; to make it up for the 3 args case.
              (if (= (length
                      (help-function-arglist 'comint-redirect-filter)) 3)
                  (set-process-filter
                   process (apply-partially
                            #'comint-redirect-filter original-filter-fn))
                (set-process-filter process #'comint-redirect-filter))
              (process-send-string process input-to-send)
              ;; Grab output until our dummy completion used as
              ;; output end marker is found.
              (when (py-shell-accept-process-output
                     process py-shell-completion-native-output-timeout
                     comint-redirect-finished-regexp)
                (re-search-backward "0__dummy_completion__" nil t)
                (cl-remove-duplicates
                 (split-string
                  (buffer-substring-no-properties
                   (line-beginning-position) (point-min))
                  "[ \f\t\n\r\v()]+" t)
                 :test #'string=))))
        (set-process-filter process original-filter-fn)))))

(defmacro py-shell--add-to-path-with-priority (pathvar paths)
  "Modify PATHVAR and ensure PATHS are added only once at beginning."
  `(dolist (path (reverse ,paths))
     (cl-delete path ,pathvar :test #'string=)
     (cl-pushnew path ,pathvar :test #'string=)))

(defun py-shell-calculate-pythonpath ()
  "Calculate the PYTHONPATH using `py-shell-extra-pythonpaths'."
  (let ((pythonpath
         (split-string
          (or (getenv "PYTHONPATH") "") path-separator 'omit)))
    (py-shell--add-to-path-with-priority
     pythonpath py-shell-extra-pythonpaths)
    (mapconcat 'identity pythonpath path-separator)))

(defun py-shell-calculate-process-environment ()
  "Calculate `process-environment' or `tramp-remote-process-environment'.
Prepends `py-shell-process-environment', sets extra
pythonpaths from `py-shell-extra-pythonpaths' and sets a few
virtualenv related vars.  If `default-directory' points to a
remote host, the returned value is intended for
`tramp-remote-process-environment'."
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

(defun py-shell-calculate-exec-path ()
  "Calculate `exec-path'.
Prepends `py-shell-exec-path' and adds the binary directory
for virtualenv if `py-shell-virtualenv-root' is set - this
will use the python interpreter from inside the virtualenv when
starting the shell.  If `default-directory' points to a remote host,
the returned value appends `py-shell-remote-exec-path' instead
of `exec-path'."
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

(defun py-shell-tramp-refresh-remote-path (vec paths)
  "Update VEC's remote-path giving PATHS priority."
  (let ((remote-path (tramp-get-connection-property vec "remote-path" nil)))
    (when remote-path
      (py-shell--add-to-path-with-priority remote-path paths)
      (tramp-set-connection-property vec "remote-path" remote-path)
      (tramp-set-remote-path vec))))

(defun py-shell-tramp-refresh-process-environment (vec env)
  "Update VEC's process environment with ENV."
  ;; Stolen from `tramp-open-connection-setup-interactive-shell'.
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

(defmacro py-shell-with-environment (&rest body)
  "Modify shell environment during execution of BODY.
Temporarily sets `process-environment' and `exec-path' during
execution of body.  If `default-directory' points to a remote
machine then modifies `tramp-remote-process-environment' and
`py-shell-remote-exec-path' instead."
  (declare (indent 0) (debug (body)))
  (let ((vec (make-symbol "vec")))
    `(progn
       (let* ((,vec
               (when (file-remote-p default-directory)
                 (ignore-errors
                   (tramp-dissect-file-name default-directory 'noexpand))))
              (process-environment
               (if ,vec
                   process-environment
                 (py-shell-calculate-process-environment)))
              (exec-path
               (if ,vec
                   exec-path
                 (py-shell-calculate-exec-path)))
              (tramp-remote-process-environment
               (if ,vec
                   (py-shell-calculate-process-environment)
                 tramp-remote-process-environment)))
         (when (tramp-get-connection-process ,vec)
           ;; For already existing connections, the new exec path must
           ;; be re-set, otherwise it won't take effect.  One example
           ;; of such case is when remote dir-locals are read and
           ;; *then* subprocesses are triggered within the same
           ;; connection.
           (py-shell-tramp-refresh-remote-path
            ,vec (py-shell-calculate-exec-path))
           ;; The `tramp-remote-process-environment' variable is only
           ;; effective when the started process is an interactive
           ;; shell, otherwise (like in the case of processes started
           ;; with `process-file') the environment is not changed.
           ;; This makes environment modifications effective
           ;; unconditionally.
           (py-shell-tramp-refresh-process-environment
            ,vec tramp-remote-process-environment))
         ,(macroexp-progn body)))))

(defun py-shell-prompt-detect ()
  "Detect prompts for the current interpreter.
When prompts can be retrieved successfully from the
interpreter run with
`py-python-command-args', returns a list of
three elements, where the first two are input prompts and the
last one is an output prompt.  When no prompts can be detected
shows a warning with instructions to avoid hangs and returns nil.
When `py-shell-prompt-detect-p' is nil avoids any
detection and just returns nil."
  (when py-shell-prompt-detect-p
    (py-shell-with-environment
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
                ;; `condition-case' and displaying the error message to
                ;; the user in the no-prompts warning.
                (ignore-errors
                  (let ((code-file
                         ;; Python 2.x on Windows does not handle
                         ;; carriage returns in unbuffered mode.
                         (let ((inhibit-eol-conversion (getenv "PYTHONUNBUFFERED")))
                           (py-shell--save-temp-file code))))
                    (unwind-protect
                        ;; Use `process-file' as it is remote-host friendly.
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
                    ;; input prompt is the only one that can't be empty.
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
              "recover with `keyboard-quit' and then try fixing the\n"
              "interactive flag for your interpreter by adjusting the\n"
              "`py-python-command-args' or add regexps\n"
              "matching shell prompts in the directory-local friendly vars:\n"
              "  + `py-shell-prompt-regexp'\n"
              "  + `py-shell-input-prompt-2-regexp'\n"
              "  + `py-shell-prompt-output-regexp'\n"
              "Or alternatively in:\n"
              "  + `py-shell-prompt-input-regexp'\n"
              "  + `py-shell-prompt-output-regexps'"))
          prompts)))))

(defun python-util-valid-regexp-p (regexp)
  "Return non-nil if REGEXP is valid."
  (ignore-errors (string-match regexp "") t))

(defun py-shell-prompt-validate-regexps ()
  "Validate all user provided regexps for prompts.
Signals `user-error' if any of these vars contain invalid
regexps: `py-shell-prompt-regexp',
`py-shell-input-prompt-2-regexp',
`py-shell-prompt-pdb-regexp',
`py-shell-prompt-output-regexp',
`py-shell-prompt-input-regexp',
`py-shell-prompt-output-regexps'."
  (dolist (symbol (list 'py-shell-prompt-input-regexp
                        'py-shell-prompt-output-regexps
                        'py-shell-prompt-regexp
                        'py-shell-input-prompt-2-regexp
                        'py-shell-prompt-pdb-regexp
                        'py-shell-prompt-output-regexp))
    (dolist (regexp (let ((regexps (symbol-value symbol)))
                      (if (listp regexps)
                          regexps
                        (list regexps))))
      (when (not (python-util-valid-regexp-p regexp))
        (user-error "Invalid regexp %s in `%s'"
                    regexp symbol)))))

(defun py-shell-prompt-set-calculated-regexps ()
  "Detect and set input and output prompt regexps.

Build and set the values for input- and output-prompt regexp
using the values from `py-shell-prompt-regexp',
`py-shell-input-prompt-2-regexp', `py-shell-prompt-pdb-regexp',
`py-shell-prompt-output-regexp', `py-shell-prompt-input-regexp',
`py-shell-prompt-output-regexps' and detected prompts from
`py-shell-prompt-detect'."
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
      (dolist (prompt (append py-shell-prompt-input-regexp
                              (list py-shell-prompt-regexp
                                    py-shell-input-prompt-2-regexp
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

(defun py-shell-completion-get-completions (process import input)
  "Do completion at point using PROCESS for IMPORT or INPUT.
When IMPORT is non-nil takes precedence over INPUT for
completion."
  (setq input (or import input))
  (with-current-buffer (process-buffer process)
    (let ((completions
           (ignore-errors
	     (string-trim
	      (py-send-string-no-output
	       (format
		(concat py-completion-setup-code
			"\nprint (" py-shell-completion-string-code ")")
		input) process (buffer-name (current-buffer)))))))
      (when (> (length completions) 2)
        (split-string completions
                      "^'\\|^\"\\|;\\|'$\\|\"$" t)))))

(defun py-shell-completion-at-point (&optional process)
  "Function for `completion-at-point-functions' in `py-shell-mode'.
Optional argument PROCESS forces completions to be retrieved
using that one instead of current buffer's process."
  ;; (setq process (or process (get-buffer-process (current-buffer))))
  (let*
      ((process (or process (get-buffer-process (current-buffer))))
       (line-start (if (derived-mode-p 'py-shell-mode)
		       ;; Working on a shell buffer: use prompt end.
		       (or (cdr (py-util-comint-last-prompt))
			   (line-beginning-position))
		     (line-beginning-position)))
       (import-statement
	(when (string-match-p
	       (rx (* space) word-start (or "from" "import") word-end space)
	       (buffer-substring-no-properties line-start (point)))
	  (buffer-substring-no-properties line-start (point))))
       (start
	(save-excursion
	  (if (not (re-search-backward
		    (py-rx
		     (or whitespace open-paren close-paren string-delimiter))
		    line-start
		    t 1))
	      line-start
	    (forward-char (length (match-string-no-properties 0)))
	    (point))))
       (end (point))
              (completion-fn
	(with-current-buffer (process-buffer process)
	  #'py-shell-completion-get-completions)))
    (list start end
          (completion-table-dynamic
           (apply-partially
            completion-fn
            process import-statement)))))

(defun py-comint-watch-for-first-prompt-output-filter (output)
  "Run `py-shell-first-prompt-hook' when first prompt is found in OUTPUT."
  (when (not py-shell--first-prompt-received)
    (set (make-local-variable 'py-shell--first-prompt-received-output-buffer)
         (concat py-shell--first-prompt-received-output-buffer
                 (ansi-color-filter-apply output)))
    (when (py-shell-comint-end-of-output-p
           py-shell--first-prompt-received-output-buffer)
      (if (string-match-p
           (concat py-shell-prompt-pdb-regexp (rx eos))
           (or py-shell--first-prompt-received-output-buffer ""))
          ;; Skip pdb prompts and reset the buffer.
          (setq py-shell--first-prompt-received-output-buffer nil)
        (set (make-local-variable 'py-shell--first-prompt-received) t)
        (setq py-shell--first-prompt-received-output-buffer nil)
        (with-current-buffer (current-buffer)
          (let ((inhibit-quit nil))
            (run-hooks 'py-shell-first-prompt-hook))))))
  output)

(defun python-shell-package-enable (directory package)
  "Add DIRECTORY parent to $PYTHONPATH and enable PACKAGE."
  (interactive
   (let* ((dir (expand-file-name
                (read-directory-name
                 "Package root: "
                 (file-name-directory
                  (or (buffer-file-name) default-directory)))))
          (name (completing-read
                 "Package: "
                 (python-util-list-packages
                  dir py-shell--package-depth))))
     (list dir name)))
  (py-shell-send-string
   (format
    (concat
     "import os.path;import sys;"
     "sys.path.append(os.path.dirname(os.path.dirname('''%s''')));"
     "__package__ = '''%s''';"
     "import %s")
    directory package package)
   (or (get-buffer-process (current-buffer)) (get-buffer-process (py-shell)))))

(defun py-comint-postoutput-scroll-to-bottom (output)
  "Faster version of `comint-postoutput-scroll-to-bottom'.
Avoids `recenter' calls until OUTPUT is completely sent."
  (when (and (not (string= "" output))
             (py-shell-comint-end-of-output-p
              (ansi-color-filter-apply output)))
    (comint-postoutput-scroll-to-bottom output))
  output)

(defun py-shell-font-lock-get-or-create-buffer ()
  "Get or create a font-lock buffer for current inferior process."
  (with-current-buffer (current-buffer)
    (if py-shell--font-lock-buffer
        py-shell--font-lock-buffer
      (let ((process-name
             (process-name (get-buffer-process (current-buffer)))))
        (generate-new-buffer
         (format " *%s-font-lock*" process-name))))))

(defun py-font-lock-kill-buffer ()
  "Kill the font-lock buffer safely."
  (when (and py-shell--font-lock-buffer
             (buffer-live-p py-shell--font-lock-buffer))
    (kill-buffer py-shell--font-lock-buffer)
    (when (derived-mode-p 'py-shell-mode)
      (setq py-shell--font-lock-buffer nil))))

(defmacro py-shell-font-lock-with-font-lock-buffer (&rest body)
  "Execute the forms in BODY in the font-lock buffer.
The value returned is the value of the last form in BODY.  See
also `with-current-buffer'."
  (declare (indent 0) (debug t))
  `(save-current-buffer
     (when (not (and py-shell--font-lock-buffer
		     (get-buffer py-shell--font-lock-buffer)))
       (setq py-shell--font-lock-buffer
	     (py-shell-font-lock-get-or-create-buffer)))
     (set-buffer py-shell--font-lock-buffer)
     (when (not font-lock-mode)
       (font-lock-mode 1))
     (set (make-local-variable 'delay-mode-hooks) t)
     (let (py-smart-indentation)
       (when (not (derived-mode-p 'python-mode))
	 (python-mode))
       ,@body)))

(defun py-shell-font-lock-cleanup-buffer ()
  "Cleanup the font-lock buffer.
Provided as a command because this might be handy if something
goes wrong and syntax highlighting in the shell gets messed up."
  (interactive)
  (with-current-buffer (current-buffer)
    (py-shell-font-lock-with-font-lock-buffer
      (erase-buffer))))

(defun py-shell-font-lock-comint-output-filter-function (output)
  "Clean up the font-lock buffer after any OUTPUT."
  (if (and (not (string= "" output))
           ;; Is end of output and is not just a prompt.
           (not (member
                 (py-shell-comint-end-of-output-p
                  (ansi-color-filter-apply output))
                 '(nil 0))))
      ;; If output is other than an input prompt then "real" output has
      ;; been received and the font-lock buffer must be cleaned up.
      (py-shell-font-lock-cleanup-buffer)
    ;; Otherwise just add a newline.
    (py-shell-font-lock-with-font-lock-buffer
      (goto-char (point-max))
      (newline 1)))
  output)

(defun py-font-lock-post-command-hook ()
  "Fontifies current line in shell buffer."
  (let ((prompt-end
	 (or (cdr (python-util-comint-last-prompt))
	     (progn (sit-for 0.1)
		    (cdr (python-util-comint-last-prompt))))))
    (when (and prompt-end (> (point) prompt-end)
               (process-live-p (get-buffer-process (current-buffer))))
      (let* ((input (buffer-substring-no-properties
                     prompt-end (point-max)))
             (deactivate-mark nil)
             (start-pos prompt-end)
             (buffer-undo-list t)
             (font-lock-buffer-pos nil)
             (replacement
              (py-shell-font-lock-with-font-lock-buffer
                (delete-region (line-beginning-position)
                               (point-max))
                (setq font-lock-buffer-pos (point))
                (insert input)
                ;; Ensure buffer is fontified, keeping it
                ;; compatible with Emacs < 24.4.
		(when py-shell-fontify-p
		    (if (fboundp 'font-lock-ensure)
			(funcall 'font-lock-ensure)
		      (font-lock-default-fontify-buffer)))
                (buffer-substring font-lock-buffer-pos
                                  (point-max))))
             (replacement-length (length replacement))
             (i 0))
        ;; Inject text properties to get input fontified.
        (while (not (= i replacement-length))
          (let* ((plist (text-properties-at i replacement))
                 (next-change (or (next-property-change i replacement)
                                  replacement-length))
                 (plist (let ((face (plist-get plist 'face)))
                          (if (not face)
                              plist
                            ;; Replace FACE text properties with
                            ;; FONT-LOCK-FACE so input is fontified.
                            (plist-put plist 'face nil)
                            (plist-put plist 'font-lock-face face)))))
            (set-text-properties
             (+ start-pos i) (+ start-pos next-change) plist)
            (setq i next-change)))))))

(defun py-shell-font-lock-turn-on (&optional msg)
  "Turn on shell font-lock.
With argument MSG show activation message."
  (interactive "p")
  (save-current-buffer
    (py-font-lock-kill-buffer)
    (set (make-local-variable 'py-shell--font-lock-buffer) nil)
    (add-hook 'post-command-hook
	      #'py-font-lock-post-command-hook nil 'local)
    (add-hook 'kill-buffer-hook
              #'py-font-lock-kill-buffer nil 'local)
    (add-hook 'comint-output-filter-functions
              #'py-shell-font-lock-comint-output-filter-function
              'append 'local)
    (when msg
      (message "Shell font-lock is enabled"))))

(defun py-shell-font-lock-turn-off (&optional msg)
  "Turn off shell font-lock.
With argument MSG show deactivation message."
  (interactive "p")
  (with-current-buffer (current-buffer)
    (py-font-lock-kill-buffer)
    (when (py-util-comint-last-prompt)
      ;; Cleanup current fontification
      (remove-text-properties
       (cdr (py-util-comint-last-prompt))
       (line-end-position)
       '(face nil font-lock-face nil)))
    (set (make-local-variable 'py-shell--font-lock-buffer) nil)
    (remove-hook 'post-command-hook
                 #'py-font-lock-post-command-hook 'local)
    (remove-hook 'kill-buffer-hook
                 #'py-font-lock-kill-buffer 'local)
    (remove-hook 'comint-output-filter-functions
                 #'py-shell-font-lock-comint-output-filter-function
                 'local)
    (when msg
      (message "Shell font-lock is disabled"))))

(defun py-shell-font-lock-toggle (&optional msg)
  "Toggle font-lock for shell.
With argument MSG show activation/deactivation message."
  (interactive "p")
  (with-current-buffer (current-buffer)
    (set (make-local-variable 'py-shell-fontify-p)
         (not py-shell-fontify-p))
    (if py-shell-fontify-p
        (py-shell-font-lock-turn-on msg)
      (py-shell-font-lock-turn-off msg))
    py-shell-fontify-p))

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
    (when (string-match (py-rx coding-cookie) first-two-lines)
      (intern (match-string-no-properties 1 first-two-lines)))))

(unless (functionp 'file-local-name)
  (defun file-local-name (file)
    "Return the local name component of FILE.
This function removes from FILE the specification of the remote host
and the method of accessing the host, leaving only the part that
identifies FILE locally on the remote system.
The returned file name can be used directly as argument of
`process-file', `start-file-process', or `shell-command'."
    (or (file-remote-p file 'localname) file)))

(provide 'python-components-extra)
;;; python-components-extra.el ends here
