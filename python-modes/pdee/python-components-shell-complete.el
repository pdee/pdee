;;; python-components-shell-complete.el -- Add suport for completion in py-shell

;; Maintainer: Andreas Roehler <andreas.roehler@online.de>
;; Original Author: Lukasz Pankowski

;; py-comint-output-filter-function is modified version from:
;; python-mode.el --- Major mode for editing Python programs

;; Copyright (C) 1992,1993,1994  Tim Peters

;; Author: 2003-2004 http://sf.net/projects/python-mode
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

;; To get tab completion in Python shell you may add these line to
;; your ~/.emacs file:
;;
;; (add-hook 'py-shell-hook
;;           '(lambda ()
;;              (require 'py-shell-complete) ; nil t)
;;              (when (functionp 'py-shell-complete)
;;                ;; this should be set in py-shell
;;                (setq comint-input-sender 'py-shell-simple-send)
;;                (local-set-key [tab] 'py-shell-complete))))

;;; Code
(require 'comint)
(require 'python-components-macros)

(defvar py-shell-input-lines nil
  "Collect input lines send interactively to the Python process in
order to allow injecting completion command between keyboard interrupt
and resending the lines later. The lines are stored in reverse order")

;;; need to clear py-shell-input-lines if primary prompt found

(defun py-comint-output-filter-function (string)
  "Watch output for Python prompt and exec next file waiting in queue.
This function is appropriate for `comint-output-filter-functions'."
  ;; TBD: this should probably use split-string
  (when (and (or (string-equal string ">>> ")
		 (and (>= (length string) 5)
		      (string-equal (substring string -5) "\n>>> ")))
	     (or (setq py-shell-input-lines nil)
		 py-file-queue))
    (pop-to-buffer (current-buffer))
    (ignore-errors (delete-file (car py-file-queue)))
    (setq py-file-queue (cdr py-file-queue))
    (if py-file-queue
	(let ((pyproc (get-buffer-process (current-buffer))))
	  (py-execute-file pyproc (car py-file-queue))))))

;;;

(defun py-shell-simple-send (proc string)
  (setq py-shell-input-lines (cons string py-shell-input-lines))
  (comint-simple-send proc string))

(if (functionp 'comint-redirect-send-command-to-process)
    (progn
      (defalias
	'py-shell-redirect-send-command-to-process
	'comint-redirect-send-command-to-process)
      (defalias
	'py-shell-dynamic-simple-complete
	'comint-dynamic-simple-complete))

  ;; XEmacs

  (make-variable-buffer-local 'comint-redirect-completed)
  (defvar py-shell-redirect-output-buffer nil)
  (make-variable-buffer-local 'py-shell-redirect-output-buffer)
  (defvar py-shell-redirect-orginal-output-filter nil)
  (make-variable-buffer-local 'py-shell-redirect-orginal-output-filter)

  (defun py-shell-redirect-filter-function (proc string)
    (let ((procbuf (process-buffer proc))
	  outbuf prompt-pos)
      (with-current-buffer procbuf
	(setq outbuf py-shell-redirect-output-buffer
	      prompt-pos (string-match comint-prompt-regexp string)))
      (if prompt-pos
	  (setq string (substring string 0 prompt-pos)))
      (save-excursion
	(set-buffer outbuf)
	(goto-char (point-max))
	(insert string))
      (if prompt-pos
	  (with-current-buffer procbuf
	    (set-process-filter proc py-shell-redirect-orginal-output-filter)
	    (setq comint-redirect-completed t))))
    "")

  (defun py-shell-redirect-send-command-to-process
    (command output-buffer process echo no-display)
    "Note: ECHO and NO-DISPLAY are ignored"
    ;; prepear
    (with-current-buffer (process-buffer process)
      (setq comint-redirect-completed nil
	    py-shell-redirect-output-buffer (get-buffer output-buffer)
	    py-shell-redirect-orginal-output-filter (process-filter process)))
    (set-process-filter process 'py-shell-redirect-filter-function)
    ;; run
    (comint-simple-send process command))

  (defun py-shell-dynamic-simple-complete (stub candidates)
    (let ((completion (try-completion stub (mapcar 'list candidates))))
      (cond
       ((null completion)
	nil)
       ((eq completion t)
	(message "Sole completion")
	'sole)
       ((> (length completion) (length stub))
	(insert (substring completion (length stub)))
	(if (eq (try-completion completion (mapcar 'list candidates)) t)
	    (progn (message "Completed")
		   'sole)
	  (message "Partially completed")
	  'partial))
       (t
	(with-output-to-temp-buffer "*Completions*"
	  (display-completion-list (sort candidates 'string<)))
	'listed)))))

(defun py-shell-execute-string-now (string)
  "Send to Python interpreter process PROC \"exec STRING in {}\".
and return collected output"
  (let* ((proc (get-process py-which-bufname))
	 (cmd (format "exec '''%s''' in {}"
		      (mapconcat 'identity (split-string string "\n") "\\n")))
         (procbuf (process-buffer proc))
         (outbuf (get-buffer-create " *pyshellcomplete-output*"))
         (lines (reverse py-shell-input-lines)))
    (if (and proc (not py-file-queue))
	(unwind-protect
	    (condition-case nil
		(progn
		  (if lines
		      (with-current-buffer procbuf
			(py-shell-redirect-send-command-to-process
			 "\C-c" outbuf proc nil t)
			;; wait for output
			(while (not comint-redirect-completed)
			  (accept-process-output proc 1))))
		  (with-current-buffer outbuf
		    (delete-region (point-min) (point-max)))
		  (with-current-buffer procbuf
		    (py-shell-redirect-send-command-to-process
		     cmd outbuf proc nil t)
		    (while (not comint-redirect-completed) ; wait for output
		      (accept-process-output proc 1)))
		  (with-current-buffer outbuf
		    (buffer-substring (point-min) (point-max))))
	      (quit (with-current-buffer procbuf
		      (interrupt-process proc comint-ptyp)
		      (while (not comint-redirect-completed) ; wait for output
			(accept-process-output proc 1)))
		    (signal 'quit nil)))
          (if (with-current-buffer procbuf comint-redirect-completed)
              (while lines
                (with-current-buffer procbuf
                  (py-shell-redirect-send-command-to-process
                   (car lines) outbuf proc nil t))
                (accept-process-output proc 1)
                (setq lines (cdr lines))))))))

;;;;

(defun py-dot-word-before-point ()
  (buffer-substring
   (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point))
   (point)))

(defun py-shell-complete (&optional arg)
  "Complete word before point, if any. Otherwise insert TAB.
With prefix argument \\[universal-argument] a TAB is inserted. "
  (interactive "*P")
  (if (eq 4 (prefix-numeric-value arg))
      (insert-tab)
    (let ((orig (point))
          (word (py-dot-word-before-point))
          result)
      (if (equal word "")
          (tab-to-tab-stop)	   ; non nil so the completion is over
        (setq result (py-shell-execute-string-now (format "
def print_completions(namespace, text, prefix=''):
   for name in namespace:
       if name.startswith(text):
           print prefix + name

def complete(text):
    import __builtin__
    import __main__
    if '.' in text:
        terms = text.split('.')
        try:
            if hasattr(__main__, terms[0]):
                obj = getattr(__main__, terms[0])
            else:
                obj = getattr(__builtin__, terms[0])
            for term in terms[1:-1]:
                obj = getattr(obj, term)
            print_completions(dir(obj), terms[-1], text[:text.rfind('.') + 1])
        except AttributeError:
            pass
    else:
        import keyword
        print_completions(keyword.kwlist, text)
        print_completions(dir(__builtin__), text)
        print_completions(dir(__main__), text)
complete('%s')
" word)))
        (if (eq result nil)
            (message "Could not do completion as the Python process is busy")
          (let ((comint-completion-addsuffix nil)
                (completions (if (split-string "\n" "\n")
                                 (split-string result "\n" t) ; XEmacs
                               (split-string result "\n"))))
            (ignore-errors (py-shell-dynamic-simple-complete word completions)))))
      ;; word before has not further completions
      (when (eq (point) orig)
        (insert-tab)))))


(defun py-switch-to-python (eob-p)
  "Switch to the Python process buffer, maybe starting new process.

With prefix arg, position cursor at end of buffer."
  (interactive "P")
  (pop-to-buffer (process-buffer (python-proc)) t) ;Runs python if needed.
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defun py-send-region-and-go (start end)
  "Send the region to the inferior Python process.

Then switch to the process buffer."
  (interactive "r")
  (py-send-region start end)
  (py-switch-to-python t))

(defun py-load-file (file-name)
  "Load a Python file FILE-NAME into the inferior Python process.

If the file has extension `.py' import or reload it as a module.
Treating it as a module keeps the global namespace clean, provides
function location information for debugging, and supports users of
module-qualified names."
  (interactive (comint-get-source "Load Python file: " python-prev-dir/file
				  python-source-modes
				  t))	; because execfile needs exact name
  (comint-check-source file-name)     ; Check to see if buffer needs saving.
  (setq python-prev-dir/file (cons (file-name-directory file-name)
				   (file-name-nondirectory file-name)))
  (with-current-buffer (process-buffer (python-proc)) ;Runs python if needed.
    ;; Fixme: I'm not convinced by this logic from python-mode.el.
    (python-send-command
     (if (string-match "\\.py\\'" file-name)
	 (let ((module (file-name-sans-extension
			(file-name-nondirectory file-name))))
	   (format "emacs.eimport(%S,%S)"
		   module (file-name-directory file-name)))
       (format "execfile(%S)" file-name)))
    (message "%s loaded" file-name)))

(defun py-set-proc ()
  "Set the default value of `python-buffer' to correspond to this buffer.

If the current buffer has a local value of `python-buffer', set the
default (global) value to that.  The associated Python process is
the one that gets input from \\[py-send-region] et al when used
in a buffer that doesn't have a local value of `python-buffer'."
  (interactive)
  (if (local-variable-p 'python-buffer)
      (setq-default python-buffer python-buffer)
    (error "No local value of `python-buffer'")))

;;; Context-sensitive help.

(defvar view-return-to-alist)

(defvar python-imports)			; forward declaration
(make-variable-buffer-local 'python-imports)

(defun py-find-imports ()
  "Find top-level imports, updating `python-imports'."
  (interactive)
  (save-excursion
    (let (lines)
      (goto-char (point-min))
      (while (re-search-forward "^import\\_>[ \n\t]\\|^from\\_>[ \n\t]" nil t)
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
                       ;; This is probably best left out since you're unlikely to need the
                       ;; doc for a function in the buffer and the import will lose if the
                       ;; Python sub-process' working directory isn't the same as the
                       ;; buffer's.
                       ;; 			 (if buffer-file-name
                       ;; 			     (concat
                       ;; 			      "import "
                       ;; 			      (file-name-sans-extension
                       ;; 			       (file-name-nondirectory buffer-file-name))))
                       (nreverse lines))
              "None"))
      (when lines
        (set-text-properties 0 (length python-imports) nil python-imports)
        ;; The output ends up in the wrong place if the string we
        ;; send contains newlines (from the imports).
        (setq python-imports
              (replace-regexp-in-string "\n" "\\n"
                                        (format "%S" python-imports) t t))))))


;; Author: Lukasz Pankowski, patch sent for lp:328836
(defvar py-shell-input-lines nil
  "Collect input lines send interactively to the Python process in
order to allow injecting completion command between keyboard interrupt
and resending the lines later. The lines are stored in reverse order")

;;; need to clear py-shell-input-lines if primary prompt found

;; (defun py-comint-output-filter-function (string)
;;   "Watch output for Python prompt and exec next file waiting in queue.
;; This function is appropriate for `comint-output-filter-functions'."
;;   ;; TBD: this should probably use split-string
;;   (when (and (or (string-equal string ">>> ")
;; 		 (and (>= (length string) 5)
;; 		      (string-equal (substring string -5) "\n>>> ")))
;; 	     (or (setq py-shell-input-lines nil)
;; 		 py-file-queue))
;;     (pop-to-buffer (current-buffer))
;;     (py-safe (delete-file (car py-file-queue)))
;;     (setq py-file-queue (cdr py-file-queue))
;;     (if py-file-queue
;; 	(let ((pyproc (get-buffer-process (current-buffer))))
;; 	  (py-execute-file pyproc (car py-file-queue))))
;;     ))

;;;

(defun py-shell-simple-send (proc string)
  (setq py-shell-input-lines (cons string py-shell-input-lines))
  (comint-simple-send proc string))

(if (functionp 'comint-redirect-send-command-to-process)
    (progn
      (defalias
	'py-shell-redirect-send-command-to-process
	'comint-redirect-send-command-to-process)
      (defalias
	'py-shell-dynamic-simple-complete
	'comint-dynamic-simple-complete))

  ;; XEmacs

  (make-variable-buffer-local 'comint-redirect-completed)
  (defvar py-shell-redirect-output-buffer)
  (make-variable-buffer-local 'py-shell-redirect-output-buffer)
  (defvar py-shell-redirect-orginal-output-filter)
  (make-variable-buffer-local 'py-shell-redirect-orginal-output-filter)

  (defun py-shell-redirect-filter-function (proc string)
    (let ((procbuf (process-buffer proc))
	  outbuf prompt-pos)
      (with-current-buffer procbuf
	(setq outbuf py-shell-redirect-output-buffer
	      prompt-pos (string-match comint-prompt-regexp string)))
      (if prompt-pos
	  (setq string (substring string 0 prompt-pos)))
      (save-excursion
	(set-buffer outbuf)
	(goto-char (point-max))
	(insert string))
      (if prompt-pos
	  (with-current-buffer procbuf
	    (set-process-filter proc py-shell-redirect-orginal-output-filter)
	    (setq comint-redirect-completed t))))
    "")

  (defun py-shell-redirect-send-command-to-process
    (command output-buffer process echo no-display)
    "Note: ECHO and NO-DISPLAY are ignored"
    ;; prepear
    (with-current-buffer (process-buffer process)
      (setq comint-redirect-completed nil
	    py-shell-redirect-output-buffer (get-buffer output-buffer)
	    py-shell-redirect-orginal-output-filter (process-filter process)))
    (set-process-filter process 'py-shell-redirect-filter-function)
    ;; run
    (comint-simple-send process command))

  (defun py-shell-dynamic-simple-complete (stub candidates)
    (let ((completion (try-completion stub (mapcar 'list candidates))))
      (cond
       ((null completion)
	nil)
       ((eq completion t)
	(message "Sole completion")
	'sole)
       ((> (length completion) (length stub))
	(insert (substring completion (length stub)))
	(if (eq (try-completion completion (mapcar 'list candidates)) t)
	    (progn (message "Completed")
		   'sole)
	  (message "Partially completed")
	  'partial))
       (t
	(with-output-to-temp-buffer "*Completions*"
	  (display-completion-list (sort candidates 'string<)))
	'listed)))))

;; from pycomplete.el
(defun py-find-global-imports ()
  (save-excursion
    (let (first-class-or-def imports)
      (goto-char (point-min))
      (setq first-class-or-def
	    (re-search-forward "^ *\\(def\\|class\\) " nil t))
      (goto-char (point-min))
      (while (re-search-forward
	      "^\\(import \\|from \\([A-Za-z_][A-Za-z_0-9]*\\) import \\).*"
	      nil t)
	(setq imports (append imports
			      (list (buffer-substring
				     (match-beginning 0)
				     (match-end 0))))))
      imports)))

(defun py-shell-execute-string-now (string)
  "Send to Python interpreter process PROC \"exec STRING in {}\".
and return collected output"
  (let* ((proc
          (get-process (py-process-name)))
         (procbuf (if (buffer-live-p (get-buffer (process-buffer proc)))
                      (get-buffer (process-buffer proc))
                    (py-shell nil nil py-shell-name)
                    (py-shell-execute-string-now string)))
	 (cmd (format "exec '''%s''' in {}"
		      (mapconcat 'identity (split-string string "\n") "\\n")))
         (outbuf (get-buffer-create " *pyshellcomplete-output*"))
         (lines (reverse py-shell-input-lines)))
    (if (and proc (not py-file-queue))
	(unwind-protect
	    (condition-case nil
		(progn
		  (if lines
		      (with-current-buffer procbuf
			(py-shell-redirect-send-command-to-process
			 "\C-c" outbuf proc nil t)
			;; wait for output
			(while (not comint-redirect-completed)
			  (accept-process-output proc 1))))
		  (with-current-buffer outbuf
		    (delete-region (point-min) (point-max)))
		  (with-current-buffer procbuf
		    (py-shell-redirect-send-command-to-process
		     cmd outbuf proc nil t)
		    (while (not comint-redirect-completed) ; wait for output
		      (accept-process-output proc 1)))
		  (with-current-buffer outbuf
		    (buffer-substring (point-min) (point-max))))
	      (quit (with-current-buffer procbuf
		      (interrupt-process proc comint-ptyp)
		      (while (not comint-redirect-completed) ; wait for output
			(accept-process-output proc 1)))
		    (signal 'quit nil)))
          (if (with-current-buffer procbuf comint-redirect-completed)
              (while lines
                (with-current-buffer procbuf
                  (py-shell-redirect-send-command-to-process
                   (car lines) outbuf proc nil t))
                (accept-process-output proc 1)
                (setq lines (cdr lines))))))))

(defun py-proc ()
  "Return the current Python process.
See variable `python-buffer'.  Starts a new process if necessary."
  ;; Fixme: Maybe should look for another active process if there
  ;; isn't one for `python-buffer'.
  (unless (comint-check-proc python-buffer)
    (run-python nil t))
  (get-buffer-process (if (derived-mode-p 'inferior-python-mode)
			  (current-buffer)
			python-buffer)))

(defun py-send-receive (string)
  "Send STRING to inferior Python (if any) and return result.

The result is what follows `_emacs_out' in the output.
This is a no-op if `python-check-comint-prompt' returns nil."
  (py-send-string string)
  (let ((proc (py-proc)))
    (with-current-buffer (process-buffer proc)
      (when (python-check-comint-prompt proc)
	(set (make-local-variable 'python-preoutput-result) nil)
	(while (progn
		 (accept-process-output proc 5)
		 (null python-preoutput-result)))
	(prog1 python-preoutput-result
	  (kill-local-variable 'python-preoutput-result))))))

;;; IPython Completion start

;; see also
;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2008-01/msg00076.html

(defvar ipython-completion-command-string nil
  "Either ipython0.10-completion-command-string or ipython0.11-completion-command-string.

ipython0.11-completion-command-string also covers version 0.12")
;; (make-variable-buffer-local 'ipython-completion-command-string)

(defvar ipython0.10-completion-command-string
  "print(';'.join(__IP.Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n"
  "The string send to ipython to query for all possible completions")

;; (setq ipython0.10-completion-command-string "print(';'.join(__IP.Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n")

;; https://github.com/ipython
;; commit 1dd379d857f836c9e8af4576cecaeb413fcba4e5
;; Date:   Tue Feb 14 19:47:04 2012 -0800
;; "print(';'.join(get_ipython().complete('%s', '%s')[1])) #PYTHON-MODE SILENT\n"
(defvar ipython0.11-completion-command-string
  "print(';'.join(get_ipython().Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n"
  "The string send to ipython to query for all possible completions")

(defun py-shell-complete ()
  "Complete word before point, if any. Otherwise insert TAB. "
  (interactive)
  ;; make sure, a process exists
  ;; (unless (and (processp (get-process (py-process-name py-shell-name)))
  ;;              (buffer-live-p (get-buffer (py-process-name py-shell-name))))
  ;;   (py-shell nil nil py-shell-name))
  (let ((word (py-dot-word-before-point))
        result)
    (if (string= word "")
        (tab-to-tab-stop)	   ; non nil so the completion is over
      (setq result (py-shell-execute-string-now (format "
def print_completions(namespace, text, prefix=''):
   for name in namespace:
       if name.startswith(text):
           print prefix + name

def complete(text):
    import __builtin__
    import __main__
    if '.' in text:
        terms = text.split('.')
        try:
            if hasattr(__main__, terms[0]):
                obj = getattr(__main__, terms[0])
            else:
                obj = getattr(__builtin__, terms[0])
            for term in terms[1:-1]:
                obj = getattr(obj, term)
            print_completions(dir(obj), terms[-1], text[:text.rfind('.') + 1])
        except AttributeError:
            pass
    else:
        import keyword
        print_completions(keyword.kwlist, text)
        print_completions(dir(__builtin__), text)
        print_completions(dir(__main__), text)
complete('%s')
" word)))
      (if (eq result nil)
          (message "Could not do completion as the Python process is busy")
        (let ((comint-completion-addsuffix nil)
              (completions (if (split-string "\n" "\n")
                               (split-string result "\n" t) ; XEmacs
                             (split-string result "\n"))))
          (py-shell-dynamic-simple-complete word completions))))))

;; (defun ipython-complete (&optional done)
;;   "Complete the python symbol before point.

(defun ipython-complete (&optional done)
  "Complete the python symbol before point.

If no completion available, insert a TAB.
Returns the completed symbol, a string, if successful, nil otherwise."
  (interactive "*")
  (let* (py-split-windows-on-execute-p
         py-shell-switch-buffers-on-execute-p
         (done done)
         (orig (point)) 
         (oldbuf (current-buffer))
         (ugly-return nil)
         (sep ";")
         (py-which-bufname "IPython-complete")
         ;; (process-connection-type 'pty)
         ;; (cond ((get-buffer-process  "IPython-complete")
         ;; "IPython-complete")
         ;; ((string-match "[Ii][Pp]ython" py-shell-name)
         ;; py-shell-name)
         ;; (t "ipython"))
         (python-process (or (get-process py-which-bufname)
                             (progn
                               (setq done (not done))
                               (get-buffer-process (py-shell nil nil "ipython" 'noswitch nil py-which-bufname))
                               )))
         ;; maybe in py-shell now
         (set-buffer oldbuf)
         (goto-char orig)
         (beg (progn (set-buffer oldbuf)(save-excursion (skip-chars-backward "a-z0-9A-Z_." (point-at-bol))
                                                        (point))))
         (end (point))
         (pattern (buffer-substring-no-properties beg end))
         (comint-output-filter-functions
          (delq 'py-comint-output-filter-function comint-output-filter-functions))
         (comint-output-filter-functions
          (append comint-output-filter-functions
                  '(ansi-color-filter-apply
                    (lambda (string)
                      (setq ugly-return (concat ugly-return string))
                      (delete-region comint-last-output-start
                                     (process-mark (get-buffer-process (current-buffer))))))))
         completion completions completion-table)
    (if (string= pattern "")
        (tab-to-tab-stop)
      (process-send-string python-process
                           (format (py-set-ipython-completion-command-string (downcase (process-name python-process))) pattern))
      (accept-process-output python-process)
      (setq completions
            (split-string (substring ugly-return 0 (position ?\n ugly-return)) sep))
      (setq completion-table (loop for str in completions
                                   collect (list str nil)))
      (setq completion (try-completion pattern completion-table))
      (cond ((eq completion t))
            ((null completion)
             ;; workaround: if an (I)Python shell didn't run
             ;; before, first completion are not delivered
             (if done (ipython-complete done)
               (message "Can't find completion for \"%s\"" pattern)
               (ding)))
            ((not (string= pattern completion))
             (delete-region beg end)
             (insert completion))
            (t
             (message "Making completion list...")
             (with-output-to-temp-buffer "*Python Completions*"
               (display-completion-list (all-completions pattern completion-table)))
             (message "Making completion list...%s" "done"))))
    completion))

(provide 'python-components-shell-complete)

;; pyshell-complete.el ends here
