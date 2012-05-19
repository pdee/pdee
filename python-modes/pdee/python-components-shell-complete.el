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
;;     (ignore-errors (delete-file (car py-file-queue)))
;;     (setq py-file-queue (cdr py-file-queue))
;;     (if py-file-queue
;; 	(let ((pyproc (get-buffer-process (current-buffer))))
;; 	  (py-execute-file pyproc (car py-file-queue))))))

(defun py-shell-simple-send (proc string)
  (setq py-shell-input-lines (cons string py-shell-input-lines))
  (comint-simple-send proc string))

(defalias
  'py-shell-redirect-send-command-to-process
  'comint-redirect-send-command-to-process)
(defalias
  'py-shell-dynamic-simple-complete
  'comint-dynamic-simple-complete)

(defun py-shell-execute-string-now (string &optional shell)
  "Send to Python interpreter process PROC \"exec STRING in {}\".
and return collected output"
  (let* ((proc (cond (shell
                      (or (get-process shell)
                          (prog1
                              (get-buffer-process (py-shell nil nil shell))
                            (sit-for 0.1))))
                     (t (or (get-buffer-process (current-buffer))
                            (get-buffer-process (py-shell))))))
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
                        (comint-redirect-send-command-to-process
                         "\C-c" outbuf proc nil t)
                        ;; wait for output
                        (while (not comint-redirect-completed)
                          (accept-process-output proc 1))))
                  (with-current-buffer outbuf
                    (delete-region (point-min) (point-max)))
                  (with-current-buffer procbuf
                    (comint-redirect-send-command-to-process
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
                  (comint-redirect-send-command-to-process
                   (car lines) outbuf proc nil t))
                (accept-process-output proc 1)
                (setq lines (cdr lines))))))))

(defun py-dot-word-before-point ()
  (buffer-substring-no-properties
   (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point))
   (point)))

(defun py-switch-to-python (eob-p)
  "Switch to the Python process buffer, maybe starting new process.

With prefix arg, position cursor at end of buffer."
  (interactive "P")
  (pop-to-buffer (process-buffer (py-proc)) t) ;Runs python if needed.
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
  (with-current-buffer (process-buffer (py-proc)) ;Runs python if needed.
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

;; (defun py-send-receive (string)
;;   "Send STRING to inferior Python (if any) and return result.
;;
;; The result is what follows `_emacs_out' in the output.
;; This is a no-op if `python-check-comint-prompt' returns nil."
;;   (py-send-string string)
;;   (let ((proc (py-proc)))
;;     (with-current-buffer (process-buffer proc)
;;       (when (python-check-comint-prompt proc)
;; 	(set (make-local-variable 'python-preoutput-result) nil)
;; 	(while (progn
;; 		 (accept-process-output proc 5)
;; 		 (null python-preoutput-result)))
;; 	(prog1 python-preoutput-result
;; 	  (kill-local-variable 'python-preoutput-result))))))

(defun py-completion-at-point ()
  "An alternative completion, similar the way python.el does it. "
  (interactive "*")
  (python-find-imports)
  (let* ((start (when (skip-chars-backward "[[:alnum:]_]")(point)))
         (end (progn (skip-chars-forward "[[:alnum:]_]")(point)))
         (completion (when start
                       (py-symbol-completions (buffer-substring-no-properties start end)))))
    (if completion
        (progn
          (delete-region start end)
          (insert (car completion)))
      (tab-to-tab-stop))))

;; started from python.el's python-completion-at-point
(defun py-script-complete ()
  (interactive "*")
  (let ((end (point))
	(start (save-excursion
		 (and (re-search-backward
		       (rx (or buffer-start (regexp "[^[:alnum:]._]"))
			   (group (1+ (regexp "[[:alnum:]._]"))) point)
		       nil t)
		      (match-beginning 1)))))
    (when start
      (list start end
            (completion-table-dynamic 'py-symbol-completions)))))

(defun py-symbol-completions (symbol)
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

(defun py-python-script-complete (&optional shell)
  "Complete word before point, if any. Otherwise insert TAB. "
  (interactive)
  (let* (py-split-windows-on-execute-p
         py-switch-buffers-on-execute-p
         (shell (or shell py-local-versioned-command))
         (orig (point))
         (beg (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point)))
         (end (point))
         (word (buffer-substring-no-properties beg end))
         (imports (py-find-imports)))
    (cond ((string= word "")
           (message "%s" "Nothing to complete. ")
           (tab-to-tab-stop))
          (t (or (setq proc (get-buffer-process shell))
                 (setq proc (get-buffer-process (py-shell nil nil shell))))
             (if (processp proc)
                 (progn
                   ;; when completing instances, make them known
                   (when (string-match "^\\(^[a-zA-Z0-9_]+\\)\\.\\([a-zA-Z0-9_]+\\)$" word)
                     ;; (message "%s" (match-string 1 word))
                     (save-excursion
                       (goto-char (point-min))
                       (when (re-search-forward (concat "^[ \t]*" (match-string-no-properties 1 word) "[ \t]*=[ \t]*[^ \n\r\f\t]+") nil t 1))
                       (if imports
                           (setq imports (concat imports (match-string-no-properties 0) ";"))
                         (setq imports (match-string-no-properties 0)))))
                   (unless (python-shell-completion--do-completion-at-point proc imports word)
                     (call-interactively 'dabbrev-expand))
                   nil)
               (error "No completion process at proc"))))))

(defun py-python2-shell-complete (&optional shell)
  (interactive)
  (let* (py-split-windows-on-execute-p
         py-switch-buffers-on-execute-p
         (shell (or shell py-local-versioned-command))
         (orig (point))
         (beg (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point)))
         (end (point))
         (word (buffer-substring-no-properties beg end)))
    (cond ((string= word "")
           (message "%s" "Nothing to complete. ")
           (tab-to-tab-stop))
          (t (or (setq proc (get-buffer-process shell))
                 (setq proc (get-buffer-process (py-shell nil nil shell))))
             (message "%s" (processp proc))
             (python-shell-completion--do-completion-at-point proc (buffer-substring-no-properties beg end) word)))))

(defun py-python3-shell-complete (&optional shell)
  "Complete word before point, if any. Otherwise insert TAB. "
  (interactive)
  (let* ((shell (or shell py-local-versioned-command))
         (orig (point))
         (beg (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point)))
         (end (point))
         (word (buffer-substring-no-properties beg end)))
    (cond ((string= word "")
           (message "%s" "Nothing to complete. ")
           (tab-to-tab-stop))
          (t
           (python-shell-completion--do-completion-at-point (get-buffer-process (current-buffer)) (buffer-substring-no-properties beg end) word)))))

(defun py-shell-complete (&optional shell)
  "Complete word before point, if any. Otherwise insert TAB. "
  (interactive)
  (let ((shell (or shell
                   (ignore-errors (process-name (get-buffer-process (current-buffer)))))))
    (if (or (eq major-mode 'comint-mode)(eq major-mode 'inferior-python-mode))
        ;;  complete in shell
        (if (string-match "[iI][pP]ython" shell)
            (ipython-complete)
          (let* ((orig (point))
                 (beg (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point)))
                 (end (point))
                 (word (buffer-substring-no-properties beg end)))
            (cond ((string= word "")
                   (message "%s" "Nothing to complete. ")
                   (tab-to-tab-stop))
                  ((string-match "[pP]ython3[^[:alpha:]]*$" shell)
                   (python-shell-completion--do-completion-at-point (get-buffer-process (current-buffer)) (buffer-substring-no-properties beg end) word))
                  (t (py-shell-complete-intern word beg end shell)))))
      ;; complete in script buffer
      (let* (py-split-windows-on-execute-p
             py-switch-buffers-on-execute-p
             (shell (or shell (py-choose-shell)))
             (proc (or (comint-check-proc (py-shell nil nil shell 'noswitch nil))
                       (py-shell nil nil shell 'noswitch nil)))
             (beg (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point)))
             (end (point))
             (word (buffer-substring-no-properties beg end)))
        (cond ((string= word "")
               (message "%s" "Nothing to complete. "))
              ((string-match "[iI][pP]ython" shell)
               (ipython-complete))
              ((string-match "[pP]ython3[^[:alpha:]]*$" shell)
               (python-shell-completion--do-completion-at-point proc (buffer-substring-no-properties beg end) word))
              (t (py-shell-complete-intern word beg end shell)))))))

(defun py-shell-complete-intern (word &optional beg end shell)
  (let (result)
    (setq result (py-shell-execute-string-now (format "
def print_completions(namespace, text, prefix=''):
   for name in namespace:
       if name.startswith(text):
           print(prefix + name)

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
complete('%s')" word) shell))
    (if (eq result nil)
        (message "Can't complete")
      (let ((comint-completion-addsuffix nil)
            (completions
             (sort
              (delete-dups (if (split-string "\n" "\n")
                               (split-string result "\n" t) ; XEmacs
                             (split-string result "\n")))
              #'string<)))
        (delete-region beg end)
        (insert (car completions)))
      ;; list-typ return required by `completion-at-point'
      (list beg end))))

;; (defun ipython-complete (&optional done)
;;   "Complete the python symbol before point.

(defun ipython-complete-py-shell-name (&optional done)
  "Complete the python symbol before point.

If no completion available, insert a TAB.
Returns the completed symbol, a string, if successful, nil otherwise.

Bug: if no IPython-shell is running, fails first time due to header returned, which messes up the result. Please repeat once then. "
  (interactive "*")
  (let* (py-split-windows-on-execute-p
         py-switch-buffers-on-execute-p
         (beg (progn (save-excursion (skip-chars-backward "a-z0-9A-Z_." (point-at-bol))
                                     (point))))
         (end (point))
         (pattern (buffer-substring-no-properties beg end))
         (sep ";")
         (python-process (or (get-buffer-process (current-buffer))
                             (get-buffer-process (py-shell))
                             (get-buffer-process (py-shell nil nil "ipython" 'noswitch nil))))

         (comint-output-filter-functions
          (delq 'py-comint-output-filter-function comint-output-filter-functions))
         (comint-output-filter-functions
          (append comint-output-filter-functions
                  '(ansi-color-filter-apply
                    (lambda (string)
                      (setq ugly-return (concat ugly-return string))
                      (delete-region comint-last-output-start
                                     (process-mark (get-buffer-process (current-buffer))))))))
         completion completions completion-table ugly-return)
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
             ;; if an (I)Python shell didn't run
             ;; before, first completion are not delivered
             ;; (if done (ipython-complete done)
             (message "Can't find completion for \"%s\"" pattern)
             (ding))
            ((not (string= pattern completion))
             (delete-region beg end)
             (insert completion))
            (t
             (message "Making completion list...")
             (with-output-to-temp-buffer "*Python Completions*"
               (display-completion-list (all-completions pattern completion-table)))
             (message "Making completion list...%s" "done"))))
    completion))

(defun ipython-complete (&optional done completion-command-string)
  "Complete the python symbol before point.

If no completion available, insert a TAB.
Returns the completed symbol, a string, if successful, nil otherwise.

Bug: if no IPython-shell is running, fails first time due to header returned, which messes up the result. Please repeat once then. "
  (interactive "*")
  (let* (py-split-windows-on-execute-p
         py-switch-buffers-on-execute-p
         (beg (progn (save-excursion (skip-chars-backward "a-z0-9A-Z_." (point-at-bol))
                                     (point))))
         (end (point))
         (pattern (buffer-substring-no-properties beg end))
         (sep ";")
         (pyshellname (py-choose-shell))
         (processlist (process-list))
         done
         (process
          (if ipython-complete-use-separate-shell-p
              (unless (comint-check-proc (process-name (get-buffer-process (get-buffer-create "*IPython-Complete*"))))
                (get-buffer-process (py-shell nil nil pyshellname 'noswitch nil "*IPython-Complete*")))
            (progn
              (while (and processlist (not done))
                (when (and
                       (string= pyshellname (process-name (car processlist)))
                       (processp (car processlist))
                       (setq done (car processlist))))
                (setq processlist (cdr processlist)))
              done)))
         (python-process (or process
                             (setq python-process (get-buffer-process (py-shell nil nil (if (string-match "[iI][pP]ython[^[:alpha:]]*$"  pyshellname) pyshellname "ipython") 'noswitch nil)))))
         (comint-output-filter-functions
          (delq 'py-comint-output-filter-function comint-output-filter-functions))
         (comint-output-filter-functions
          (append comint-output-filter-functions
                  '(ansi-color-filter-apply
                    (lambda (string)
                      (setq ugly-return (concat ugly-return string))
                      (delete-region comint-last-output-start
                                     (process-mark (get-buffer-process (current-buffer))))))))

         (ccs (or completion-command-string (py-set-ipython-completion-command-string
                                             (process-name python-process))))
         completion completions completion-table ugly-return)
    (if (string= pattern "")
        (tab-to-tab-stop)
      (process-send-string python-process (format ccs pattern))
      (accept-process-output python-process 5)
      (setq completions
            (split-string (substring ugly-return 0 (position ?\n ugly-return)) sep))
      (setq completion-table (loop for str in completions
                                   collect (list str nil)))
      (setq completion (try-completion pattern completion-table))
      (cond ((eq completion t)
             (tab-to-tab-stop))
            ((null completion)
             ;; if an (I)Python shell didn't run
             ;; before, first completion are not delivered
             ;; (if done (ipython-complete done)
             (message "Can't find completion for \"%s\"" pattern)
             (ding))
            ((not (string= pattern completion))
             (delete-region beg end)
             (insert completion))
            (t
             (message "Making completion list...")
             (with-output-to-temp-buffer "*Python Completions*"
               (display-completion-list (all-completions pattern completion-table)))
             (message "Making completion list...%s" "done"))))
    ;; minibuffer.el requires that
    (list beg end)))

(provide 'python-components-shell-complete)

;; pyshell-complete.el ends here
