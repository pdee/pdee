;;; python-components-completion.el -- Add suport for completion in python-mode

;;; Code
(require 'comint)
(require 'python-components-macros)

(defvar py-shell-input-lines nil
  "Collect input lines send interactively to the Python process in
order to allow injecting completion command between keyboard interrupt
and resending the lines later. The lines are stored in reverse order")

;;; need to clear py-shell-input-lines if primary prompt found

(defun py-shell-simple-send (proc string)
  (setq py-shell-input-lines (cons string py-shell-input-lines))
  (comint-simple-send proc string))

(defalias
  'py-shell-redirect-send-command-to-process
  'comint-redirect-send-command-to-process)
(defalias
  'py-shell-dynamic-simple-complete
  'comint-dynamic-simple-complete)

;; (defun py-shell-execute-string-now (string &optional shell)
;;   "Send to Python interpreter process PROC \"exec STRING in {}\".
;; and return collected output"
;;   (let* ((proc (cond (shell
;;                       (or (get-process shell)
;;                           (prog1
;;                               (get-buffer-process (py-shell nil nil shell))
;;                             (sit-for 0.1)
;;                             )))
;;                      (t (or (get-buffer-process (current-buffer))
;;                             (get-buffer-process (py-shell))))))
;; 	 (cmd (format "exec '''%s''' in {}"
;; 		      (mapconcat 'identity (split-string string "\n") "\\n")))
;;          (procbuf (process-buffer proc))
;;          x         (outbuf (get-buffer-create " *pyshellcomplete-output*"))
;;          (lines (reverse py-shell-input-lines)))
;;     (if (and proc (not py-file-queue))
;;         (unwind-protect
;;             (condition-case nil
;;                 (progn
;;                   (if lines
;;                       (with-current-buffer procbuf
;;                         (comint-redirect-send-command-to-process
;;                          "\C-c" outbuf proc nil t)
;;                         ;; wait for output
;;                         (while (not comint-redirect-completed)
;;                           (accept-process-output proc 1))))
;;                   (with-current-buffer outbuf
;;                     (delete-region (point-min) (point-max)))
;;                   (with-current-buffer procbuf
;;                     (comint-redirect-send-command-to-process
;;                      cmd outbuf proc nil t)
;;                     (while (not comint-redirect-completed) ; wait for output
;;                       (accept-process-output proc 1)))
;;                   (with-current-buffer outbuf
;;                     (buffer-substring (point-min) (point-max))))
;;               (quit (with-current-buffer procbuf
;;                       (interrupt-process proc comint-ptyp)
;;                       (while (not comint-redirect-completed) ; wait for output
;;                         (accept-process-output proc 1)))
;;                     (signal 'quit nil)))
;;           (if (with-current-buffer procbuf comint-redirect-completed)
;;               (while lines
;;                 (with-current-buffer procbuf
;;                   (comint-redirect-send-command-to-process
;;                    (car lines) outbuf proc nil t))
;;                 (accept-process-output proc 1)
;;                 (setq lines (cdr lines))))))))
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

;; (defun py-find-imports ()
;;   "Find top-level imports, updating `python-imports'."
;;   (interactive)
;;   (save-excursion
;;     (let (lines)
;;       (goto-char (point-min))
;;       (while (re-search-forward "^import\\_>[ \n\t]\\|^from\\_>[ \n\t]" nil t)
;;         (unless (syntax-ppss-context (syntax-ppss))
;;           (let ((start (line-beginning-position)))
;;             ;; Skip over continued lines.
;;             (while (and (eq ?\\ (char-before (line-end-position)))
;;                         (= 0 (forward-line 1)))
;;               t)
;;             (push (buffer-substring start (line-beginning-position 2))
;;                   lines))))
;;       (setq python-imports
;;             (if lines
;;                 (apply #'concat
;;                        ;; This is probably best left out since you're unlikely to need the
;;                        ;; doc for a function in the buffer and the import will lose if the
;;                        ;; Python sub-process' working directory isn't the same as the
;;                        ;; buffer's.
;;                        ;; 			 (if buffer-file-name
;;                        ;; 			     (concat
;;                        ;; 			      "import "
;;                        ;; 			      (file-name-sans-extension
;;                        ;; 			       (file-name-nondirectory buffer-file-name))))
;;                        (nreverse lines))
;;               "None"))
;;       (when lines
;;         (set-text-properties 0 (length python-imports) nil python-imports)
;;         ;; The output ends up in the wrong place if the string we
;;         ;; send contains newlines (from the imports).
;;         (setq python-imports
;;               (replace-regexp-in-string "\n" "\\n"
;;                                         (format "%S" python-imports)) t t)))))

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
         (py-process (or (get-buffer-process (current-buffer))
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
      (process-send-string py-process
                           (format (py-set-ipython-completion-command-string (downcase (process-name py-process))) pattern))
      (accept-process-output py-process)
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

(defun ipython-complete (&optional done)
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
              (unless (and (buffer-live-p "*IPython-Complete*")
                           (comint-check-proc (process-name (get-buffer-process "*IPython-Complete*"))))
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
             (ding)
             nil)
            ((not (string= pattern completion))
             (delete-region beg end)
             (insert completion)
             nil)
            (t
             (when py-verbose-p (message "Making completion list..."))
             (with-output-to-temp-buffer "*Python Completions*"
               (display-completion-list (all-completions pattern completion-table)))
             nil
             ;; (message "Making completion list...%s" "done")
)))
    ;; minibuffer.el requires that
    ;; (list beg end)
))

;;; Flymake
(defun py-toggle-flymake-intern (name command)
  ;; (clear-flymake-allowed-file-name-masks)
  (unless (string-match "pyflakespep8" name)
    (unless (executable-find name)
      (when py-verbose-p (message "Don't see %s. Use `easy_install' %s? " name name))))
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (add-to-list 'flymake-allowed-file-name-masks (car (read-from-string (concat "(\"\\.py\\'\" flymake-" name ")"))))
    (list command (list local-file))))

(defun pylint-flymake-mode ()
  "Toggle `pylint' `flymake-mode'. "
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode)
    (py-toggle-flymake-intern "pylint" "pylint")
    (flymake-mode)))

(defun pyflakes-flymake-mode ()
  "Toggle `pyflakes' `flymake-mode'. "
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode)
    (py-toggle-flymake-intern "pyflakes" "pyflakes")
    (flymake-mode)))

(defun pychecker-flymake-mode ()
  "Toggle `pychecker' `flymake-mode'. "
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode)
    (py-toggle-flymake-intern "pychecker" "pychecker")
    (flymake-mode)))

(defun pep8-flymake-mode ()
  "Toggle `pep8' `flymake-mode'. "
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode)
    (py-toggle-flymake-intern "pep8" "pep8")
    (flymake-mode)))

(defun pyflakespep8-flymake-mode ()
  "Toggle `pyflakespep8' `flymake-mode'.

Joint call to pyflakes and pep8 as proposed by

Keegan Carruthers-Smith

"
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode)
    (py-toggle-flymake-intern "pyflakespep8" "pyflakespep8")
    (flymake-mode)))

(provide 'python-components-completion)

;; python-components-completion.el ends here
