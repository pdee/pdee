;;; python-components-shell-complete.el -- Add suport for completion in py-shell

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

;;; Code

(defun py-shell-execute-string-now (string &optional shell buffer proc)
  "Send to Python interpreter process PROC \"exec STRING in {}\".
and return collected output"
  (let* ((procbuf (or buffer (process-buffer proc) (py-shell nil nil shell)))

         (proc (or proc (get-buffer-process procbuf)))
	 (cmd (format "exec '''%s''' in {}"
		      (mapconcat 'identity (split-string string "\n") "\\n")))
         (outbuf (get-buffer-create " *pyshellcomplete-output*"))
         ;; (lines (reverse py-shell-input-lines))
         )
    ;; (when proc
    (unwind-protect
        (condition-case nil
            (progn
              ;; (if lines
              ;;     (with-current-buffer procbuf
              ;;       (comint-redirect-send-command-to-process
              ;;        "\C-c" outbuf proc nil t)
              ;;       ;; wait for output
              ;;       (while (not comint-redirect-completed)
              ;;         (accept-process-output proc 1))))
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
      ;; (if (with-current-buffer procbuf comint-redirect-completed)
      ;;     (while lines
      ;;       (with-current-buffer procbuf
      ;;         (comint-redirect-send-command-to-process
      ;;          (car lines) outbuf proc nil t))
      ;;       (accept-process-output proc 1)
      ;;       (setq lines (cdr lines))))
      )))

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

;; Author: Lukasz Pankowski, patch sent for lp:328836
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

;; (defun py-completion-at-point ()
;;   "An alternative completion, similar the way python.el does it. "
;;   (interactive "*")
;;   (python-find-imports)
;;   (let* ((start (when (skip-chars-backward "[[:alnum:]_]")(point)))
;;          (end (progn (skip-chars-forward "[[:alnum:]_]")(point)))
;;          (completion (when start
;;                        (py-symbol-completions (buffer-substring-no-properties start end)))))
;;     (if completion
;;         (progn
;;           (delete-region start end)
;;           (insert (car completion)))
;;       (tab-to-tab-stop))))

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
		     (py-send-receive
		      (format "emacs.complete(%S,%s)"
			      (substring-no-properties symbol)
			      python-imports))))
	     (error nil))))
      (sort
       ;; We can get duplicates from the above -- don't know why.
       (delete-dups completions)
       #'string<))))

(defun py-python-script-complete (&optional shell imports beg end word)
  "Complete word before point, if any.

When `py-no-completion-calls-dabbrev-expand-p' is non-nil, try dabbrev-expand. Otherwise, when `py-indent-no-completion-p' is non-nil, call `tab-to-tab-stop'. "
  (interactive)
  (let* (py-split-windows-on-execute-p
         py-switch-buffers-on-execute-p
         (orig (point))
         (shell (or shell py-local-versioned-command (py-choose-shell)))
         (beg (or beg (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point))))
         (end (or end (point)))
         (word (or word (buffer-substring-no-properties beg end)))
         (imports (or imports (py-find-imports))))
    (cond ((string= word "")
           (if py-indent-no-completion-p
               (tab-to-tab-stop)
             (message "%s" "Nothing to complete. ")))
          (t (or (setq proc (get-buffer-process (py-buffer-name-prepare shell)))
                 (setq proc (get-buffer-process (py-shell nil nil shell))))
             (if (processp proc)
                 (progn
                   ;; when completing instances, make them known
                   (when (string-match "^\\(^[a-zA-Z0-9_]+\\)\\.\\([a-zA-Z0-9_]+\\)$" word)
                     ;; (message "%s" (match-string 1 word))
                     (save-excursion
                       (save-match-data
                         (goto-char (point-min))
                         (when (re-search-forward (concat "^[ \t]*" (match-string-no-properties 1 word) "[ \t]*=[ \t]*[^ \n\r\f\t]+") nil t 1)
                           (when py-verbose-p (message "%s" (match-string-no-properties 0)))
                           (if imports
                               (setq imports (concat imports (match-string-no-properties 0) ";"))
                             (setq imports (match-string-no-properties 0)))))))
                   (python-shell-completion--do-completion-at-point proc imports word))
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
             (python-shell-completion--do-completion-at-point proc nil word))))
  nil)

(defun py-python3-shell-complete (&optional shell)
  "Complete word before point, if any. Otherwise insert TAB. "
  (interactive)
  (let* ((shell (or shell py-local-versioned-command))
         (orig (point))
         (beg (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point)))
         (end (point))
         (word (buffer-substring-no-properties beg end)))
    (cond ((string= word "")
           (tab-to-tab-stop))
          (t
           (python-shell-completion--do-completion-at-point (get-buffer-process (current-buffer)) nil word)
           nil))))

(defun py-shell-complete (&optional shell debug)
  "Complete word before point, if any. Otherwise insert TAB. "
  (interactive)
  ;; (window-configuration-to-register 313465889)
  ;; (save-window-excursion
  (when debug (setq py-shell-complete-debug nil))
  (if (or (eq major-mode 'comint-mode)(eq major-mode 'inferior-python-mode))
      ;;  kind of completion resp. to shell
      (let (py-fontify-shell-buffer-p
            (shell (or shell (py-report-executable (buffer-name (current-buffer)))))
            (imports (py-find-imports)))
        (if (string-match "[iI][pP]ython" shell)
            (ipython-complete nil nil nil nil nil shell debug)
          (let* ((orig (point))
                 (beg (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point)))
                 (end (point))
                 (word (buffer-substring-no-properties beg end))
                 (proc (get-buffer-process (current-buffer))))
            (cond ((string= word "")
                   (tab-to-tab-stop))
                  ((string-match "[pP]ython3[^[:alpha:]]*$" shell)
                   (python-shell-completion--do-completion-at-point proc imports word))
                  (t (py-shell-complete-intern word beg end shell imports proc))))))
    ;; complete in script buffer
    (let* (
           ;; (a (random 999999999))
           (shell (or shell (py-choose-shell)))
           py-split-windows-on-execute-p
           py-switch-buffers-on-execute-p
           (proc (or (get-process shell)
                     (get-buffer-process (py-shell nil nil shell 'noswitch nil))))
           (beg (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point)))
           (end (point))
           (word (buffer-substring-no-properties beg end))
           (imports (py-find-imports)))
      ;; (window-configuration-to-register a)
      (cond ((string= word "")
             (tab-to-tab-stop))
            ((string-match "[iI][pP]ython" shell)
             (ipython-complete nil nil beg end word))
            ((string-match "[pP]ython3[^[:alpha:]]*$" shell)
             (python-shell-completion--do-completion-at-point proc (buffer-substring-no-properties beg end) word))
            ;; deals better with imports
            ;; (imports
            ;; (py-python-script-complete shell imports beg end word))
            (t (py-shell-complete-intern word beg end shell imports proc debug))))))

(defun py-shell-complete-intern (word &optional beg end shell imports proc debug)
  (when imports
    (python-shell-send-string-no-output imports proc))
  (let ((result (py-shell-execute-string-now (format "
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
complete('%s')" word) shell nil proc)))
    (if (or (eq result nil)(string= "" result))
        (progn
          (if py-no-completion-calls-dabbrev-expand-p
              (or (ignore-errors (dabbrev-expand nil)) (message "Can't complete"))
            (message "No completion found")))

      (setq result (replace-regexp-in-string comint-prompt-regexp "" result))
      (let ((comint-completion-addsuffix nil)
            (completions
             (sort
              (delete-dups (if (split-string "\n" "\n")
                               (split-string result "\n" t) ; XEmacs
                             (split-string result "\n")))
              #'string<)))
        (when debug (setq py-shell-complete-debug completions))
        (if (and completions (not (string= "" (car completions))))
            (cond ((eq completions t)
                   (if (eq this-command last-command)
                       (when python-completion-original-window-configuration
                         (set-window-configuration
                          python-completion-original-window-configuration)))
                   (setq python-completion-original-window-configuration nil)
                   (message "Can't find completion for \"%s\"" word)
                   (ding)
                   nil)
                  ((< 1 (length completions))
                   (unless python-completion-original-window-configuration
                     (setq python-completion-original-window-configuration
                           (current-window-configuration)))
                   (with-output-to-temp-buffer "*Python Completions*"
                     (display-completion-list
                      (all-completions word completions)))
                   (recenter)
                   nil)
                  ((not (string= word (car completions)))
                   (progn (delete-char (- (length word)))
                          (insert (car completions))
                          nil)))
          (when py-no-completion-calls-dabbrev-expand-p
            (ignore-errors (dabbrev-expand nil)))
          (when py-indent-no-completion-p
            (tab-to-tab-stop)))))))

(defalias 'ipyhton-complete 'ipython-complete)
(defun ipython-complete (&optional done completion-command-string beg end word shell debug)
  "Complete the python symbol before point.

If no completion available, insert a TAB.
Returns the completed symbol, a string, if successful, nil otherwise. "

  (interactive "*")
  (let* (
         py-split-windows-on-execute-p
         py-switch-buffers-on-execute-p
         (beg (or beg (progn (save-excursion (skip-chars-backward "a-z0-9A-Z_." (point-at-bol))
                                             (point)))))
         (end (or end (point)))
         (pattern (or word (buffer-substring-no-properties beg end)))
         (sep ";")
         (shell (or shell (py-choose-shell)))
         (pyshellname (if (string-match "ipython" shell)
                          shell
                        "ipython"))
         (processlist (process-list))
         done
         (process
          (if ipython-complete-use-separate-shell-p
              (unless (and (buffer-live-p " *IPython-Complete*")
                           (comint-check-proc (process-name (get-buffer-process " *IPython-Complete*"))))
                (get-buffer-process (py-shell nil nil pyshellname 'noswitch nil " *IPython-Complete*")))
            (progn
              (while (and processlist (not done))
                (when (and
                       (string= pyshellname (process-name (car processlist)))
                       (processp (car processlist))
                       (setq done (car processlist))))
                (setq processlist (cdr processlist)))
              done)))
         (python-process (or process
                             (get-buffer-process (py-shell nil nil (if (string-match "[iI][pP]ython[^[:alpha:]]*$"  pyshellname) pyshellname "ipython") 'noswitch nil))))
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
      (accept-process-output python-process 0.1)
      (if ugly-return
          (progn
            (setq completions
                  (split-string (substring ugly-return 0 (position ?\n ugly-return)) sep))
            (when debug (setq py-shell-complete-debug completions))
            (if (and completions (not (string= "" (car completions))))
                (cond ((eq completions t)
                       (if (eq this-command last-command)
                           (when python-completion-original-window-configuration
                             (set-window-configuration
                              python-completion-original-window-configuration)))
                       (setq python-completion-original-window-configuration nil)
                       (message "Can't find completion for \"%s\"" pattern)
                       (ding)
                       nil)
                      ((< 1 (length completions))
                       (unless python-completion-original-window-configuration
                         (setq python-completion-original-window-configuration
                               (current-window-configuration)))
                       (with-output-to-temp-buffer "*IPython Completions*"
                         (display-completion-list
                          (all-completions pattern completions)))
                       (recenter))
                      ((not (string= pattern (car completions)))
                       (progn (delete-char (- (length pattern)))
                              (insert (car completions))
                              nil)))
              (when py-no-completion-calls-dabbrev-expand-p
                (ignore-errors (dabbrev-expand nil)))
              (when py-indent-no-completion-p
                (tab-to-tab-stop))))
        (message "%s" "No response from Python process. Please check your configuration. If config is okay, please file a bug-regport at http://launchpad.net/python-mode")))))

(provide 'python-components-shell-complete)

;; pyshell-complete.el ends here
