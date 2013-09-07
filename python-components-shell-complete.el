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

;;;

(defalias 'py-script-complete 'py-shell-complete)

(defun py-shell-completion--get-completions (input process completion-code)
  "Retrieve available completions for INPUT using PROCESS.
Argument COMPLETION-CODE is the python code used to get
completions on the current context."
  (with-current-buffer (process-buffer process)
    (let ((completions
           (py-send-string-no-output
            (format completion-code input) process)))
      (when (> (length completions) 2)
        (split-string completions "^'\\|^\"\\|;\\|'$\\|\"$" t)))))

(defun py-shell--do-completion-at-point (process imports input)
  "Do completion at point for PROCESS."
  (with-syntax-table py-dotted-expression-syntax-table
    (when imports (py-send-string-no-output imports process))
    (let* ((code python-shell-module-completion-string-code)
           (completions
            (py-shell-completion--get-completions
             input process code))
           (completion (when completions
                         (try-completion input completions))))
      (cond ((eq completion t)
             (if (eq this-command last-command)
                 (when py-completion-last-window-configuration
                   (set-window-configuration
                    py-completion-last-window-configuration)))
             ;; (setq py-completion-last-window-configuration nil)
             (if py-no-completion-calls-dabbrev-expand-p
                 (or (ignore-errors (dabbrev-expand nil))(when py-indent-no-completion-p
                                                           (tab-to-tab-stop)))
               (when py-indent-no-completion-p
                 (tab-to-tab-stop)))
             nil)
            ((null completion)
             (if py-no-completion-calls-dabbrev-expand-p
                 (or (dabbrev-expand nil)(when py-indent-no-completion-p
                                           (tab-to-tab-stop))(message "Can't find completion "))
               (when py-indent-no-completion-p
                 (tab-to-tab-stop)))
             nil)
            ((not (string= input completion))
             (progn (delete-char (- (length input)))
                    (insert completion)
                    ;; minibuffer.el expects a list, a bug IMO
                    nil))
            (t
             (unless py-completion-last-window-configuration
               (setq py-completion-last-window-configuration
                     (current-window-configuration)))
             (with-output-to-temp-buffer "*Python Completions*"
               (display-completion-list
                (all-completions input completions)))
             nil)))))

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
         (imports (or imports (py-find-imports)))
         proc)
    (cond ((string= word "")
           (if py-indent-no-completion-p
               (tab-to-tab-stop)
             (message "%s" "Nothing to complete. ")))
          (t (or (setq proc (get-buffer-process (py-buffer-name-prepare)))
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
                   (py-shell--do-completion-at-point proc imports word))
               (error "No completion process at proc"))))))

(defun py-python2-shell-complete (&optional shell)
  (interactive)
  (let* (py-split-windows-on-execute-p
         py-switch-buffers-on-execute-p
         (shell (or shell py-local-versioned-command))
         (orig (point))
         (beg (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point)))
         (end (point))
         (word (buffer-substring-no-properties beg end))
         proc)
    (cond ((string= word "")
           (message "%s" "Nothing to complete. ")
           (tab-to-tab-stop))
          (t (or (setq proc (get-buffer-process shell))
                 (setq proc (get-buffer-process (py-shell nil nil shell))))
             (py-shell--do-completion-at-point proc nil word))))
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
           (py-shell--do-completion-at-point (get-buffer-process (current-buffer)) nil word)
           nil))))

(defun py-shell-complete (&optional shell debug)
  "Complete word before point, if any. Otherwise insert TAB. "
  (interactive)
  ;; (window-configuration-to-register 313465889)
  ;; (save-window-excursion
  (when debug (setq py-shell-complete-debug nil))
  (unless (buffer-live-p (get-buffer "*Python Completions*"))
    (setq py-completion-last-window-configuration
          (current-window-configuration)))
  (let* ((windows-config (window-configuration-to-register 313465889))
         (orig (point))
         (beg (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point)))
         (end (point))
         (word (buffer-substring-no-properties beg end))
         ;; used by window-configuration
         val)
    ;; (ignore-errors (comint-dynamic-complete))
    (when (eq (point) orig)
      (if (or (eq major-mode 'comint-mode)(eq major-mode 'inferior-python-mode))
          ;;  kind of completion resp. to shell
          (let (py-fontify-shell-buffer-p
                (shell (or shell (py-report-executable (buffer-name (current-buffer)))))
                (imports (py-find-imports)))
            (if (string-match "[iI][pP]ython" shell)
                (ipython-complete nil nil nil nil nil shell debug imports)
              (let* ((orig (point))

                     (proc (get-buffer-process (current-buffer))))
                (cond ((string= word "")
                       (tab-to-tab-stop))
                      ((string-match "[pP]ython3[^[:alpha:]]*$" shell)
                       (py-shell--do-completion-at-point proc imports word))
                      (t (py-shell-complete-intern word beg end shell imports proc))))))
        ;; complete in script buffer
        (let* (
               ;; (a (random 999999999))
               (shell (or shell (py-choose-shell)))
               py-split-windows-on-execute-p
               py-switch-buffers-on-execute-p
               (proc (or (get-process shell)
                         (get-buffer-process (py-shell nil nil shell nil t))))
               (imports (py-find-imports)))
          ;; (window-configuration-to-register a)
          (cond ((string= word "")
                 (tab-to-tab-stop))
                ((string-match "[iI][pP]ython" shell)
                 (ipython-complete nil nil beg end word nil debug imports))
                ((string-match "[pP]ython3[^[:alpha:]]*$" shell)
                 (py-shell--do-completion-at-point proc (buffer-substring-no-properties beg end) word))
                ;; deals better with imports
                ;; (imports
                ;; (py-python-script-complete shell imports beg end word))
                (t (py-shell-complete-intern word beg end shell imports proc debug)))))
      (and (setq val (get-register 313465889))(and (consp val) (window-configuration-p (car val))(markerp (cadr val)))(marker-buffer (cadr val))
             (jump-to-register 313465889))
      )))

(defun py-shell-complete-intern (word &optional beg end shell imports proc debug)
  (when imports
    (py-send-string-no-output imports proc))
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
                       (when py-completion-last-window-configuration
                         (set-window-configuration
                          py-completion-last-window-configuration)))
                   (setq py-completion-last-window-configuration nil)
                   (when (buffer-live-p (get-buffer "*Python Completions*"))
                     (kill-buffer (get-buffer "*Python Completions*")))
                   (message "Can't find completion for \"%s\"" word)
                   (ding)
                   nil)
                  ((< 1 (length completions))
                   (with-output-to-temp-buffer "*Python Completions*"
                     (display-completion-list
                      (all-completions word completions)
                      word))
                   nil)
                  ((not (string= word (car completions)))
                   (completion-in-region beg end completions)
                   ;; (progn (delete-char (- (length word)))
                   ;; (insert (car completions))
                   (py-restore-window-configuration)
                   (when (buffer-live-p (get-buffer "*Python Completions*"))
                     (kill-buffer (get-buffer "*Python Completions*")))
                   nil))
          (when py-no-completion-calls-dabbrev-expand-p
            (ignore-errors (dabbrev-expand nil)))
          (when py-indent-no-completion-p
            (tab-to-tab-stop)
            (when (buffer-live-p (get-buffer "*Python Completions*"))
              (kill-buffer (get-buffer "*Python Completions*")))))))))

(defalias 'ipyhton-complete 'ipython-complete)
(defun ipython-complete (&optional done completion-command-string beg end word shell debug imports)
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
         (py-shell-name (or shell (py-choose-shell)))
         (processlist (process-list))
         (imports (or imports (py-find-imports)))
         done
         (process
          (if ipython-complete-use-separate-shell-p
              (unless (and (buffer-live-p " *IPython-Complete*")
                           (comint-check-proc (process-name (get-buffer-process " *IPython-Complete*"))))
                (get-buffer-process (py-shell nil nil py-shell-name " *IPython-Complete*")))
            (progn
              (while (and processlist (not done))
                (when (and
                       (string= py-shell-name (process-name (car processlist)))
                       (processp (car processlist))
                       (setq done (car processlist))))
                (setq processlist (cdr processlist)))
              done)))
         (python-process (or process
                             (get-buffer-process (py-shell nil nil (if (string-match "[iI][pP]ython[^[:alpha:]]*$"  py-shell-name) "ipython") nil))))
         (comint-output-filter-functions
          (delq 'py-comint-output-filter-function comint-output-filter-functions))
         (comint-output-filter-functions
          (append comint-output-filter-functions
                  '(ansi-color-filter-apply
                    (lambda (string)
                      (setq ugly-return (concat ugly-return string))
                      (delete-region comint-last-output-start
                                     (process-mark (get-buffer-process (current-buffer))))))))

         (ccs (or completion-command-string
                  (if imports
                      (concat imports (py-set-ipython-completion-command-string))
                    (py-set-ipython-completion-command-string))))
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
                           (when py-completion-last-window-configuration
                             (set-window-configuration
                              py-completion-last-window-configuration)))
                       (setq py-completion-last-window-configuration nil)
                       (message "Can't find completion for \"%s\"" pattern)
                       (ding)
                       nil)
                      ((< 1 (length completions))
                       (unless py-completion-last-window-configuration
                         (setq py-completion-last-window-configuration
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
