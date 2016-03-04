;;; python-components-foot.el --- foot

;; Copyright (C) 2015  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>

;; Keywords: languages

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

(defun py-shell-fontify ()
  "Fontifies input in shell buffer. "
  ;; causes delay in fontification until next trigger
  ;; (unless (or (member (char-before) (list 32 ?: ?\)))
  ;; (unless (and (eq last-command 'self-insert-command) (eq (char-before) 32))
  ;; (< (abs (save-excursion (skip-chars-backward "^ \t\r\n\f"))) 2))
  (let* ((pps (parse-partial-sexp (line-beginning-position) (point)))
	 (start (if (and (nth 8 pps) (nth 1 pps))
		    (max (nth 1 pps) (nth 8 pps))
		  (or (nth 1 pps) (nth 8 pps)))))
    (when (or start
	      (setq start (ignore-errors (cdr comint-last-prompt))))
      (let* ((input (buffer-substring-no-properties
		     start (point-max)))
	     (buffer-undo-list t)
	     (replacement
	      (save-current-buffer
		(set-buffer py-shell--font-lock-buffer)
		(erase-buffer)
		(insert input)
		;; Ensure buffer is fontified, keeping it
		;; compatible with Emacs < 24.4.
		(if (fboundp 'font-lock-ensure)
		    (funcall 'font-lock-ensure)
		  (font-lock-default-fontify-buffer))
		(buffer-substring (point-min) (point-max))))
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
	     (+ start i) (+ start next-change) plist)
	    (setq i next-change)))))))

(define-derived-mode py-auto-completion-mode python-mode "Pac"
  "Run auto-completion"
  ;; disable company
  ;; (when company-mode (company-mode))
  (if py-auto-completion-mode-p
      (progn
	(setq py-auto-completion-mode-p nil
	      py-auto-completion-buffer nil)
	(when (timerp py--auto-complete-timer)(cancel-timer py--auto-complete-timer)))
    (setq py-auto-completion-mode-p t
	  py-auto-completion-buffer (current-buffer))
    (setq py--auto-complete-timer
	  (run-with-idle-timer
	   py--auto-complete-timer-delay
	   ;; 1
	   t
	   #'py-complete-auto)))
  (force-mode-line-update))

;;;
(define-derived-mode python-mode prog-mode python-mode-modeline-display
  "Major mode for editing Python files.

To submit a problem report, enter `\\[py-submit-bug-report]' from a
`python-mode' buffer.  Do `\\[py-describe-mode]' for detailed
documentation.  To see what version of `python-mode' you are running,
enter `\\[py-version]'.

This mode knows about Python indentation, tokens, comments and
continuation lines.  Paragraphs are separated by blank lines only.

COMMANDS

`py-shell'\tStart an interactive Python interpreter in another window
`py-execute-statement'\tSend statement at point to Python default interpreter
`py-backward-statement'\tGo to the initial line of a simple statement

etc.

See available commands listed in files commands-python-mode at directory doc

VARIABLES

`py-indent-offset'	indentation increment
`py-shell-name'		shell command to invoke Python interpreter
`py-split-window-on-execute'		When non-nil split windows
`py-switch-buffers-on-execute-p'	When non-nil switch to the Python output buffer

See available customizations listed in files variables-python-mode at directory doc

\\{python-mode-map}"
  :group 'python-mode
  ;; Local vars
  (set (make-local-variable 'electric-indent-inhibit) nil)
  (set (make-local-variable 'outline-regexp)
       (concat (mapconcat 'identity
                          (mapcar #'(lambda (x) (concat "^\\s-*" x "\\_>"))
                                  py-outline-mode-keywords)
                          "\\|")))
  (when (eq 0 (string-match "25" emacs-version))
    (global-eldoc-mode -1))
  (if py-use-font-lock-doc-face-p
      (set (make-local-variable 'font-lock-defaults)
           '(python-font-lock-keywords nil nil nil nil
				       (font-lock-syntactic-keywords
					. py-font-lock-syntactic-keywords)
				       (font-lock-syntactic-face-function
					. py--font-lock-syntactic-face-function)))
    (set (make-local-variable 'font-lock-defaults)
         '(python-font-lock-keywords nil nil nil nil
				     (font-lock-syntactic-keywords
				      . py-font-lock-syntactic-keywords))))
  ;; avoid to run py-choose-shell again from `py--fix-start'
  (cond ((string-match "ython3" py-python-edit-version)
	 (font-lock-add-keywords 'python-mode
				 '(("\\<print\\>" . 'py-builtins-face)
				   ("\\<file\\>" . nil))))
	(t (font-lock-add-keywords 'python-mode
				   '(("\\<print\\>" . 'font-lock-keyword-face)
				     ("\\<file\\>" . 'py-builtins-face)))))
  (set (make-local-variable 'which-func-functions) 'py-which-def-or-class)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start) "#[^*]")
  (set (make-local-variable 'comment-start-skip) "^[ \t]*#+[^*] *")

  (if py-empty-comment-line-separates-paragraph-p
      (progn
        (set (make-local-variable 'paragraph-separate) "\f\\|^[ \t]*$\\|^[ \t]*#[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$")
        (set (make-local-variable 'paragraph-start) "\f\\|^[ \t]*$\\|^[ \t]*#[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$"))
    (set (make-local-variable 'paragraph-separate) "\f\\|^[ \t]*$\\|^[ \t]*#[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$")
    (set (make-local-variable 'paragraph-start) "\f\\|^[ \t]*$\\|^[ \t]*#[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$"))
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-indent-function) #'py--comment-indent-function)
  (set (make-local-variable 'indent-region-function) 'py-indent-region)
  (set (make-local-variable 'indent-line-function) 'py-indent-line)
  (set (make-local-variable 'hs-hide-comments-when-hiding-all) 'py-hide-comments-when-hiding-all)
  (set (make-local-variable 'outline-heading-end-regexp) ":[^\n]*\n")
  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)
  (set (make-local-variable 'add-log-current-defun-function) 'py-current-defun)
  (set (make-local-variable 'fill-paragraph-function) 'py-fill-paragraph)
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  (set (make-local-variable 'tab-width) py-indent-offset)
  (set (make-local-variable 'eldoc-documentation-function)
       #'py-eldoc-function)
  (and py-load-skeletons-p
       (py-load-skeletons)
       (set (make-local-variable 'skeleton-further-elements)
            '((< '(backward-delete-char-untabify (min py-indent-offset
                                                      (current-column))))
              (^ '(- (1+ (current-indentation)))))))
  (and py-guess-py-install-directory-p (py-set-load-path))
  ;;  (unless gud-pdb-history (when (buffer-file-name) (add-to-list 'gud-pdb-history (buffer-file-name))))
  (and py-autopair-mode
       (load-library "autopair")
       (add-hook 'python-mode-hook
                 #'(lambda ()
                     (setq autopair-handle-action-fns
                           (list #'autopair-default-handle-action
                                 #'autopair-python-triple-quote-action))))
       (py-autopair-mode-on))
  (when py-trailing-whitespace-smart-delete-p
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local))
  (when py-pdbtrack-do-tracking-p
    (add-hook 'comint-output-filter-functions 'py--pdbtrack-track-stack-file t))
  (cond
   (py-complete-function
    (add-hook 'completion-at-point-functions
              py-complete-function nil 'local))
   (py-load-pymacs-p
    (add-hook 'completion-at-point-functions
              'py-complete-completion-at-point nil 'local))
   (t
    (add-hook 'completion-at-point-functions
              'py-shell-complete nil 'local)))
  ;; (if py-auto-complete-p
  ;; (add-hook 'python-mode-hook 'py--run-completion-timer)
  ;; (remove-hook 'python-mode-hook 'py--run-completion-timer))
  ;; (when py-auto-complete-p
  ;; (add-hook 'python-mode-hook
  ;; (lambda ()
  ;; (run-with-idle-timer 1 t 'py-shell-complete))))
  (if py-auto-fill-mode
      (add-hook 'python-mode-hook 'py--run-auto-fill-timer)
    (remove-hook 'python-mode-hook 'py--run-auto-fill-timer))

  ;; caused insert-file-contents error lp:1293172
  ;;  (add-hook 'after-change-functions 'py--after-change-function nil t)
  (if py-defun-use-top-level-p
      (progn
        (set (make-local-variable 'beginning-of-defun-function) 'py-backward-top-level)
        (set (make-local-variable 'end-of-defun-function) 'py-end-of-top-level)
        (define-key python-mode-map [(control meta a)] 'py-backward-top-level)
        (define-key python-mode-map [(control meta e)] 'py-end-of-top-level))
    (set (make-local-variable 'beginning-of-defun-function) 'py-backward-def-or-class)
    (set (make-local-variable 'end-of-defun-function) 'py-end-of-def-or-class)
    (define-key python-mode-map [(control meta a)] 'py-backward-def-or-class)
    (define-key python-mode-map [(control meta e)] 'py-end-of-def-or-class))
  (when py-sexp-use-expression-p
    	(define-key python-mode-map [(control meta f)] 'py-forward-expression)
	(define-key python-mode-map [(control meta b)] 'py-backward-expression))
  (when (and py--imenu-create-index-p
             (fboundp 'imenu-add-to-menubar)
             (ignore-errors (require 'imenu)))
  (setq imenu-create-index-function 'py--imenu-create-index-function)
  (setq imenu--index-alist (funcall py--imenu-create-index-function))
  ;; fallback
  (unless imenu--index-alist
    (setq imenu--index-alist (py--imenu-create-index-new)))
    ;; (message "imenu--index-alist: %s" imenu--index-alist)
    (imenu-add-to-menubar "PyIndex"))
  ;; add the menu
  (when py-menu
    (easy-menu-add py-menu))
  (when py-hide-show-minor-mode-p (hs-minor-mode 1))
  (when py-outline-minor-mode-p (outline-minor-mode 1))
  (when (called-interactively-p 'any) (message "python-mode loaded from: %s" python-mode-message-string))
  (force-mode-line-update))

(defun py--all-shell-mode-setting ()
  (when py-fontify-shell-buffer-p
    (save-current-buffer
      ;; Prepare the buffer where the input is fontified
      (set-buffer (get-buffer-create py-shell--font-lock-buffer))
      (font-lock-mode 1)
      (python-mode))
    (set (make-local-variable 'delay-mode-hooks) t)
    ;; post-self-insert-hook
    (add-hook 'post-command-hook
	      #'py-shell-fontify nil 'local))
  (setenv "PAGER" "cat")
  (setenv "TERM" "dumb")
  (set-syntax-table python-mode-syntax-table)
  (if py-auto-complete-p
      (add-hook 'py-shell-mode-hook 'py--run-completion-timer)
    (remove-hook 'py-shell-mode-hook 'py--run-completion-timer))
  ;; comint settings
  (set (make-local-variable 'comint-prompt-regexp)
       (cond ((string-match "[iI][pP]ython[[:alnum:]*-]*$" py-buffer-name)
	      (concat "\\("
		      (mapconcat 'identity
				 (delq nil (list py-shell-input-prompt-1-regexp py-shell-input-prompt-2-regexp py-ipython-input-prompt-re py-ipython-output-prompt-re py-pdbtrack-input-prompt py-pydbtrack-input-prompt))
				 "\\|")
		      "\\)"))
	     (t (concat "\\("
			(mapconcat 'identity
				   (delq nil (list py-shell-input-prompt-1-regexp py-shell-input-prompt-2-regexp py-pdbtrack-input-prompt py-pydbtrack-input-prompt))
				   "\\|")
			"\\)"))))
  (remove-hook 'comint-output-filter-functions 'font-lock-extend-jit-lock-region-after-change t)

  (make-local-variable 'comint-output-filter-functions)
  ;; (set (make-local-variable 'comint-input-filter) 'py--input-filter)
  (set (make-local-variable 'comint-input-filter) 'py-history-input-filter)
  (set (make-local-variable 'comint-prompt-read-only) py-shell-prompt-read-only)
  ;; (set (make-local-variable 'comint-use-prompt-regexp) nil)
  (set (make-local-variable 'compilation-error-regexp-alist)
       py-compilation-regexp-alist)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "^[ \t]*#+ *")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-indent-function) #'py--comment-indent-function)
  (set (make-local-variable 'indent-region-function) 'py-indent-region)
  (set (make-local-variable 'indent-line-function) 'py-indent-line)
  (set (make-local-variable 'inhibit-point-motion-hooks) t)
  (set (make-local-variable 'comint-input-sender) 'py--shell-simple-send))

(define-derived-mode py-python-shell-mode comint-mode "Py"
  "Major mode for interacting with a Python process.
A Python process can be started with \\[py-shell].

You can send text to the Python process from other buffers
containing Python source.
 * \\[py-execute-region] sends the current region to the Python process.

Sets basic comint variables, see also versions-related stuff in `py-shell'.
\\{py-python-shell-mode-map}"
  :group 'python-mode
  ;; (require 'ansi-color) ; for ipython
  (setq mode-line-process '(":%s"))
  ;; (sit-for 0.1)
  (when py-verbose-p (message "%s" "Initializing Python shell, please wait"))
  (py--all-shell-mode-setting)
  (py--python-send-completion-setup-code)
  (py--python-send-ffap-setup-code)
  (py--python-send-eldoc-setup-code)
  (set-process-sentinel (get-buffer-process (current-buffer))  #'shell-write-history-on-exit)

  ;; (setq comint-input-ring-file-name
  ;;       (cond ((string-match "[iI][pP]ython[[:alnum:]*-]*$" py-buffer-name)
  ;;              (if py-honor-IPYTHONDIR-p
  ;;                  (if (getenv "IPYTHONDIR")
  ;;                      (concat (getenv "IPYTHONDIR") "/history")
  ;;                    py-ipython-history)
  ;;                py-ipython-history))
  ;;             (t
  ;;              (if py-honor-PYTHONHISTORY-p
  ;;                  (if (getenv "PYTHONHISTORY")
  ;;                      (concat (getenv "PYTHONHISTORY") "/" (py--report-executable py-buffer-name) "_history")
  ;;                    py-ipython-history)
  ;;                py-ipython-history)))
  ;;)
  (comint-read-input-ring t)
  (compilation-shell-minor-mode 1)
  ;;
  (if py-complete-function
      (progn
  	(add-hook 'completion-at-point-functions
  		  py-complete-function nil 'local)
  	(add-to-list (make-local-variable 'comint-dynamic-complete-functions)
  		     py-complete-function))
    (add-hook 'completion-at-point-functions
              'py-shell-complete nil 'local)
    (add-to-list (make-local-variable 'comint-dynamic-complete-functions)
  		 'py-shell-complete))
  (when py-sexp-use-expression-p
    (define-key py-python-shell-mode-map [(control meta f)] 'py-forward-expression)
    (define-key py-python-shell-mode-map [(control meta b)] 'py-backward-expression))
  (when py-shell-menu
    (easy-menu-add py-menu))
  (force-mode-line-update))

(define-derived-mode py-ipython-shell-mode comint-mode "IPy"
  "Major mode for interacting with a Python process.
A Python process can be started with \\[py-shell].

You can send text to the Python process from other buffers
containing Python source.
 * \\[py-execute-region] sends the current region to the Python process.

Sets basic comint variables, see also versions-related stuff in `py-shell'.
\\{py-ipython-shell-mode-map}"
  :group 'python-mode
  ;; (require 'ansi-color) ; for ipython
  (setq mode-line-process '(":%s"))
  (when py-verbose-p (message "%s" "Initializing IPython shell, please wait"))
  (py--all-shell-mode-setting)
  (py--python-send-completion-setup-code)
  (py--python-send-ffap-setup-code)
  (py--python-send-eldoc-setup-code)
  (py--ipython-import-module-completion)
  (py-set-ipython-completion-command-string (process-name (get-buffer-process (current-buffer))))
  (sit-for 0.1 t)
  (comint-read-input-ring t)
  (compilation-shell-minor-mode 1)
  (if py-complete-function
      (progn
  	(add-hook 'completion-at-point-functions
  		  py-complete-function nil 'local)
  	(add-to-list (make-local-variable 'comint-dynamic-complete-functions)
  		     py-complete-function))
    (add-hook 'completion-at-point-functions
              'py-shell-complete nil 'local)
    (add-to-list (make-local-variable 'comint-dynamic-complete-functions)
  		 'py-shell-complete))
  (when py-shell-menu
    (easy-menu-add py-menu))
  ;; Running py-ipython-shell-mode-hook seems to need some delay
  (sit-for 0.5 t)
  (force-mode-line-update))

(defalias 'py-backward-decorator-bol 'py-backward-decorator)
(defalias 'py-beginning-of-block 'py-backward-block)
(defalias 'py-beginning-of-block-bol 'py-backward-block-bol)
(defalias 'py-beginning-of-block-or-clause 'py-backward-block-or-clause)
(defalias 'py-beginning-of-block-or-clause 'py-goto-block-or-clause-up)
(defalias 'py-beginning-of-block-or-clause 'py-previous-block-or-clause)
(defalias 'py-beginning-of-class 'py-backward-class)
(defalias 'py-beginning-of-class-bol 'py-backward-class-bol)
(defalias 'py-beginning-of-clause 'py-backward-clause)
(defalias 'py-beginning-of-clause 'py-goto-clause-up)
(defalias 'py-beginning-of-clause 'py-previous-clause)
(defalias 'py-beginning-of-clause-bol 'py-backward-clause-bol)
(defalias 'py-beginning-of-comment 'py-backward-comment)
(defalias 'py-beginning-of-declarations 'py-backward-declarations)
(defalias 'py-beginning-of-decorator 'py-backward-decorator)
(defalias 'py-beginning-of-decorator-bol 'py-backward-decorator)
(defalias 'py-beginning-of-def-or-class 'py-backward-def-or-class)
(defalias 'py-beginning-of-expression 'py-backward-expression)
(defalias 'py-beginning-of-line 'py-backward-line)
(defalias 'py-beginning-of-minor-block 'py-backward-minor-block)
(defalias 'py-beginning-of-partial-expression 'py-backward-partial-expression)
(defalias 'py-beginning-of-section 'py-backward-section)
(defalias 'py-beginning-of-statement 'py-backward-statement)
(defalias 'py-beginning-of-statement-bol 'py-backward-statement-bol)
(defalias 'py-beginning-of-top-level 'py-backward-top-level)
(defalias 'py-end-of-block 'py-forward-block)
(defalias 'py-end-of-block-or-clause 'py-forward-block-or-clause)
(defalias 'py-end-of-class 'py-forward-class)
(defalias 'py-end-of-clause 'py-forward-clause)
(defalias 'py-end-of-comment 'py-forward-comment)
(defalias 'py-end-of-decorator 'py-forward-decorator)
(defalias 'py-end-of-def-or-class 'py-forward-def-or-class)
(defalias 'py-end-of-expression 'py-forward-expression)
(defalias 'py-end-of-line 'py-forward-line)
(defalias 'py-end-of-partial-expression 'py-forward-partial-expression)
(defalias 'py-end-of-section 'py-forward-section)
(defalias 'py-end-of-statement 'py-forward-statement)
(defalias 'py-end-of-statement-bol 'py-forward-statement-bol)
(defalias 'py-end-of-top-level 'py-forward-top-level)
(defalias 'py-goto-block-or-clause-up 'py-backward-block-or-clause)
(defalias 'py-goto-block-up 'py-backward-block)
(defalias 'py-goto-clause-up 'py-backward-clause)
(defalias 'py-next-statement 'py-forward-statement)
(defalias 'py-previous-block-or-clause 'py-backward-block-or-clause)
(defalias 'py-previous-class 'py-backward-class)
(defalias 'py-previous-clause 'py-backward-clause)
(defalias 'py-previous-def-or-class 'py-backward-def-or-class)
(defalias 'py-previous-statement 'py-backward-statement)
(defalias 'py-markup-region-as-section 'py-sectionize-region)

;;;
(provide 'python-components-foot)
;;; python-components-foot.el ends here
