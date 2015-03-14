;;; python-components-foot.el --- foot            

;; Copyright (C) 2015  Andreas Roehler

;; Author: Andreas Roehler <andreas.roehler@online.de>

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
	   #'py-complete-auto))))

;; (add-hook 'after-change-major-mode-hook #'py-protect-other-buffers-ac)

;; after-change-major-mode-hook

;;;
(define-derived-mode python-mode fundamental-mode python-mode-modeline-display
  "Major mode for editing Python files.

To submit a problem report, enter `\\[py-submit-bug-report]' from a
`python-mode' buffer.  Do `\\[py-describe-mode]' for detailed
documentation.  To see what version of `python-mode' you are running,
enter `\\[py-version]'.

This mode knows about Python indentation, tokens, comments and
continuation lines.  Paragraphs are separated by blank lines only.

COMMANDS

`py-shell'\tStart an interactive Python interpreter in another window
`py-execute-statement'\tSend statement at point to a Python interpreter
`py-beginning-of-statement'\tGo to the initial line of a simple statement

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
  (cond ((and (boundp 'py-buffer-name) py-buffer-name)
	 (if (string-match "python3" py-buffer-name)
	     (font-lock-add-keywords 'python-mode
				     '(("\\<print\\>" . 'py-builtins-face)))
	   '(("\\<print\\>" . 'font-lock-keyword-face))))
	((string-match "python3" (py-choose-shell))
	 (font-lock-add-keywords 'python-mode
				 '(("\\<print\\>" . 'py-builtins-face))))
	(t (font-lock-add-keywords 'python-mode
				   '(("\\<print\\>" . 'font-lock-keyword-face)))))

  (set (make-local-variable 'which-func-functions) 'py-which-def-or-class)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start) "#")
  (if py-empty-comment-line-separates-paragraph-p
      (progn
        (set (make-local-variable 'paragraph-separate) "\f\\|^[ \t]*$\\|^[ \t]*#[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$")
        (set (make-local-variable 'paragraph-start) "\f\\|^[ \t]*$\\|^[ \t]*#[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$"))
    (set (make-local-variable 'paragraph-separate) "\f\\|^[ \t]*$\\|^[ \t]*#[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$")
    (set (make-local-variable 'paragraph-start) "\f\\|^[ \t]*$\\|^[ \t]*#[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$"))
  (set (make-local-variable 'comment-start-skip) "^[ \t]*#+ *")
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
  ;; (set (make-local-variable 'imenu-create-index-function) 'py--imenu-create-index-function)
  (setq imenu-create-index-function 'py--imenu-create-index-function)

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
        (set (make-local-variable 'beginning-of-defun-function) 'py-beginning-of-top-level)
        (set (make-local-variable 'end-of-defun-function) 'py-end-of-top-level)
        (define-key python-mode-map [(control meta a)] 'py-beginning-of-top-level)
        (define-key python-mode-map [(control meta e)] 'py-end-of-top-level))
    (set (make-local-variable 'beginning-of-defun-function) 'py-beginning-of-def-or-class)
    (set (make-local-variable 'end-of-defun-function) 'py-end-of-def-or-class)
    (define-key python-mode-map [(control meta a)] 'py-beginning-of-def-or-class)
    (define-key python-mode-map [(control meta e)] 'py-end-of-def-or-class))
  (when (and py--imenu-create-index-p
             (fboundp 'imenu-add-to-menubar)
             (ignore-errors (require 'imenu)))
    (setq imenu--index-alist (funcall py--imenu-create-index-function))
    ;; (setq imenu--index-alist (py--imenu-create-index-new))
    ;; (message "imenu--index-alist: %s" imenu--index-alist)
    (imenu-add-to-menubar "PyIndex"))
  ;; add the menu
  (when py-menu
    (easy-menu-add py-menu))
  (when py-hide-show-minor-mode-p (hs-minor-mode 1))
  (when py-outline-minor-mode-p (outline-minor-mode 1))
  (when (interactive-p) (message "python-mode loaded from: %s" python-mode-message-string)))

(defun py--all-shell-mode-setting ()
  (when py-fontify-shell-buffer-p
    (set (make-local-variable 'font-lock-defaults)
	 '(python-font-lock-keywords nil nil nil nil
				     (font-lock-syntactic-keywords
				      . py-font-lock-syntactic-keywords))))
  (setenv "PAGER" "cat")
  (setenv "TERM" "dumb")
  (set-syntax-table python-mode-syntax-table)
  (set (make-local-variable 'py--shell-unfontify) 'py-shell-unfontify-p)
  ;; (if py-auto-complete-p
  ;; (add-hook 'py-shell-mode-hook 'py--run-completion-timer)
  ;; (remove-hook 'py-shell-mode-hook 'py--run-completion-timer))

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
  (set (make-local-variable 'compilation-error-regexp-alist)
       py-compilation-regexp-alist)
  (set (make-local-variable 'comint-input-filter) 'py-history-input-filter)
  (set (make-local-variable 'comint-prompt-read-only) py-shell-prompt-read-only)
  ;; It might be useful having a different setting of `comint-use-prompt-regexp' in py-shell - please report when a use-case shows up
  ;; (set (make-local-variable 'comint-use-prompt-regexp) nil)
  (set (make-local-variable 'compilation-error-regexp-alist)
       py-compilation-regexp-alist)
  ;; (setq completion-at-point-functions nil)

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
  ;; (if py-shell-unfontify-p
  ;; (add-hook 'py-python-shell-mode-hook #'py--run-unfontify-timer (current-buffer))
  ;; (remove-hook 'py-python-shell-mode-hook 'py--run-unfontify-timer))
  (set-process-sentinel proc #'shell-write-history-on-exit)

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
  (when py-shell-menu
    (easy-menu-add py-menu))
  (if py-shell-unfontify-p
      (add-hook 'py-python-shell-mode-hook #'py--run-unfontify-timer (current-buffer))
    (remove-hook 'py-python-shell-mode-hook 'py--run-unfontify-timer)))

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
  ;; (py--unfontify-banner-intern)
  (if py-shell-unfontify-p
      (add-hook 'py-ipython-shell-mode-hook #'py--run-unfontify-timer (current-buffer))
    (remove-hook 'py-ipython-shell-mode-hook 'py--run-unfontify-timer))

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
  ;;                py-ipython-history))))
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
  (when py-shell-menu
    (easy-menu-add py-menu))
  ;; Running py-ipython-shell-mode-hook seems to need some delay
  (sit-for 0.5 t))


(provide 'python-components-foot)
;;; python-components-foot.el ends here
