;;; python-components-foot.el --- foot -*- lexical-binding: t; -*-

;; URL: https://gitlab.com/python-mode-devs

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

(defun py-message-which-python-mode ()
  (if (buffer-file-name)
      (if (string= "python-mode-el" (buffer-file-name))
	  (message "%s" "python-mode loaded from python-mode-el")
	(message "%s" "python-mode loaded from python-components-mode"))
    (message "python-mode loaded from: %s" python-mode-message-string)))

(defalias 'py-next-statement 'py-forward-statement)
;; #134, cython-mode compatibility
(defalias 'py-end-of-statement 'py-forward-statement)
(defalias 'py-beginning-of-statement 'py-backward-statement)
(defalias 'py-beginning-of-block 'py-backward-block)
(defalias 'py-end-of-block 'py-forward-block)
(defalias 'py-previous-statement 'py-backward-statement)
(defalias 'py-markup-region-as-section 'py-sectionize-region)

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

(autoload 'python-mode "python-mode" "Python Mode." t)

(defun all-mode-setting ()
  (set (make-local-variable 'indent-tabs-mode) py-indent-tabs-mode)
  )

(defun py--update-version-dependent-keywords ()
  (let ((kw-py2 '(("\\<print\\>" . 'font-lock-keyword-face)
                  ("\\<file\\>" . 'py-builtins-face)))
        (kw-py3 '(("\\<print\\>" . 'py-builtins-face))))
    (font-lock-remove-keywords 'python-mode kw-py3)
    (font-lock-remove-keywords 'python-mode kw-py2)
    ;; avoid to run py-choose-shell again from ‘py--fix-start’
    (cond ((string-match "ython3" py-python-edit-version)
           (font-lock-add-keywords 'python-mode kw-py3 t))
          (t (font-lock-add-keywords 'python-mode kw-py2 t)))))

(define-derived-mode python-mode prog-mode python-mode-modeline-display
  "Major mode for editing Python files.

To submit a report, enter `\\[py-submit-bug-report]'
from a‘python-mode’ buffer.
Do `\\[py-describe-mode]' for detailed documentation.
To see what version of ‘python-mode’ you are running,
enter `\\[py-version]'.

This mode knows about Python indentation,
tokens, comments (and continuation lines.
Paragraphs are separated by blank lines only.

COMMANDS

‘py-shell’\tStart an interactive Python interpreter in another window
‘py-execute-statement’\tSend statement at point to Python default interpreter
‘py-backward-statement’\tGo to the initial line of a simple statement

etc.

See available commands listed in files commands-python-mode at directory doc

VARIABLES

‘py-indent-offset’	indentation increment
‘py-shell-name’		shell command to invoke Python interpreter
‘py-split-window-on-execute’		When non-nil split windows
‘py-switch-buffers-on-execute-p’	When non-nil switch to the Python output buffer

\\{python-mode-map}"
  :group 'python-mode
  ;; load known shell listed in
  ;; Local vars
  (all-mode-setting)
  (set (make-local-variable 'electric-indent-inhibit) nil)
  (set (make-local-variable 'outline-regexp)
       (concat (mapconcat 'identity
                          (mapcar #'(lambda (x) (concat "^\\s-*" x "\\_>"))
                                  py-outline-mode-keywords)
                          "\\|")))
  (when py-font-lock-defaults-p
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
					. py-font-lock-syntactic-keywords)))))
  (py--update-version-dependent-keywords)
  ;; (cond ((string-match "ython3" py-python-edit-version)
  ;;        (font-lock-add-keywords 'python-mode
  ;;       			 '(("\\<print\\>" . 'py-builtins-face)
  ;;       			   ("\\<file\\>" . nil))))
  ;;       (t (font-lock-add-keywords 'python-mode
  ;;       			   '(("\\<print\\>" . 'font-lock-keyword-face)
  ;;       			     ("\\<file\\>" . 'py-builtins-face)))))
  (set (make-local-variable 'which-func-functions) 'py-which-def-or-class)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "^[ \t]*#+ *")

  (if py-empty-comment-line-separates-paragraph-p
      (progn
        (set (make-local-variable 'paragraph-separate) (concat "\f\\|^[\t]*$\\|^[ \t]*" comment-start "[ \t]*$\\|^[\t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$"))
        (set (make-local-variable 'paragraph-start)
	     (concat "\f\\|^[ \t]*$\\|^[ \t]*" comment-start "[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$"))
	(set (make-local-variable 'paragraph-separate)
	     (concat "\f\\|^[ \t]*$\\|^[ \t]*" comment-start "[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$")))
    (set (make-local-variable 'paragraph-separate) "\f\\|^[ \t]*$\\|^[\t]*#[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$")
    (set (make-local-variable 'paragraph-start) "\f\\|^[ \t]*$\\|^[\t]*#[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$"))
  (set (make-local-variable 'comment-column) 40)
  ;; (set (make-local-variable 'comment-indent-function) #'py--comment-indent-function)
  (set (make-local-variable 'indent-region-function) 'py-indent-region)
  (set (make-local-variable 'indent-line-function) 'py-indent-line)
  ;; introduced to silence compiler warning, no real setting
  ;; (set (make-local-variable 'hs-hide-comments-when-hiding-all) 'py-hide-comments-when-hiding-all)
  (set (make-local-variable 'outline-heading-end-regexp) ":[^\n]*\n")
  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)
  (set (make-local-variable 'add-log-current-defun-function) 'py-current-defun)
  (set (make-local-variable 'fill-paragraph-function) 'py-fill-paragraph)
  (set (make-local-variable 'normal-auto-fill-function) 'py-fill-string-or-comment)
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  (set (make-local-variable 'tab-width) py-indent-offset)
  (set (make-local-variable 'electric-indent-mode) nil)
  (and py-load-skeletons-p (py-load-skeletons))
  (and py-guess-py-install-directory-p (py-set-load-path))
  ;; (and py-autopair-mode
  ;;      (declare-function autopair-python-triple-quote-action "autopair" ())
  ;;      (declare-function autopair-default-handle-action "autopair" ())
  ;;      (load-library "autopair")
  ;;      (add-hook 'python-mode-hook
  ;;                #'(lambda ()
  ;;                    (setq autopair-handle-action-fns
  ;;                          (list #'autopair-default-handle-action
  ;;                                #'autopair-python-triple-quote-action))))
  ;;      (py-autopair-mode-on))
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
  (when py-trailing-whitespace-smart-delete-p
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local))
  (py-shell-prompt-set-calculated-regexps)
  (setq comint-prompt-regexp py-shell--prompt-calculated-input-regexp)
  (cond
   (py-complete-function
    (add-hook 'completion-at-point-functions
              py-complete-function nil 'local))
   (py-load-pymacs-p
    (add-hook 'completion-at-point-functions
              'py-complete-completion-at-point nil 'local))
   (py-do-completion-p
    (add-hook 'completion-at-point-functions
              'py-shell-complete nil 'local)))
  ;; #'python-shell-completion-at-point nil 'local)))
  ;; (if py-auto-complete-p
  ;; (add-hook 'python-mode-hook 'py--run-completion-timer)
  ;; (remove-hook 'python-mode-hook 'py--run-completion-timer))
  ;; (when py-auto-complete-p
  ;; (add-hook 'python-mode-hook
  ;; (lambda ()
  ;; (run-with-idle-timer 1 t 'py-shell-complete))))
  (add-hook 'python-mode-hook
            (lambda ()
              (if py-electric-backspace-p (py-electric-backspace-mode 1)
                (py-electric-backspace-mode -1))))
  (if py-auto-fill-mode
      (add-hook 'python-mode-hook 'py--run-auto-fill-timer)
    (remove-hook 'python-mode-hook 'py--run-auto-fill-timer))
  (add-hook 'python-mode-hook
            (lambda ()
              (setq imenu-create-index-function py--imenu-create-index-function)))
  ;; caused insert-file-contents error lp:1293172
  ;;  (add-hook 'after-change-functions 'py--after-change-function nil t)
  (if py-defun-use-top-level-p
      (progn
        (set (make-local-variable 'beginning-of-defun-function) 'py-backward-top-level)
        (set (make-local-variable 'end-of-defun-function) 'py-forward-top-level)
        (define-key python-mode-map [(control meta a)] 'py-backward-top-level)
        (define-key python-mode-map [(control meta e)] 'py-forward-top-level))
    (set (make-local-variable 'beginning-of-defun-function) 'py-backward-def-or-class)
    (set (make-local-variable 'end-of-defun-function) 'py-forward-def-or-class)
    (define-key python-mode-map [(control meta a)] 'py-backward-def-or-class)
    (define-key python-mode-map [(control meta e)] 'py-forward-def-or-class))
  (when py-sexp-use-expression-p
    (define-key python-mode-map [(control meta f)] 'py-forward-expression)
    (define-key python-mode-map [(control meta b)] 'py-backward-expression))

  (when py-hide-show-minor-mode-p (hs-minor-mode 1))
  (when py-outline-minor-mode-p (outline-minor-mode 1))
  (when (and py-debug-p (called-interactively-p 'any))
    (py-message-which-python-mode))
  (when py-use-menu-p
    (py-define-menu python-mode-map))
  (force-mode-line-update))

(define-derived-mode py-shell-mode comint-mode py-modeline-display
  "Major mode for Python shell process.

Variables
‘py-shell-prompt-regexp’,
‘py-shell-prompt-output-regexp’,
`py-shell-input-prompt-2-regexp',
‘py-shell-fontify-p’,
‘py-completion-setup-code’,
‘py-shell-completion-string-code’,
can customize this mode for different Python interpreters.

This mode resets ‘comint-output-filter-functions’ locally, so you
may want to re-add custom functions to it using the
‘py-shell-mode-hook’.

\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (setq mode-line-process '(":%s"))
  (all-mode-setting)
  ;; (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'py-shell--prompt-calculated-input-regexp) nil)
  (set (make-local-variable 'py-shell--block-prompt) nil)
  (set (make-local-variable 'py-shell--prompt-calculated-output-regexp) nil)
  (py-shell-prompt-set-calculated-regexps)
  (set (make-local-variable 'comint-prompt-read-only) t)
  (set (make-local-variable 'comint-output-filter-functions)
       '(ansi-color-process-output
         py-comint-watch-for-first-prompt-output-filter
         py-pdbtrack-comint-output-filter-function
         py-comint-postoutput-scroll-to-bottom
         comint-watch-for-password-prompt))
  (set (make-local-variable 'compilation-error-regexp-alist)
       py-shell-compilation-regexp-alist)
  (compilation-shell-minor-mode 1)
  (add-hook 'completion-at-point-functions
	    #'py-shell-completion-at-point nil 'local)
  (define-key py-shell-mode-map [(control c) (control r)] 'py-nav-last-prompt)
  (make-local-variable 'py-pdbtrack-buffers-to-kill)
  (make-local-variable 'py-shell-fast-last-output)
  (set (make-local-variable 'py-shell--block-prompt) nil)
  (set (make-local-variable 'py-shell--prompt-calculated-output-regexp) nil)
  (py-shell-prompt-set-calculated-regexps)
  (if py-shell-fontify-p
      (progn
  	(py-shell-font-lock-turn-on))
    (py-shell-font-lock-turn-off)))

(make-obsolete 'jpython-mode 'jython-mode nil)

;; (push "*Python*"  same-window-buffer-names)
;; (push "*IPython*"  same-window-buffer-names)

;; Python Macro File
(unless (member '("\\.py\\'" . python-mode) auto-mode-alist)
  (push (cons "\\.py\\'"  'python-mode)  auto-mode-alist))

(unless (member '("\\.pym\\'" . python-mode) auto-mode-alist)
  (push (cons "\\.pym\\'"  'python-mode)  auto-mode-alist))

(unless (member '("\\.pyc\\'" . python-mode)  auto-mode-alist)
  (push (cons "\\.pyc\\'"  'python-mode)  auto-mode-alist))

;; Pyrex Source
(unless (member '("\\.pyx\\'" . python-mode)  auto-mode-alist)
  (push (cons "\\.pyx\\'"  'python-mode) auto-mode-alist))

;; Python Optimized Code
(unless (member '("\\.pyo\\'" . python-mode)  auto-mode-alist)
  (push (cons "\\.pyo\\'"  'python-mode) auto-mode-alist))

;; Pyrex Definition File
(unless (member '("\\.pxd\\'" . python-mode)  auto-mode-alist)
  (push (cons "\\.pxd\\'"  'python-mode) auto-mode-alist))

;; Python Repository
(unless (member '("\\.pyr\\'" . python-mode)  auto-mode-alist)
  (push (cons "\\.pyr\\'"  'python-mode)  auto-mode-alist))

;; Python Stub file
;; https://www.python.org/dev/peps/pep-0484/#stub-files
(unless (member '("\\.pyi\\'" . python-mode)  auto-mode-alist)
  (push (cons "\\.pyi\\'"  'python-mode)  auto-mode-alist))

;; Python Path Configuration
(unless (member '("\\.pth\\'" . python-mode)  auto-mode-alist)
  (push (cons "\\.pth\\'"  'python-mode)  auto-mode-alist))

;; Python Wheels
(unless (member '("\\.whl\\'" . python-mode)  auto-mode-alist)
  (push (cons "\\.whl\\'"  'python-mode)  auto-mode-alist))

(unless (member '("!#[          ]*/.*[jp]ython[0-9.]*" . python-mode) magic-mode-alist)
  (push '("!#[ \\t]*/.*[jp]ython[0-9.]*" . python-mode) magic-mode-alist))

;;  lp:1355458, what about using ‘magic-mode-alist’?

(defalias 'py-hungry-delete-forward 'c-hungry-delete-forward)
(defalias 'py-hungry-delete-backwards 'c-hungry-delete-backwards)

(defalias 'py-end-of-def-or-class 'py-forward-def-or-class)
(defalias 'py-beginning-of-def-or-class 'py-backward-def-or-class)

;; https://gitlab.com/python-mode-devs/python-mode/-/issues/105#note_1095808557
(puthash "python-"
         (append (gethash "python" definition-prefixes) '("python-mode"))
         definition-prefixes)
;;;
(provide 'python-components-foot)

;;; python-components-foot.el ends here
