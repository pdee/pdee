;; This file is a great big shortcut for all the features contained in PDEE

;; Trick to get the filename of the installation directory

(defgroup pdee nil
  "Python Development Emacs Environment stuff."
  :group 'languages)

(defgroup python nil
  "Python Language's support for Emacs."
  :group 'languages)

;; Auto-detect installation dir from load-file-name if the lisp file
;; is not compiled; else from bytecomp-filename
(defcustom pdee-install-dir ""
"Directory where PDEE should be installed. Must end with a slash. "
  :type 'string
  :group 'pdee)

(defcustom pdee-unload-first t
  "If previosly loaded python- and related modes shall unloaded first, avoiding conflicts. "
  :type 'boolean
  :group 'pdee)

(defcustom pdee-load-all nil
  "Load all the pdee libraries at initialization, default is nil. "
  :type 'boolean
  :group 'pdee)

(defcustom pdee-default-mode 'pdee
  "Which python-mode should be loaded at start. "
  :type '(choice
  (const :tag "fgallina" fgallina)
  (const :tag "gnu" gnu)
  (const :tag "loveshack" loveshack)
  (const :tag "pdee" pdee)
  (const :tag "python-mode-el" python-mode-el)
  (const :tag "xemacs-lp-python-mode" xemacs-lp-python-mode)
  (const :tag "xemacs-mode-shipped" xemacs-mode-shipped))
  :group 'pdee)

(defvar py-install-directory (concat pdee-install-dir  "python-modes/"  (prin1-to-string pdee-default-mode) "/")
  "The directory, where core python-modes for choice reside. ")

(defun pdee-set-mode (&optional branch)
  "Select a python-mode to use. See `pdee-default-mode' for available choices.

Then set `py-install-directory', load the needed python(-mode).el
When called without arguments, default mode is switched on. "
  (interactive "P")
  (let ((branch (if (eq 4 (prefix-numeric-value branch))
                    (read-from-minibuffer "pdee-default-mode: " pdee-default-mode)
                  (or branch (prin1-to-string pdee-default-mode)))))
    (unless (string-match ".+/$" branch)
      (setq branch (concat branch "/")))
    (setq py-install-directory (concat pdee-install-dir "python-modes/" branch))
    (add-to-list 'load-path py-install-directory)
    (cond ((eq pdee-default-mode 'pdee)
           (load (concat py-install-directory "python-components-mode.el")))
          ((eq pdee-default-mode 'fgallina)
           (load (concat py-install-directory "python.el")))
          ((eq pdee-default-mode 'gnu)
           (load (concat py-install-directory "python.el")))
          ((eq pdee-default-mode 'loveshack)
           (load (concat py-install-directory "python.el")))
          ((eq pdee-default-mode 'python-mode-el)
           (load (concat py-install-directory "python-mode.el")))
          ((eq pdee-default-mode 'xemacs-lp-python-mode)
           (load (concat py-install-directory "python-mode.el")))
          ((eq pdee-default-mode 'xemacs-mode-shipped)
           (load (concat py-install-directory "python-mode.el"))))))

(when (ignore-errors pdee-install-dir)
(add-to-list 'load-path pdee-install-dir))

;; Adding paths to the variable load-path
(dolist (relpath '(""
                   "extensions/"
                   "extensions/yasnippet"
                   "extensions/auto-complete"
		   "extensions/eproject"))
  (add-to-list 'load-path (concat pdee-install-dir relpath)))

(when pdee-unload-first
  (when (featurep 'ipython) (unload-feature 'ipython t))
  (when (featurep 'python-mode) (unload-feature 'python-mode t))
    (when (featurep 'pymacs) (unload-feature 'pymacs t))
  (when (featurep 'pycomplete) (unload-feature 'pycomplete t)))

(when pdee-load-all
  (require 'pdee-python)
  (require 'pdee-completion)
  (require 'pdee-editing)
  (require 'pdee-bindings))

;;; Pymacs
(require 'pymacs (concat pdee-install-dir "extensions/pymacs.el"))

(defun setup-ropemacs ()
  "Setup the ropemacs harness"
  (setenv "PYTHONPATH"
          (concat
           (getenv "PYTHONPATH") path-separator
           (concat (expand-file-name pdee-install-dir) "python-libs/")))
  (message (getenv "PYTHONPATH"))
  (pymacs-load "ropemacs" "rope-")

  ;; Stops from erroring if there's a syntax err
  (setq ropemacs-codeassist-maxfixes 3)

  ;; Configurations
  (setq ropemacs-guess-project t)
  (setq ropemacs-enable-autoimport t)


  (setq ropemacs-autoimport-modules '("os" "shutil" "sys" "logging"
				      "django.*"))



  ;; Adding hook to automatically open a rope project if there is one
  ;; in the current or in the upper level directory
  (add-hook 'python-mode-hook
            (lambda ()
              (cond ((file-exists-p ".ropeproject")
                     (rope-open-project default-directory))
                    ((file-exists-p "../.ropeproject")
                     (rope-open-project (concat default-directory "..")))
                    )))
  )

;;; Ipython integration
(defun pdee-setup-ipython ()
  "Setup ipython integration with python-mode"
  (interactive)
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \[[0-9]+\]: "
   python-shell-prompt-output-regexp "Out\[[0-9]+\]: "
   python-shell-completion-setup-code ""
   python-shell-completion-string-code "';'.join(__IP.complete('''%s'''))\n")
  )

;;; Flymake additions, I have to put this one somwhere else?


(defun flymake-create-copy-file ()
  "Create a copy local file"
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace)))
    (file-relative-name
     temp-file
     (file-name-directory buffer-file-name))))

(defun flymake-command-parse (cmdline)
  "Parses the command line CMDLINE in a format compatible
       with flymake, as:(list cmd-name arg-list)

The CMDLINE should be something like:

 flymake %f python custom.py %f

%f will be substituted with a temporary copy of the file that is
 currently being checked.
"
  (let ((cmdline-subst (replace-regexp-in-string "%f" (flymake-create-copy-file) cmdline)))
    (setq cmdline-subst (split-string-and-unquote cmdline-subst))
    (list (first cmdline-subst) (rest cmdline-subst))
    ))


(when (load-file (concat pdee-install-dir "extensions/flymake-patch.el"))
  (setq flymake-info-line-regex
        (append flymake-info-line-regex '("unused$" "^redefinition" "used$")))
  (load-library "flymake-cursor"))

(defun pdee-setup-checker (cmdline)
  (add-to-list 'flymake-allowed-file-name-masks
               (list "\\.py\\'" (apply-partially 'flymake-command-parse cmdline)))
  )


;; Python or python mode?
(eval-after-load 'python
  '(progn
     ;;==================================================
     ;; Ropemacs Configuration
     ;;==================================================
     (setup-ropemacs)

     ;;==================================================
     ;; Virtualenv Commands
     ;;==================================================
     (autoload 'virtualenv-activate "virtualenv"
       "Activate a Virtual Environment specified by PATH" t)
     (autoload 'virtualenv-workon "virtualenv"
       "Activate a Virtual Environment present using virtualenvwrapper" t)


     ;; Not on all modes, please
     (add-hook 'python-mode-hook 'flymake-find-file-hook)


     )
  )
;;; Cython Mode
(autoload 'cython-mode "cython-mode" "Mode for editing Cython source files")

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode))

;; Py3 files
(add-to-list 'auto-mode-alist '("\\.py3\\'" . python-mode))

(add-hook 'python-mode-hook
          '(lambda ()
             (cond ((boundp 'python-mode-map)
                    (define-key python-mode-map "\C-m" 'newline-and-indent))
                   ((boundp 'py-mode-map)
                    (define-key py-mode-map "\C-m" 'newline-and-indent)))))

;; Rope bindings
(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map "\C-ci" 'rope-auto-import)
	    (define-key python-mode-map "\C-c\C-d" 'rope-show-calltip))
	  )


(provide 'pdee-init)
