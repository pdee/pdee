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
  "Load all the pdee libraries at initialization, default is nil.

This variable has no effect at the moment. "
  :type 'boolean
  :group 'pdee)

(defcustom pdee-default-mode 'pdee
  "Which python-mode should be loaded at start.

Actuallly only components-python-mode branch is implemented.
components-python-mode is the development-branch of python-mode.el "
  :type '(choice
  (const :tag "pdee" pdee)
  :group 'pdee))

(defvar py-install-directory "")
(defcustom py-install-directory ""
  "Directory where python-mode.el and it's subdirectories should be installed. Needed for completion and other environment stuff only. "

  :type 'string
  :group 'python-mode)


;; Adding paths to the variable load-path
(dolist (relpath '(""
                   "extensions/"
                   "extensions/auto-complete"
		   "extensions/eproject"))
  (add-to-list 'load-path (concat pdee-install-dir relpath)))

(defun unload-python-components ()
  (interactive)
      (when (featurep 'python-components-mode) (unload-feature 'python-components-mode t))
    (when (featurep 'python-components-edit) (unload-feature 'python-components-edit t))
  (when (featurep 'python-components-intern) (unload-feature 'python-components-intern t))
  (when (featurep 'python-components-move)(unload-feature 'python-components-move t))
  (when (featurep 'python-components-shell)(unload-feature 'python-components-shell t))
  (when (featurep 'python-components-pdb)(unload-feature 'python-components-pdb t))
  (when (featurep 'python-components-skeletons)(unload-feature 'python-components-skeletons t))
  (when (featurep 'python-components-help)(unload-feature 'python-components-help t))
  (when (featurep 'python-components-test)(unload-feature 'python-components-test t))
  (when (featurep 'python-components-extensions)(unload-feature 'python-components-extensions t))
  (when (featurep 'python-components-imenu)(unload-feature 'python-components-imenu t))
  (when (featurep 'python-describe-symbol)(unload-feature 'python-describe-symbol t)))


(when pdee-unload-first
  (when (featurep 'ipython) (unload-feature 'ipython t))
  (when (featurep 'python-mode) (unload-feature 'python-mode t))
  (when (featurep 'pymacs) (unload-feature 'pymacs t))
  (when (featurep 'pycomplete) (unload-feature 'pycomplete t))
  (unload-python-components))


(when pdee-load-all
  (require 'open-next-line)
  (require 'pdee-completion)
  (require 'pdee-editing)
  (require 'pymacs (concat pdee-install-dir "extensions/pymacs.el")))

(defun pdee-load-all ()
  (interactive)
  (dolist (relpath '(""
                     "extensions/"
                     "extensions/yasnippet"
                     "extensions/auto-complete"
                     "extensions/eproject"))
    (add-to-list 'load-path (concat pdee-install-dir relpath)))
  (load (concat pdee-install-dir "python-components-mode.el"))
  (require 'open-next-line)
  (require 'pdee-completion)
  (require 'pdee-editing)
  (require 'pymacs (concat pdee-install-dir "extensions/pymacs.el"))
  )

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
; this is  deliverd py python-mode already
(defun pdee-setup-ipython ()
  "Setup ipython integration is already delivered by python-mode.

You should not need to call this command. "
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

(message "py-install-directory %s" py-install-directory)
(when
    (file-readable-p (concat py-install-directory "/extensions/flymake-patch.el"))
  (load
   (concat py-install-directory "/extensions/flymake-patch.el")
   nil t)
  (setq flymake-info-line-regex
        (append flymake-info-line-regex '("unused$" "^redefinition" "used$")))
  (load-library "flymake-cursor"))

(defun pdee-setup-checker (cmdline)
  (add-to-list 'flymake-allowed-file-name-masks
               (list "\\.py\\'" (apply-partially 'flymake-command-parse cmdline))))

;; Python or python mode?
(eval-after-load 'python-mode
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

;; Rope bindings
(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map "\C-ci" 'rope-auto-import)
	    (define-key python-mode-map "\C-c\C-d" 'rope-show-calltip))
	  )

(provide 'pdee-init)
