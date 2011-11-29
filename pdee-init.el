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

(provide 'pdee-init)
