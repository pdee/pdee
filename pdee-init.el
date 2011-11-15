;; This file is a great big shortcut for all the features contained in emacs-for-python

;; Trick to get the filename of the installation directory

(defgroup python nil
  "Python Language's support for Emacs."
  :group 'languages)

(defcustom pdee-install-dir nil
  "Directory where PDEE should be installed. Must end with a slash. "
  :type 'string
  :group 'python)

(defcustom pdee-unload-first t
  "If previosly loaded python- and related modes shall unloaded first, avoiding conflicts. "
  :type 'string
  :group 'python)

(unless pdee-install-dir (message "pdee-install-dir must be set. Do M-x customize pdee-install-dir RET")) 

(add-to-list 'load-path pdee-install-dir)

;; Adding paths to the variable load-path
(dolist (relpath '(""
                   "extensions/"
                   "extensions/yasnippet"
                   "extensions/auto-complete"
		   "extensions/eproject"
                   )
                 )
  (add-to-list 'load-path (concat pdee-install-dir relpath)))

(when pdee-unload-first
  (when (featurep 'ipython) (unload-feature 'ipython t))
  (when (featurep 'python-mode) (unload-feature 'python-mode t))
    (when (featurep 'pymacs) (unload-feature 'pymacs t))
  (when (featurep 'pycomplete) (unload-feature 'pycomplete t)))

(require 'pdee-python)
(require 'pdee-completion)
(require 'pdee-editing)
(require 'pdee-bindings)

(provide 'pdee-init)
