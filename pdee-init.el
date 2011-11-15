;; This file is a great big shortcut for all the features contained in emacs-for-python

;; Trick to get the filename of the installation directory

(defgroup python nil
  "Python Language's support for Emacs."
  :group 'languages)

(defcustom pdee-install-dir nil
  "Directory where PDEE should be installed. Must end with a slash. "
  :type 'string
  :group 'python)

(unless pdee-install-dir
  (message "pdee-install-dir %s" (concat "not found, use " (expand-file-name "~/")))
  (setq pdee-install-dir (concat (expand-file-name "~/") "pdee")))

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

(require 'pdee-python)
(require 'pdee-completion)
(require 'pdee-editing)
(require 'pdee-bindings)

(provide 'pdee-init)
