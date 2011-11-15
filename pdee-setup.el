;; pdee-setup.el - setup and load all the paths necessary

(defgroup python nil
  "Python Language's support for Emacs."
  :group 'languages)

(defcustom pdee-install-dir nil
  "Directory where PDEE should be installed. "
  :type 'string
  :group 'python)

(unless pdee-install-dir
  (setq pdee-install-dir (expand-file-name "~/")))

;; (defconst pdee-install-dir
;;   (file-name-directory (or load-file-name
;;                            (when (boundp 'bytecomp-filename) bytecomp-filename)
;;                            buffer-file-name))
;;   "Installation directory of emacs-for-python terminating with a slash"
;;   )


;; Adding paths to the variable load-path
(dolist (relpath '(""
                   "extensions/"
                   "extensions/yasnippet"
                   "extensions/auto-complete"
		   "extensions/eproject"
                   )
                 )
  (add-to-list 'load-path (concat pdee-install-dir relpath)))

(provide 'pdee-setup)
