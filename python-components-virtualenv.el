;;; python-components-virtualenv.el --- Python virtualenv support  -*- lexical-binding: t; -*-
;;; Virtualenv
;; Thanks Gabriele Lanaro and all working on that
;; The installation is fairly easy, you have the load option, put this
;; in your .emacs:

;; (load-file "/path/to/virtualenv.el")
;;
;; M-x virtualenv-activate

;; It will prompt you for the virtual environment path.
;; If you want to deactivate a virtual environment, use:

;; M-x virtualenv-deactivate


;;; Commentary:
;;

;;; Code:

(defvar virtualenv-workon-home nil)

(defvar virtualenv-name nil)

(defvar virtualenv-old-path nil)

(defvar virtualenv-old-exec-path nil)

(if (getenv "WORKON_HOME")
    (setq virtualenv-workon-home (getenv "WORKON_HOME"))
  (setq virtualenv-workon-home "~/.virtualenvs"))

;;TODO: Move to a generic UTILITY or TOOL package
(defun virtualenv-filter (predicate sequence)
  "Return a list of each SEQUENCE element for which the PREDICATE is non-nil.
The order of elements in SEQUENCE is retained."
  (let ((retlist '()))
    (dolist (element sequence (nreverse retlist))
      (when (funcall predicate element)
        (push element retlist)))))

(defun virtualenv-append-path (dir var)
  "Append DIR to a path-like variable VAR.

For example:
>>> (virtualenv-append-path \"/usr/bin:/bin\" \"/home/test/bin\")
\"/home/test/bin:/usr/bin:/bin\""
  (concat (expand-file-name dir)
          path-separator
          var))

(defun virtualenv-add-to-path (dir)
  "Add the specified DIR path element to the Emacs PATH."
  (setenv "PATH"
          (virtualenv-append-path dir
                                  (getenv "PATH"))))

(defun virtualenv-current ()
  "Display the current activated virtualenv."
  (interactive)
  (message virtualenv-name))

(defun virtualenv-activate (dir)
  "Activate the virtualenv located in specified DIR."
  (interactive "DVirtualenv Directory: ")
  ;; Eventually deactivate previous virtualenv
  (when virtualenv-name
    (virtualenv-deactivate))
  (let ((cmd (concat "source " dir "/bin/activate\n")))
    (comint-send-string (get-process (get-buffer-process "*shell*")) cmd)
    ;; Storing old variables
    (setq virtualenv-old-path (getenv "PATH"))
    (setq virtualenv-old-exec-path exec-path)

    (setenv "VIRTUAL_ENV" dir)
    (virtualenv-add-to-path (concat (py--normalize-directory dir) "bin"))
    (push (concat (py--normalize-directory dir) "bin")  exec-path)

    (setq virtualenv-name dir)))

(defun virtualenv-deactivate ()
  "Deactivate the current virtual environment."
  (interactive)
  ;; Restoring old variables
  (setenv "PATH" virtualenv-old-path)
  (setq exec-path virtualenv-old-exec-path)
  (message (concat "Virtualenv '" virtualenv-name "' deactivated."))
  (setq virtualenv-name nil))

(defun virtualenv-p (dir)
  "Check if a directory DIR is a virtualenv."
  (file-exists-p (concat dir "/bin/activate")))

(defun virtualenv-workon-complete ()
  "Return available completions for `virtualenv-workon'."
  (let
      ;;Varlist
      ((filelist (directory-files virtualenv-workon-home t)))
    ;; Get only the basename from the list of the virtual environments
    ;; paths
    (mapcar
     'file-name-nondirectory
     ;; Filter the directories and then the virtual environments
     (virtualenv-filter 'virtualenv-p
                        (virtualenv-filter 'file-directory-p filelist)))))

(defun virtualenv-workon (name)
  "Issue a virtualenvwrapper-like virtualenv-workon NAME command."
  (interactive (list (completing-read "Virtualenv: "
                                      (virtualenv-workon-complete))))
  (if (getenv "WORKON_HOME")
      (virtualenv-activate (concat (py--normalize-directory
                                    (getenv "WORKON_HOME")) name))
    (virtualenv-activate (concat
                          (py--normalize-directory virtualenv-workon-home)
                          name))))

(provide 'python-components-virtualenv)

;;; python-components-virtualenv.el ends here
