;;; py-virtualenv.el --- Switching Python virtual environments

;; Original Authors:

;; Copyright (C) 2010 Gabriele Lanaro <gabriele.lanaro@gmail.com>
;; Version: 0.1
;; Url: http://github.com/gabrielelanaro/emacs-starter-kit

;; Copyright (c) 2010 Aaron Culich
;; Author: Aaron Culich <aculich@gmail.com>
;; Vcs-git: git://github.com/aculich/virtualenv.git

;; Aaron Culich declares being inspired by earlier
;; implementations by Jesse Legg and Jeremiah Dodds.

;; Maintainer: Andreas Roehler <andreas.roehler@online.de>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The installation is fairly easy, you have the load option, put this
;; in your .emacs:

;; (load-file "/path/to/virtualenv.el")
;;
;; Otherwise you can do it with the load path:

;; (add-to-list 'load-path "Path/to/virtualenv.el/containing/directory/"
;; (require 'virtualenv)

;; The usage is very intuitive, to activate a virtualenv use

;; M-x virtualenv-activate

;; It will prompt you for the virtual environment path.
;; If you want to deactivate a virtual environment, use:

;; M-x virtualenv-deactivate

(defcustom virtualenv-dir
  (or (getenv "WORKON_HOME") "~/.virtualenvs")
  "Default location for user's virtual environments"
  :group 'python-mode
  :type 'directory)


(defcustom virtualenv-root "~/.virtualenvs"
  "Default location for user's virtual environments"
  :group 'virtualenv
  :type 'directory)


(defvar virtualenv-default-directory nil
  "Buffer-local variable that should be set in your project's
top-level .dir-locals.el file as the place you want to start the python shell.
When using paster set this to where your .ini files live, e.g.: \
((nil . ((virtualenv-default-directory . \"/projects/foo\"))))")


(defvar virtualenv-workon nil
  "Buffer-local variable that should be set in your project's
top-level .dir-locals.el file, e.g.: \
((nil . ((virtualenv-workon . \"tg2.1\"))))")
(put 'virtualenv-workon 'safe-local-variable 'stringp)

(defvar virtualenv-workon-session nil
  "The virtualenv that this emacs session will workon.")
(put 'virtualenv-default-directory 'risky-local-variable 'stringp)

(defvar virtualenv-workon-history nil
  "History list of virtual environments used.")

(setq virtualenv-name nil)

;;TODO: Move to a generic UTILITY or TOOL package
(defun virtualenv-filter (predicate sequence)
  "Apply to each element of SEQUENCE the PREDICATE, if FUNCTION
  returns non-nil append the element to the return value of
  virtualenv-filter: a list"
  (let ((retlist '()))
    (dolist (element sequence)
      (when (funcall predicate element)
        (push element retlist)))
    (nreverse retlist)))

(defun virtualenv-append-path (dir var)
  "Append DIR to a path-like varibale VAR, for example:
 (virtualenv-append-path /usr/bin:/bin /home/test/bin) -> /home/test/bin:/usr/bin:/bin"
  (concat (expand-file-name dir)
          path-separator
          var))

(defun virtualenv-add-to-path (dir)
  "Add the specified path element to the Emacs PATH"
  (setenv "PATH"
	  (virtualenv-append-path dir
                                  (getenv "PATH"))))

(defun virtualenv-current ()
  "Barfs the current activated virtualenv"
  (interactive)
  (message virtualenv-name))

(defun virtualenv-p (dir)
  "Check if a directory is a virtualenv"
  (file-exists-p (concat dir "/bin/activate")))

(defun virtualenv-workon-complete ()
  "Return available completions for virtualenv-workon"
  (let
      ;;Varlist
      ((filelist (directory-files virtualenv-dir t)))
    ;; Get only the basename from the list of the virtual environments
    ;; paths
    (mapcar 'file-name-nondirectory
            ;; Filter the directories and then the virtual environments
            (virtualenv-filter 'virtualenv-p
                               (virtualenv-filter 'file-directory-p filelist)))))


(defun virtualenv-activate (dir)
  "Activate the virtualenv located in DIR"
  (interactive "DVirtualenv Directory: ")

  ;; Eventually deactivate previous virtualenv
  (when virtualenv-name
    (virtualenv-deactivate))

  ;; Storing old variables
  (setq virtualenv-old-path (getenv "PATH"))
  (setq virtualenv-old-exec-path exec-path)

  (setenv "VIRTUAL_ENV" dir)
  (virtualenv-add-to-path (concat (normalize-directory dir) "bin"))
  (add-to-list 'exec-path (concat (normalize-directory dir) "bin"))

  (setq virtualenv-name dir)

  (message (concat "Virtualenv '" virtualenv-name "' activated.")))

(defun virtualenv-deactivate ()
  "Deactivate the current virtual enviroment"
  (interactive)

  ;; Restoring old variables
  (setenv "PATH" virtualenv-old-path)
  (setq exec-path virtualenv-old-exec-path)

  (message (concat "Virtualenv '" virtualenv-name "' deactivated."))

  (setq virtualenv-name nil))

;; (defun virtualenv-workon (name)
;;   "If environment variable $WORKON_HOME is  set, activate VE with that value
;; 
;; otherwise use `virtualenv-root' "
;;   (interactive (list (completing-read "Virtualenv: " (virtualenv-workon-complete))))
;;   (if (getenv "WORKON_HOME")
;;       (virtualenv-activate (concat (normalize-directory (getenv "WORKON_HOME")) name))
;;     (virtualenv-activate (concat (normalize-directory virtualenv-dir) name))))


(defun virtualenv-workon (&optional env)
  "Activate a virtual environment for python.
Optional argument ENV if non-nil, either use the string given as
the virtual environment or if not a string then query the user."
  (interactive "P")

  ;; reset virtualenv-workon-session if env is non-nil and also not a
  ;; string (e.g. invoked interactively with C-u prefix arg)
  (when (and env (not (stringp env)))
    (setq virtualenv-workon-session nil))

  ;; if env is a string, then just use it, otherwise check to see if
  ;; we have already queried the user the session, at last querying
  ;; the user if all else fails.
  (let ((env
	 (cond
	  ((stringp env) env)
	  ((stringp virtualenv-workon-session)
	   virtualenv-workon-session)
	  (t
	   (let* ((default (car virtualenv-workon-history))
		  (prompt (concat
			   "Virtualenv to activate"
			   (when default
			     (format " (default %s)" default))
			   ": "))
		  ;; look for directories in virtualenv-root that
		  ;; contain a bin directory for tab-completion
		  (dirs (remove
			 nil
			 (mapcar
			  (lambda (d)
			    (when (file-exists-p
				   (expand-file-name
				    (concat
				     virtualenv-root "/" d "/bin")))
			      d))
			  (directory-files virtualenv-root nil "^[^.]"))))
		  (result (completing-read prompt dirs nil t nil
					   'virtualenv-workon-history)))
	     ;; if the user entered nothing, then return the default
	     ;; if there is one
	     (if (not (string= result ""))
		 result
	       default))))))
    
    (let* ((buffer (get-buffer "*Python*"))
	   (kill (or (when buffer
		       (yes-or-no-p
			"Python process already running. Kill? ")))))
      
      (if (or (not buffer) kill)
	  (progn
	    (when buffer
	      (kill-buffer buffer))
	    (setq virtualenv-workon-session env)
	    (when virtualenv-workon-starts-python
	      (cond ((fboundp 'py-shell)
		     (py-shell))
		    ((fboundp 'python-shell)
		     (python-shell))
		    (t (error "Could not start a python shell!"))))
	    (message (format "Now using virtualenv: %s" env)))
	(message "Not changing virtualenv")))))

(provide 'virtualenv)
