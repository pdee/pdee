;;; python-shell-complete.el -- Add suport for completion in py-shell

;; Maintainer: Andreas Roehler <andreas.roehler@online.de>
;; Original Author: Lukasz Pankowski

;; py-comint-output-filter-function is modified version from:
;; python-mode.el --- Major mode for editing Python programs

;; Copyright (C) 1992,1993,1994  Tim Peters

;; Author: 2003-2004 http://sf.net/projects/python-mode
;;         1995-2002 Barry A. Warsaw
;;         1992-1994 Tim Peters
;; Maintainer: python-mode@python.org
;; Created:    Feb 1992
;; Keywords:   python languages oop

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

;; To get tab completion in Python shell you may add these line to
;; your ~/.emacs file:
;;
;; (add-hook 'py-shell-hook
;;           '(lambda ()
;;              (require 'py-shell-complete) ; nil t)
;;              (when (functionp 'py-shell-complete)
;;                ;; this should be set in py-shell
;;                (setq comint-input-sender 'py-shell-simple-send)
;;                (local-set-key [tab] 'py-shell-complete))))

(require 'comint)

(defvar py-shell-input-lines nil
  "Collect input lines send interactively to the Python process in
order to allow injecting completion command between keyboard interrupt
and resending the lines later. The lines are stored in reverse order")

;;; need to clear py-shell-input-lines if primary prompt found

(defun py-comint-output-filter-function (string)
  "Watch output for Python prompt and exec next file waiting in queue.
This function is appropriate for `comint-output-filter-functions'."
  ;; TBD: this should probably use split-string
  (when (and (or (string-equal string ">>> ")
		 (and (>= (length string) 5)
		      (string-equal (substring string -5) "\n>>> ")))
	     (or (setq py-shell-input-lines nil)
		 py-file-queue))
    (pop-to-buffer (current-buffer))
    (py-safe (delete-file (car py-file-queue)))
    (setq py-file-queue (cdr py-file-queue))
    (if py-file-queue
	(let ((pyproc (get-buffer-process (current-buffer))))
	  (py-execute-file pyproc (car py-file-queue))))))

;;;

(defun py-shell-simple-send (proc string)
  (setq py-shell-input-lines (cons string py-shell-input-lines))
  (comint-simple-send proc string))

(if (functionp 'comint-redirect-send-command-to-process)
    (progn
      (defalias
	'py-shell-redirect-send-command-to-process
	'comint-redirect-send-command-to-process)
      (defalias
	'py-shell-dynamic-simple-complete
	'comint-dynamic-simple-complete))

  ;; XEmacs

  (make-variable-buffer-local 'comint-redirect-completed)
  (make-variable-buffer-local 'py-shell-redirect-output-buffer)
  (make-variable-buffer-local 'py-shell-redirect-orginal-output-filter)

  (defun py-shell-redirect-filter-function (proc string)
    (let ((procbuf (process-buffer proc))
	  outbuf prompt-pos)
      (with-current-buffer procbuf
	(setq outbuf py-shell-redirect-output-buffer
	      prompt-pos (string-match comint-prompt-regexp string)))
      (if prompt-pos
	  (setq string (substring string 0 prompt-pos)))
      (save-excursion
	(set-buffer outbuf)
	(goto-char (point-max))
	(insert string))
      (if prompt-pos
	  (with-current-buffer procbuf
	    (set-process-filter proc py-shell-redirect-orginal-output-filter)
	    (setq comint-redirect-completed t))))
    "")

  (defun py-shell-redirect-send-command-to-process
    (command output-buffer process echo no-display)
    "Note: ECHO and NO-DISPLAY are ignored"
    ;; prepear
    (with-current-buffer (process-buffer process)
      (setq comint-redirect-completed nil
	    py-shell-redirect-output-buffer (get-buffer output-buffer)
	    py-shell-redirect-orginal-output-filter (process-filter process)))
    (set-process-filter process 'py-shell-redirect-filter-function)
    ;; run
    (comint-simple-send process command))

  (defun py-shell-dynamic-simple-complete (stub candidates)
    (let ((completion (try-completion stub (mapcar 'list candidates))))
      (cond
       ((null completion)
	nil)
       ((eq completion t)
	(message "Sole completion")
	'sole)
       ((> (length completion) (length stub))
	(insert (substring completion (length stub)))
	(if (eq (try-completion completion (mapcar 'list candidates)) t)
	    (progn (message "Completed")
		   'sole)
	  (message "Partially completed")
	  'partial))
       (t
	(with-output-to-temp-buffer "*Completions*"
	  (display-completion-list (sort candidates 'string<)))
	'listed)))))

(defun py-shell-execute-string-now (string)
  "Send to Python interpreter process PROC \"exec STRING in {}\".
and return collected output"
  (let* ((proc (get-process py-which-bufname))
	 (cmd (format "exec '''%s''' in {}"
		      (mapconcat 'identity (split-string string "\n") "\\n")))
	(procbuf (process-buffer proc))
	(outbuf (get-buffer-create " *pyshellcomplete-output*"))
	(lines (reverse py-shell-input-lines)))
    (if (and proc (not py-file-queue))
	(unwind-protect
	    (condition-case nil
		(progn
		  (if lines
		      (with-current-buffer procbuf
			(py-shell-redirect-send-command-to-process
			 "\C-c" outbuf proc nil t)
			;; wait for output
			(while (not comint-redirect-completed)
			  (accept-process-output proc 1))))
		  (with-current-buffer outbuf
		    (delete-region (point-min) (point-max)))
		  (with-current-buffer procbuf
		    (py-shell-redirect-send-command-to-process
		     cmd outbuf proc nil t)
		    (while (not comint-redirect-completed) ; wait for output
		      (accept-process-output proc 1)))
		  (with-current-buffer outbuf
		    (buffer-substring (point-min) (point-max))))
	      (quit (with-current-buffer procbuf
		      (interrupt-process proc comint-ptyp)
		      (while (not comint-redirect-completed) ; wait for output
			(accept-process-output proc 1)))
		    (signal 'quit nil)))
	    (if (with-current-buffer procbuf comint-redirect-completed)
		(while lines
		  (with-current-buffer procbuf
		    (py-shell-redirect-send-command-to-process
		     (car lines) outbuf proc nil t))
		  (accept-process-output proc 1)
		  (setq lines (cdr lines))))))))

;;;;

(defun py-dot-word-before-point ()
  (buffer-substring
   (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point))
   (point)))

;;;###autoload
(defun py-shell-complete (&optional arg)
  "Complete word before point, if any. Otherwise insert TAB.
With prefix argument \\[universal-argument] a TAB is inserted. "
  (interactive "*P")
  (if (eq 4 (prefix-numeric-value arg))
      (insert-tab)
    (let ((orig (point))
          (word (py-dot-word-before-point))
          result)
      (if (equal word "")
          (tab-to-tab-stop)	   ; non nil so the completion is over
        (setq result (py-shell-execute-string-now (format "
def print_completions(namespace, text, prefix=''):
   for name in namespace:
       if name.startswith(text):
           print prefix + name

def complete(text):
    import __builtin__
    import __main__
    if '.' in text:
        terms = text.split('.')
        try:
            if hasattr(__main__, terms[0]):
                obj = getattr(__main__, terms[0])
            else:
                obj = getattr(__builtin__, terms[0])
            for term in terms[1:-1]:
                obj = getattr(obj, term)
            print_completions(dir(obj), terms[-1], text[:text.rfind('.') + 1])
        except AttributeError:
            pass
    else:
        import keyword
        print_completions(keyword.kwlist, text)
        print_completions(dir(__builtin__), text)
        print_completions(dir(__main__), text)
complete('%s')
" word)))
        (if (eq result nil)
            (message "Could not do completion as the Python process is busy")
          (let ((comint-completion-addsuffix nil)
                (completions (if (split-string "\n" "\n")
                                 (split-string result "\n" t) ; XEmacs
                               (split-string result "\n"))))
            (ignore-errors (py-shell-dynamic-simple-complete word completions)))))
      ;; word before has not further completions
      (when (eq (point) orig)
        (insert-tab)))))

(provide 'python-shell-complete)

;; pyshell-complete.el ends here
