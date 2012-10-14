;;; python-components-completion.el --- shipped emacs23 completion

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008  Free Software Foundation, Inc.
;; Copyright (C) 2009, 2010  David Love

;; Original Author: Dave Love <fx@gnu.org>
;; Created: Nov 2003
;; Keywords: languages
;; URL: http://www.loveshack.ukfsn.org/emacs/

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

;;; Commentary: extractions from python.el, see above

;;; Code
(require 'comint)
(require 'python-components-macros)

(defvar python-pdbtrack-is-tracking-p nil)

(defconst python-pdbtrack-track-range 10000
  "Max number of characters from end of buffer to search for stack entry.")

(defvar python-preoutput-skip-next-prompt nil)

(defvar python-preoutput-result nil
  "Data from last `_emacs_out' line seen by the preoutput filter.")

(defvar python-preoutput-continuation nil
  "If non-nil, funcall this when `python-preoutput-filter' sees `_emacs_ok'.")

(defvar python-preoutput-leftover nil)

(defvar python-version-checked nil)

(defconst python-compilation-regexp-alist
  ;; FIXME: maybe these should move to compilation-error-regexp-alist-alist.
  ;;   The first already is (for CAML), but the second isn't.  Anyhow,
  ;;   these are specific to the inferior buffer.  -- fx
  `((,(rx line-start (1+ (any " \t")) "File \""
	  (group (1+ (not (any "\"<")))) ; avoid `<stdin>' &c
	  "\", line " (group (1+ digit)))
     1 2)
    (,(rx " in file " (group (1+ not-newline)) " on line "
	  (group (1+ digit)))
     1 2)
    ;; pdb stack trace
    (,(rx line-start "> " (group (1+ (not (any "(\"<"))))
	  "(" (group (1+ digit)) ")" (1+ (not (any "("))) "()")
     1 2))
  "`compilation-error-regexp-alist' for inferior Python.")

;;;

(defun python-guess-indent ()
  "Guess step for indentation of current buffer.
Set `python-indent' locally to the value guessed."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (done indent)
	(while (and (not done) (not (eobp)))
	  (when (and (re-search-forward (rx ?: (0+ space)
					    (or (syntax comment-start)
						line-end))
					nil 'move)
		     (python-open-block-statement-p))
	    (save-excursion
	      (python-beginning-of-statement)
	      (let ((initial (current-indentation)))
		(if (zerop (python-next-statement))
		    (setq indent (- (current-indentation) initial)))
		(if (and indent (>= indent 2) (<= indent 8)) ; sanity check
		    (setq done t))))))
	(when done
	  (when (/= indent (default-value 'python-indent))
	    (set (make-local-variable 'python-indent) indent)
	    (unless (= tab-width python-indent)
	      (setq indent-tabs-mode nil)))
	  indent)))))

(defun python-pdbtrack-get-source-buffer (block)
  "Return line number and buffer of code indicated by block's traceback text.

We look first to visit the file indicated in the trace.

Failing that, we look for the most recently visited python-mode buffer
with the same name or having the named function.

If we're unable find the source code we return a string describing the
problem."

  (if (not (string-match python-pdbtrack-stack-entry-regexp block))

      "Traceback cue not found"

    (let* ((filename (match-string 1 block))
           (lineno (string-to-number (match-string 2 block)))
           (funcname (match-string 3 block))
           funcbuffer)

      (cond ((file-exists-p filename)
             (list lineno (find-file-noselect filename)))

            ((setq funcbuffer (python-pdbtrack-grub-for-buffer funcname lineno))
             (if (string-match "/Script (Python)$" filename)
                 ;; Add in number of lines for leading '##' comments:
                 (setq lineno
                       (+ lineno
                          (with-current-buffer funcbuffer
                            (if (equal (point-min)(point-max))
                                0
                              (count-lines
                               (point-min)
                               (max (point-min)
                                    (string-match "^\\([^#]\\|#[^#]\\|#$\\)"
                                                  (buffer-substring
                                                   (point-min) (point-max)))
                                    )))))))
               (list lineno funcbuffer))

            ((= (elt filename 0) ?\<)
             (format "(Non-file source: '%s')" filename))

            (t (format "Not found: %s(), %s" funcname filename)))
      )
    )
  )

(defun python-pdbtrack-overlay-arrow (activation)
  "Activate or deactivate arrow at beginning-of-line in current buffer."
  (if activation
      (progn
        (setq overlay-arrow-position (make-marker)
              overlay-arrow-string "=>"
              python-pdbtrack-is-tracking-p t)
        (set-marker overlay-arrow-position
                    (save-excursion (beginning-of-line) (point))
                    (current-buffer)))
    (setq overlay-arrow-position nil
          python-pdbtrack-is-tracking-p nil)))

(defun python-pdbtrack-track-stack-file (text)
  "Show the file indicated by the pdb stack entry line, in a separate window.

Activity is disabled if the buffer-local variable
`python-pdbtrack-do-tracking-p' is nil.

We depend on the pdb input prompt being a match for
`python-pdbtrack-input-prompt'.

If the traceback target file path is invalid, we look for the
most recently visited python-mode buffer which either has the
name of the current function or class, or which defines the
function or class.  This is to provide for scripts not in the
local filesytem (e.g., Zope's 'Script \(Python)', but it's not
Zope specific).  If you put a copy of the script in a buffer
named for the script and activate python-mode, then pdbtrack will
find it."
  ;; Instead of trying to piece things together from partial text
  ;; (which can be almost useless depending on Emacs version), we
  ;; monitor to the point where we have the next pdb prompt, and then
  ;; check all text from comint-last-input-end to process-mark.
  ;;
  ;; Also, we're very conservative about clearing the overlay arrow,
  ;; to minimize residue.  This means, for instance, that executing
  ;; other pdb commands wipe out the highlight.  You can always do a
  ;; 'where' (aka 'w') PDB command to reveal the overlay arrow.

  (let* ((origbuf (current-buffer))
	 (currproc (get-buffer-process origbuf)))

    (if (not (and currproc python-pdbtrack-do-tracking-p))
        (python-pdbtrack-overlay-arrow nil)

      (let* ((procmark (process-mark currproc))
             (block (buffer-substring (max comint-last-input-end
                                           (- procmark
                                              python-pdbtrack-track-range))
                                      procmark))
             target target_fname target_lineno target_buffer)

        (if (not (string-match (concat python-pdbtrack-input-prompt "$") block))
            (python-pdbtrack-overlay-arrow nil)

          (setq target (python-pdbtrack-get-source-buffer block))

          (if (stringp target)
              (progn
                (python-pdbtrack-overlay-arrow nil)
                (message "pdbtrack: %s" target))

            (setq target_lineno (car target)
                  target_buffer (cadr target)
                  target_fname (buffer-file-name target_buffer))
            (switch-to-buffer-other-window target_buffer)
            (goto-char (point-min))
            (forward-line (1- target_lineno))
            (message "pdbtrack: line %s, file %s" target_lineno target_fname)
            (python-pdbtrack-overlay-arrow t)
            (pop-to-buffer origbuf t)
            ;; in large shell buffers, above stuff may cause point to lag output
            (goto-char procmark)))))))

(defun python-check-comint-prompt (&optional proc)
  "Return non-nil if and only if there's a normal prompt in the inferior buffer.
If there isn't, it's probably not appropriate to send input to return Eldoc
information etc.  If PROC is non-nil, check the buffer for that process."
  (with-current-buffer (process-buffer (or proc (python-proc)))
    (save-excursion
      (save-match-data
	(re-search-backward (concat python--prompt-regexp " *\\=")
			    nil t)))))

(autoload 'comint-check-proc "comint")

(defun python-proc ()
  "Return the current Python process.
See variable `python-buffer'.  Starts a new process if necessary."
  ;; Fixme: Maybe should look for another active process if there
  ;; isn't one for `python-buffer'.
  (unless (comint-check-proc python-buffer)
    (run-python nil t))
  (get-buffer-process (if (derived-mode-p 'inferior-python-mode)
			  (current-buffer)
			python-buffer)))

(defun python-symbol-completions (symbol)
  "Return a list of completions of the string SYMBOL from Python process.
The list is sorted.
Uses `python-imports' to load modules against which to complete."
  (when (stringp symbol)
    (let ((completions
	   (condition-case ()
	       (car (read-from-string
		     (python-send-receive
		      (format "emacs.complete(%S,%s)"
			      (substring-no-properties symbol)
			      python-imports))))
	     (error nil))))
      (sort
       ;; We can get duplicates from the above -- don't know why.
       (delete-dups completions)
       #'string<))))

(defun python-completion-at-point ()
  (interactive)
  (let ((end (point))
	(start (save-excursion
		 (and (re-search-backward
		       (rx (or buffer-start (regexp "[^[:alnum:]._]"))
			   (group (1+ (regexp "[[:alnum:]._]"))) point)
		       nil t)
		      (match-beginning 1)))))
    (when start
      (list start end
            (completion-table-dynamic 'python-symbol-completions)))))

(provide 'python-components-completion)
;;; python-components-completion.el ends here
