;;; python-components-electric.el --- Python-mode electric inserts  -*- lexical-binding: t; -*-

;; URL: https://gitlab.com/python-mode-devs

;; Keywords: languages

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

;;; Commentary:

;;

;;; Code:
(defun py-electric-colon (arg)
  "Insert a colon and indent accordingly.

If a numeric argument ARG is provided, that many colons are inserted
non-electrically.

Electric behavior is inhibited inside a string or
comment or by universal prefix \\[universal-argument].

Switched by ‘py-electric-colon-active-p’, default is nil
See also ‘py-electric-colon-greedy-p’"
  (interactive "*P")
  (cond
   ((not py-electric-colon-active-p)
    (self-insert-command (prefix-numeric-value arg)))
   ;;
   ((and py-electric-colon-bobl-only
         (save-excursion
           (py-backward-statement)
           (not (py--beginning-of-block-p))))
    (self-insert-command (prefix-numeric-value arg)))
   ;;
   ((eq 4 (prefix-numeric-value arg))
    (self-insert-command 1))
   ;;
   (t
    (insert ":")
    (unless (py-in-string-or-comment-p)
      (let ((orig (copy-marker (point)))
            (indent (py-compute-indentation)))
        (unless (or (eq (current-indentation) indent)
                    (and py-electric-colon-greedy-p
                         (eq indent
                             (save-excursion
                               (py-backward-statement)
                               (current-indentation))))
                    (and (looking-at py-def-or-class-re)
                         (< (current-indentation) indent)))
          (beginning-of-line)
          (delete-horizontal-space)
          (indent-to indent))
        (goto-char orig))
      (when py-electric-colon-newline-and-indent-p
        (py-newline-and-indent))))))

;; TODO: PRouleau: I would like to better understand this.
;;                 I don't understand the docstring.
;;                 What was the completion bug this is reacting to?
(defun py-electric-close (arg)
  "Close completion buffer when no longer needed.

It is it's sure, it's no longer needed, i.e. when inserting a space.

Works around a bug in ‘choose-completion’."

  (interactive "*P")
  (cond
   ((not py-electric-close-active-p)
    (self-insert-command (prefix-numeric-value arg)))
   ;;
   ((eq 4 (prefix-numeric-value arg))
    (self-insert-command 1))
   ;;
   (t (if (called-interactively-p 'any)
          (self-insert-command (prefix-numeric-value arg))
        ;; used from dont-indent-code-unnecessarily-lp-1048778-test
        (insert " ")))))

;; TODO: PRouleau: describe the electric behavior of '#'.
;;       This description should be in docstring of the
;;       ‘py-electric-comment-p’ user option and be referred to here.
;;       I currently don't understand what it should be and prefer not
;;       having to infer it from code.
;;       - From what I saw, the intent is to align the comment being
;;         typed to the one on line above or at the indentation level.
;;         - Is there more to it it than that?
;;         - I would like to see the following added (possibly via options):
;;           - When inserting the '#' follow it with a space, such that
;;             comment text is separated from the leading '#' by one space, as
;;             recommended in PEP-8
;;             URL https://www.python.org/dev/peps/pep-0008/#inline-comments
(defun py-electric-comment (arg)
  "Insert a comment.  If starting a comment, indent accordingly.

If a numeric argument ARG is provided, that many \"#\" are inserted
non-electrically.
With \\[universal-argument] \"#\" electric behavior is inhibited inside a
string or comment."
  (interactive "*P")
  (if (and py-indent-comments py-electric-comment-p)
      (if (ignore-errors (eq 4 (car-safe arg)))
          (insert "#")
        (when (and (eq last-command (quote py-electric-comment))
                   (looking-back " " (line-beginning-position)))
          (forward-char -1))
        (if (called-interactively-p 'any)
            (self-insert-command (prefix-numeric-value arg))
          (insert "#"))
        (let ((orig (copy-marker (point)))
              (indent (py-compute-indentation)))
          (unless (eq (current-indentation) indent)
            (goto-char orig)
            (beginning-of-line)
            (delete-horizontal-space)
            (indent-to indent)
            (goto-char orig))
          (when py-electric-comment-add-space-p
            (unless (looking-at "[ \t]")
              (insert " "))))
        (setq last-command this-command))
    (self-insert-command (prefix-numeric-value arg))))

;; Electric deletion
(defun py-empty-out-list-backward ()
  "Deletes all elements from list before point."
  (interactive "*")
  (and (member (char-before) (list ?\) ?\] ?\}))
       (let ((orig (point))
             (thischar (char-before))
             pps cn)
         (forward-char -1)
         (setq pps (parse-partial-sexp (point-min) (point)))
         (if (and (not (nth 8 pps)) (nth 1 pps))
             (progn
               (goto-char (nth 1 pps))
               (forward-char 1))
           (cond ((or (eq thischar 41)(eq thischar ?\)))
                  (setq cn "("))
                 ((or (eq thischar 125) (eq thischar ?\}))
                  (setq cn "{"))
                 ((or (eq thischar 93)(eq thischar ?\]))
                  (setq cn "[")))
           (skip-chars-backward (concat "^" cn)))
         (delete-region (point) orig)
         (insert-char thischar 1)
         (forward-char -1))))

;; TODO: PRouleau Question: [...]

;;       - Also, the mapping for [backspace] in python-mode-map only works in
;;         graphics mode, it does not work when Emacs runs in terminal mode.
;;         It would be nice to have a binding that works in terminal mode too.
;; keep-one handed over form ‘py-electric-delete’ maybe
(defun py-electric-backspace (&optional arg)
  "Delete one or more of whitespace chars left from point.
Honor indentation.

If called at whitespace below max indentation,

Delete region when both variable ‘delete-active-region’ and ‘use-region-p’
are non-nil.

With \\[universal-argument], deactivate electric-behavior this time,
delete just one character before point.

At no-whitespace character, delete one before point.

"
  (interactive "*P")
  (unless (bobp)
    (let ((backward-delete-char-untabify-method 'untabify)
	  indent
	  done)
      (cond
       ;; electric-pair-mode
       ((and electric-pair-mode
             (or
              (and
               (ignore-errors (eq 5 (car (syntax-after (point)))))
               (ignore-errors (eq 4 (car (syntax-after (1- (point)))))))
              (and
               (ignore-errors (eq 7 (car (syntax-after (point)))))
               (ignore-errors (eq 7 (car (syntax-after (1- (point)))))))))
      (delete-char 1)
      (backward-delete-char-untabify 1))
       ((eq 4 (prefix-numeric-value arg))
	(backward-delete-char-untabify 1))
       ((use-region-p)
        ;; Emacs23 doesn't know that var
        (if (boundp 'delete-active-region)
	    (delete-active-region)
	  (delete-region (region-beginning) (region-end))))
       ((looking-back "[[:graph:]]" (line-beginning-position))
	(backward-delete-char-untabify 1))
       ;; before code
       ((looking-back "^[ \t]+" (line-beginning-position))
        (setq indent (py-compute-indentation))
	(cond ((< indent (current-indentation))
	       (back-to-indentation)
	       (delete-region (line-beginning-position) (point))
	       (indent-to indent))
	      ((<=  (current-column) py-indent-offset)
	       (delete-region (line-beginning-position) (point)))
	      ((eq 0 (% (current-column) py-indent-offset))
	       (delete-region (point) (progn (backward-char py-indent-offset) (point))))
	      (t (delete-region
		  (point)
		  (progn
		    ;; go backward the remainder
		    (backward-char (% (current-column) py-indent-offset))
		    (point))))))
       ((looking-back "[[:graph:]][ \t]+" (line-beginning-position))
	;; in the middle fixup-whitespace
	(setq done (line-end-position))
	(fixup-whitespace)
	;; if just one whitespace at point, delete that one
	(or (< (line-end-position) done) (delete-char 1)))

       ;; (if (< 1 (abs (skip-chars-backward " \t")))
       ;; 		 (delete-region (point) (progn (skip-chars-forward " \t") (point)))
       ;; 	       (delete-char 1))

       ((bolp)
	(delete-char -1))
       (t
	(py-indent-line nil t))))))

(defun py-electric-delete (&optional arg)
  "Delete one or more of whitespace chars right from point.
Honor indentation.

Delete region when both variable ‘delete-active-region’ and ‘use-region-p’
are non-nil.

With \\[universal-argument], deactivate electric-behavior this time,
delete just one character at point.

At spaces in line of code, call fixup-whitespace.
At no-whitespace char, delete one char at point.
"
  (interactive "P*")
  (unless (eobp)
    (let* (;; py-ert-deletes-too-much-lp:1300270-dMegYd
	   ;; x = {'abc':'def',
           ;;     'ghi':'jkl'}
	   (backward-delete-char-untabify-method 'untabify)
	   (indent (py-compute-indentation))
	   ;; (delpos (+ (line-beginning-position) indent))
	   ;; (line-end-pos (line-end-position))
	   ;; (orig (point))
	   done)
      (cond
       ((eq 4 (prefix-numeric-value arg))
	(delete-char 1))
       ;; delete active region if one is active
       ((use-region-p)
	;; Emacs23 doesn't know that var
	(if (boundp 'delete-active-region)
            (delete-active-region)
	  (delete-region (region-beginning) (region-end))))
       ((looking-at "[[:graph:]]")
	(delete-char 1))
       ((or (eolp) (looking-at "[ \t]+$"))
	(cond
	 ((eolp) (delete-char 1))
	 ((< (+ indent (line-beginning-position)) (line-end-position))
	  (end-of-line)
	  (while (and (member (char-before) (list 9 32 ?\r))
		      (< indent (current-column)))
	    (backward-delete-char-untabify 1)))))
       (;; before code
	(looking-at "[ \t]+[[:graph:]]")
	;; before indent
	(if (looking-back "^[ \t]*" (line-beginning-position))
	    (cond ((< indent (current-indentation))
		   (back-to-indentation)
		   (delete-region (line-beginning-position) (point))
		   (indent-to indent))
		  ((< 0 (% (current-indentation) py-indent-offset))
		   (back-to-indentation)
		   (delete-region (point) (progn (backward-char (% (current-indentation) py-indent-offset)) (point))))
		  ((eq 0 (% (current-indentation) py-indent-offset))
		   (back-to-indentation)
		   (delete-region (point) (progn (backward-char py-indent-offset) (point))))
		  (t
		   (skip-chars-forward " \t")
		   (delete-region (line-beginning-position) (point))))
	  ;; in the middle fixup-whitespace
	  (setq done (line-end-position))
	  (fixup-whitespace)
	  ;; if just one whitespace at point, delete that one
	  (or (< (line-end-position) done) (delete-char 1))))
       (t (delete-char 1))))))

;; TODO: PRouleau: the electric yank mechanism is currently commented out.
;;       Is this a feature to keep?  Was it used?  I can see a benefit for it.
;;       Why is it currently disabled?
(defun py-electric-yank (&optional arg)
  "Perform command ‘yank’ followed by an ‘indent-according-to-mode’.
Pass ARG to the command ‘yank’."
  (interactive "P")
  (cond
   (py-electric-yank-active-p
    (yank arg)
    ;; (py-indent-line)
    )
   (t
    (yank arg))))

(defun py-toggle-py-electric-colon-active ()
  "Toggle use of electric colon for Python code."
  (interactive)
  (setq py-electric-colon-active-p (not py-electric-colon-active-p))
  (when (and py-verbose-p (called-interactively-p 'interactive)) (message "py-electric-colon-active-p: %s" py-electric-colon-active-p)))

;; TODO: PRouleau: It might be beneficial to have toggle commands for all
;;       the electric behaviours, not just the electric colon.

;; required for pending-del and delsel modes
(put (quote py-electric-colon) 'delete-selection t) ;delsel
(put (quote py-electric-colon) 'pending-delete t) ;pending-del
(put (quote py-electric-backspace) 'delete-selection 'supersede) ;delsel
(put (quote py-electric-backspace) 'pending-delete 'supersede) ;pending-del
(put (quote py-electric-delete) 'delete-selection 'supersede) ;delsel
(put (quote py-electric-delete) 'pending-delete 'supersede) ;pending-del

(provide 'python-components-electric)
;;; python-components-electric.el ends here
