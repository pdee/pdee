;;; python-components-extensions.el --- more editing utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>

;; Keywords: lisp

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

;;; Code:

(defun py-indent-forward-line (&optional arg)
  "Indent and move one line forward to next indentation.
Returns column of line reached.

If `py-kill-empty-line' is non-nil, delete an empty line.
When closing a form, use py-close-block et al, which will move and indent likewise.
With \\[universal argument] just indent.
"
  (interactive "*P")
  (let ((orig (point))
        erg)
    (unless (eobp)
      (if (and (py--in-comment-p)(not py-indent-comments))
          (forward-line 1)
        (py-indent-line-outmost)
        (unless (eq 4 (prefix-numeric-value arg))
          (if (eobp) (newline)
            (progn (forward-line 1))
            (when (and py-kill-empty-line (empty-line-p) (not (looking-at "[ \t]*\n[[:alpha:]]")) (not (eobp)))
              (delete-region (line-beginning-position) (line-end-position)))))))
    (back-to-indentation)
    (when (or (eq 4 (prefix-numeric-value arg)) (< orig (point))) (setq erg (current-column)))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-dedent-forward-line (&optional arg)
  "Dedent line and move one line forward. "
  (interactive "*p")
  (py-dedent arg)
  (if (eobp)
      (newline)
    (forward-line 1))
  (end-of-line))

(defun py-dedent (&optional arg)
  "Dedent line according to `py-indent-offset'.

With arg, do it that many times.
If point is between indent levels, dedent to next level.
Return indentation reached, if dedent done, nil otherwise.

Affected by `py-dedent-keep-relative-column'. "
  (interactive "*p")
  (or arg (setq arg 1))
  (let ((orig (copy-marker (point)))
        erg)
    (dotimes (_ arg)
      (let* ((cui (current-indentation))
             (remain (% cui py-indent-offset))
             (indent (* py-indent-offset (/ cui py-indent-offset))))
        (beginning-of-line)
        (fixup-whitespace)
        (if (< 0 remain)
            (indent-to-column indent)
          (indent-to-column (- cui py-indent-offset)))))
    (when (< (point) orig)
      (setq erg (current-column)))
    (when py-dedent-keep-relative-column (goto-char orig))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-class-at-point ()
  "Return class definition as string.

With interactive call, send it to the message buffer too. "
  (interactive)
  (save-excursion
    (let* ((beg (py-backward-class))
	   (end (py-forward-class))
	   (res (when (and (numberp beg)(numberp end)(< beg end)) (buffer-substring-no-properties beg end))))
      (when (called-interactively-p 'any) (message "%s" res))
      res)))

(defun py-function-at-point ()
  "Return functions definition as string.

With interactive call, send it to the message buffer too. "
  (interactive)
  (save-excursion
    (let* ((beg (py-backward-function))
	   (end (py-forward-function))
	   (res (when (and (numberp beg)(numberp end)(< beg end)) (buffer-substring-no-properties beg end))))
      (when (called-interactively-p 'any) (message "%s" res))
      res)))

(defun py-backward-function ()
  "Jump to the beginning of defun. Returns point. "
  (interactive "p")
  (let ((pos (py-backward-def-or-class)))
    (when (called-interactively-p 'any) (message "%s" pos))
    pos))

(defun py-forward-function ()
  "Jump to the end of function. "
  (interactive "p")
  (let ((pos (py-forward-def-or-class)))
    (when (called-interactively-p 'any) (message "%s" pos))
    pos))

;; Functions for marking regions

(defun py-line-at-point ()
  "Return line as string.
  With interactive call, send it to the message buffer too. "
  (interactive)
  (let* ((beg (line-beginning-position))
	 (end (line-end-position))
	 (res (when (and (numberp beg)(numberp end)(< beg end)) (buffer-substring-no-properties beg end))))
    (when (called-interactively-p 'any) (message "%s" res))
    res))

(defun py-match-paren-mode (&optional arg)
  "py-match-paren-mode nil oder t"
  (interactive "P")
  (if (or arg (not py-match-paren-mode))
      (progn
	(setq py-match-paren-mode t)
        (setq py-match-paren-mode nil))))

(defun py--match-end-finish (cui)
  (let (skipped)
    (unless (eq (current-column) cui)
      (when (< (current-column) cui)
	(setq skipped (skip-chars-forward " \t" (line-end-position)))
	(setq cui (- cui skipped))
	;; may current-column greater as needed indent?
	(if (< 0 cui)
	    (progn
	      (unless (empty-line-p) (split-line))
	      (indent-to cui))
	  (forward-char cui))
	(unless (eq (char-before) 32)(insert 32)(forward-char -1))))))

(defun py--match-paren-forward ()
  (setq py--match-paren-forward-p t)
  (let ((cui (current-indentation)))
    (cond
     ((py--beginning-of-top-level-p)
      (py-forward-top-level-bol)
      (py--match-end-finish cui))
     ((py--beginning-of-class-p)
      (py-forward-class-bol)
      (py--match-end-finish cui))
     ((py--beginning-of-def-p)
      (py-forward-def-bol)
      (py--match-end-finish cui))
     ((py--beginning-of-if-block-p)
      (py-forward-if-block-bol)
      (py--match-end-finish cui))
     ((py--beginning-of-try-block-p)
      (py-forward-try-block-bol)
      (py--match-end-finish cui))
     ((py--beginning-of-for-block-p)
      (py-forward-for-block-bol)
      (py--match-end-finish cui))
     ((py--beginning-of-block-p)
      (py-forward-block-bol)
      (py--match-end-finish cui))
     ((py--beginning-of-clause-p)
      (py-forward-clause-bol)
      (py--match-end-finish cui))
     ((py--beginning-of-statement-p)
      (py-forward-statement-bol)
      (py--match-end-finish cui))
     (t (py-forward-statement)
	(py--match-end-finish cui)))))

(defun py--match-paren-backward ()
  (setq py--match-paren-forward-p nil)
  (let* ((cui (current-indentation))
	 (cuc (current-column))
	 (cui (min cuc cui)))
    (if (eq 0 cui)
	(py-backward-top-level)
      (when (empty-line-p) (delete-region (line-beginning-position) (point)))
      (py-backward-statement)
      (unless (< (current-column) cuc)
      (while (and (not (bobp))
		  (< cui (current-column))
		  (py-backward-statement)))))))

(defun py--match-paren-blocks ()
  (cond
   ((and (looking-back "^[ \t]*" (line-beginning-position))(if (eq last-command 'py-match-paren)(not py--match-paren-forward-p)t)
	 ;; (looking-at py-extended-block-or-clause-re)
	 (looking-at "[[:alpha:]_]"))
    ;; from beginning of top-level, block, clause, statement
    (py--match-paren-forward))
   (t
    (py--match-paren-backward))))

(defun py-match-paren ()
  "If at a beginning, jump to end and vice versa.

When called from within, go to the start.
Matches lists, but also block, statement, string and comment. "
  (interactive)
  (let ((pps (parse-partial-sexp (point-min) (point))))
    (cond
     ;; if inside string, go to beginning
     ((nth 3 pps)
      (goto-char (nth 8 pps)))
     ;; if inside comment, go to beginning
     ((nth 4 pps)
      (py-backward-comment))
     ;; at comment start, go to end of commented section
     ((and
       ;; unless comment starts where jumped to some end
       (not py--match-paren-forward-p)
       (eq 11 (car-safe (syntax-after (point)))))
      (py-forward-comment))
     ;; at string start, go to end
     ((or (eq 15 (car-safe (syntax-after (point))))
	  (eq 7 (car (syntax-after (point)))))
      (goto-char (scan-sexps (point) 1))
      (forward-char -1))
     ;; open paren
     ((eq 4 (car (syntax-after (point))))
      (goto-char (scan-sexps (point) 1))
      (forward-char -1))
     ((eq 5 (car (syntax-after (point))))
      (goto-char (scan-sexps (1+ (point)) -1)))
     ((nth 1 pps)
      (goto-char (nth 1 pps)))
     (t
      ;; Python specific blocks
      (py--match-paren-blocks)))))

(unless (boundp 'empty-line-p-chars)
  (defvar empty-line-p-chars "^[ \t\f\r]*$"))

(unless (functionp 'in-string-p)
  (defun in-string-p (&optional pos)
    (interactive)
    (let* ((orig (or pos (point)))
           (erg
            (save-excursion
              (save-restriction
                (widen)
                (beginning-of-defun)
                (numberp
                 (progn
                   (if (featurep 'xemacs)
                       (nth 3 (parse-partial-sexp (point) orig)
                            (nth 3 (parse-partial-sexp (point-min) (point)))))))))))
      (when (called-interactively-p 'any) (message "%s" erg))
      erg)))

(defun py-documentation (w)
  "Launch PyDOC on the Word at Point"
  (interactive
   (list (let* ((word (thing-at-point 'word))
                (input (read-string
                        (format "pydoc entry%s: "
                                (if (not word) "" (format " (default %s)" word))))))
           (if (string= input "")
               (if (not word) (error "No pydoc args given")
                 word) ;sinon word
             input)))) ;sinon input
  (shell-command (concat py-shell-name " -c \"from pydoc import help;help(\'" w "\')\"") "*PYDOCS*")
  (view-buffer-other-window "*PYDOCS*" t 'kill-buffer-and-window))

(defun eva ()
  "Put \"eval(...)\" forms around strings at point. "
  (interactive "*")
  (skip-chars-forward " \t\r\n\f")
  (let* ((bounds (ar-bounds-of-word-atpt))
         (beg (car bounds))
         (end (cdr bounds)))
    (goto-char end)
    (insert ")")
    (goto-char beg)
    (insert "eval(")))

(defun pst-here ()
  "Kill previous \"pdb.set_trace()\" and insert it at point. "
  (interactive "*")
  (let ((orig (copy-marker (point))))
    (search-backward "pdb.set_trace()")
    (replace-match "")
    (when (empty-line-p)
      (delete-region (line-beginning-position) (line-end-position)))
    (goto-char orig)
    (insert "pdb.set_trace()")))

(defalias 'durck 'py-printform-insert)
(defalias 'druck 'py-printform-insert)

(defun py-printform-insert (&optional arg strg)
  "Inserts a print statement out of current `(car kill-ring)' by default, inserts STRING if delivered.

With optional \\[universal-argument] print as string"
  (interactive "*P")
  (let* ((name (py--string-strip (or strg (car kill-ring))))
         ;; guess if doublequotes or parentheses are needed
         (numbered (not (eq 4 (prefix-numeric-value arg))))
         (form (cond ((or (eq major-mode 'python-mode)(eq major-mode 'py-shell-mode))
                      (if numbered
                          (concat "print(\"" name ": %s \" % (" name "))")
                        (concat "print(\"" name ": %s \" % \"" name "\")"))))))
    (insert form)))

(defun py-line-to-printform-python2 ()
  "Transforms the item on current in a print statement. "
  (interactive "*")
  (let* ((name (thing-at-point 'word))
         (form (cond ((or (eq major-mode 'python-mode)(eq major-mode 'py-shell-mode))
                      (concat "print(\"" name ": %s \" % " name ")")))))
    (delete-region (line-beginning-position) (line-end-position))
    (insert form))
  (forward-line 1)
  (back-to-indentation))

(defun py-boolswitch ()
  "Edit the assignment of a boolean variable, revert them.

I.e. switch it from \"True\" to \"False\" and vice versa"
  (interactive "*")
  (save-excursion
    (unless (py--end-of-statement-p)
      (py-forward-statement))
    (backward-word)
    (cond ((looking-at "True")
           (replace-match "False"))
          ((looking-at "False")
           (replace-match "True"))
          (t (message "%s" "Can't see \"True or False\" here")))))

(when (featurep 'thing-at-point-utils)
  (defun py-beginning-of-list (&optional iact orig limit done last)
    "Go to beginning of any parentized, braced or bracketed expression in statement. "
    (interactive "p")
    (save-restriction
      (let ((orig (or orig (point)))
            (done done)
            (limit (or limit (re-search-backward "^[a-zA-Z]" nil t 1)))
            (last last))
        (unless (or done (not limit)) (narrow-to-region limit (point-max)))
        (setq done t)
        (goto-char orig)
        (let* ((pt (car-safe (ar-in-parentized-p-atpt)))
               (br (car-safe (ar-in-braced-p-atpt)))
               (bk (car-safe (ar-in-bracketed-p-atpt)))
               (erg (car (sort (delq nil (list pt br bk)) '<))))
          (if erg
              (progn
                (goto-char (1- erg))
                (setq last erg)
                (py-beginning-of-list iact (1- erg) limit done last))
            (when last
              (goto-char last))
            (when iact (message "%s" last))
            last)))))

  (defun py-end-of-list (&optional iact orig limit done last)
    "Go to end of any parentized, braced or bracketed expression in statement. "
    (interactive "p")
    (save-restriction
      (let ((orig (or orig (point)))
            (done done)
            (limit (or limit (re-search-backward "^[a-zA-Z]" nil t 1)))
            (last last))
        (unless (or done (not limit)) (narrow-to-region limit (point-max)))
        (setq done t)
        (goto-char orig)
        (let* ((pt (car-safe (ar-in-parentized-p-atpt)))
               (br (car-safe (ar-in-braced-p-atpt)))
               (bk (car-safe (ar-in-bracketed-p-atpt)))
               (erg (car (sort (delq nil (list pt br bk)) '<))))
          (if erg
              (progn
                (goto-char (1- erg))
                (setq last erg)
                (py-end-of-list iact (1- erg) limit done last))
            (when last
              (goto-char last)
              (match-paren)
              (setq last (1+ (point)))
              (when iact (message "%s" last))
              last)))))))

(provide 'python-components-extensions)
;;; python-components-extensions.el ends here
