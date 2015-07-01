;;; python-components-extensions.el --- more editing utilities

;; Copyright (C) 2015  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>

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
    (when (interactive-p) (message "%s" erg))
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
    (dotimes (i arg)
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
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py--close-intern (regexp)
  "Core function, internal used only. "
  (let ((cui (car (py--go-to-keyword (symbol-value regexp)))))
    (message "%s" cui)
    (py--end-base regexp (point))
    (forward-line 1)
    (if py-close-provides-newline
        (unless (empty-line-p) (split-line))
      (fixup-whitespace))
    (indent-to-column cui)
    cui))

(defun py-close-def ()
  "Set indent level to that of beginning of function definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py--close-intern 'py-def-re)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-close-class ()
  "Set indent level to that of beginning of class definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py--close-intern 'py-class-re)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-close-def-or-class ()
  "Set indent level to that of beginning of def-or-class definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py--close-intern 'py-def-or-class-re)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-close-clause ()
  "Set indent level to that of beginning of clause definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py--close-intern 'py-block-or-clause-re)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-close-block ()
  "Set indent level to that of beginning of block definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py--close-intern 'py-block-re)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-close-block-or-clause ()
  "Set indent level to that of beginning of block-or-clause definition.

If final line isn't empty and `py-close-block-or-clause-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py--close-intern 'py-block-or-clause-re)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-class-at-point ()
  "Return class definition as string.

With interactive call, send it to the message buffer too. "
  (interactive)
  (save-excursion
    (let* ((beg (ar-py-beginning-of-class))
	   (end (ar-py-end-of-class))
	   (res (when (and (numberp beg)(numberp end)(< beg end)) (buffer-substring-no-properties beg end))))
      (when (interactive-p) (message "%s" res))
      res)))

(defun ar-py-function-at-point ()
  "Return functions definition as string.

With interactive call, send it to the message buffer too. "
  (interactive)
  (save-excursion
    (let* ((beg (ar-py-beginning-of-function))
	   (end (ar-py-end-of-function))
	   (res (when (and (numberp beg)(numberp end)(< beg end)) (buffer-substring-no-properties beg end))))
      (when (interactive-p) (message "%s" res))
      res)))

(defun ar-py-beginning-of-function (&optional count)
  "Jump to the beginning of defun. Returns point. "
  (interactive "p")
  (let ((pos (ar-py-beginning-of-def-or-class nil count)))
    (when (interactive-p) (message "%s" pos))
    pos))

(defun ar-py-beginning-of-class (&optional count)
  "Jump to the beginning of class definition. Returns column. "
  (interactive "p")
  (let ((pos (ar-py-beginning-of-def-or-class t count)))
    (when (interactive-p) (message "%s" pos))
    pos))

(defun ar-py-end-of-function (&optional class count)
  "Jump to the end of function. "
  (interactive "p")
  (let ((pos (ar-py-end-of-def-or-class nil count)))
    (when (interactive-p) (message "%s" pos))
    pos))

;; Functions for marking regions

(defun ar-py-line-at-point ()
  "Return line as string.
  With interactive call, send it to the message buffer too. "
  (interactive)
  (let* ((beg (line-beginning-position))
	 (end (line-end-position))
	 (res (when (and (numberp beg)(numberp end)(< beg end)) (buffer-substring-no-properties beg end))))
    (when (interactive-p) (message "%s" res))
    res))

(defun ar-py-looking-at-keywords-p ()
  "If looking at a python keyword. Returns t or nil. "
  (interactive)
  (let* ((kwds1 (car (nth 1 (eval (eval (quote (car font-lock-defaults)))))))
         (kwds3 (car (nth 3 (eval (eval (quote (car font-lock-defaults)))))))
	 (res
	  (or
           (looking-at kwds1)
           (looking-at kwds3))))
    (when (interactive-p) (message "looking-at keywords: %s" res))
    res))

(defun ar-py-match-paren-mode (&optional arg)
  "py-match-paren-mode nil oder t"
  (interactive "P")
  (if (or arg (not py-match-paren-mode))
      (progn
	(setq py-match-paren-mode t)
        ;; 	(define-key python-mode-map (kbd (concat "<" py-match-paren-key ">")) 'py-match-paren))
        (setq py-match-paren-mode nil))))

(defun ar-py-match-paren ()
  "Goto to the opening or closing of block before or after point.

With arg, do it that many times.
 Closes unclosed block if jumping from beginning. "
  (interactive)
  (let ((cuc (current-column))
	(cid (current-indentation)))
    (ar-py-beginning-of-block-or-clause)
    (if (< cuc (current-indentation))
	(goto-char cuc)
      (back-to-indentation)
      (when (eq (point) cuc)
	(ar-py-end-of-block)))))

;; from sh-werkstatt.el. Introduced here for convenience.
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
      (when (interactive-p) (message "%s" erg))
      erg)))

;; (unless (functionp 'empty-line-p)
;;   (defun empty-line-p (&optional bound noerror count)
;;     "Returns t if cursor is at an empty line, nil otherwise."
;;     (interactive)
;;     (save-excursion
;;       (beginning-of-line)
;;       (when (interactive-p)
;;         (message "%s" (looking-at empty-line-p-chars)))
;;       (looking-at empty-line-p-chars))))

(defun ar-py-documentation (w)
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

(defun py-printform-insert (&optional arg string)
  "Inserts a print statement out of current `(car kill-ring)' by default, inserts STRING if delivered.

With optional \\[universal-argument] print as string"
  (interactive "*P")
  (let* ((name (py--string-strip (or arg (car kill-ring))))
         ;; guess if doublequotes or parentheses are needed
         (numbered (not (eq 4 (prefix-numeric-value arg))))
         (form (cond ((or (eq major-mode 'python-mode)(eq major-mode 'py-shell-mode))
                      (if numbered
                          (concat "print(\"" name ": %s \" % (" name "))")
                        (concat "print(\"" name ": %s \" % \"" name "\")"))))))
    (insert form)))

(defun py-line-to-printform-python2 (&optional arg)
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
