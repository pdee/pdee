;;; python-components-extensions.el --- more editing utilities

;; Maintainer: Andreas Roehler <andreas.roehler@online.de>
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

;; Rewrite of python-mode.el started with functions
;; below, which are obsolet partly due to success on
;; the main line
;; Just kept for notice for the moment

;;; Code:

;; (unless py-mode-map
;;   (setq py-mode-map (make-sparse-keymap)))
;;
;; (define-key py-mode-map [(super s)] 'suche-settrace)
;; (define-key py-mode-map  [(super I)] 'py-indent-line)
;; (define-key py-mode-map  [(super i)] 'py-indent-forward-line)
;; (define-key py-mode-map [(control meta n)]  'py-end-of-block)
;; (define-key py-mode-map [(control meta p)] 'py-beginning-of-block)
;; (define-key py-mode-map [(control return)] 'py-newline-and-dedent)
;; (define-key py-mode-map [(super backspace)] 'py-dedent-forward-line)

(defcustom py-match-paren-mode nil
  "*Non-nil means, cursor will jump to beginning or end of a block.
This vice versa, to beginning first.
Sets `py-match-paren-key' in py-mode-map.
Customize `py-match-paren-key' which key to use. "
  :type 'boolean
  :group 'python)

(defcustom py-match-paren-key "%"
  "*String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands won't get confused (i.e., the string
should be of the form `#x...' where `x' is not a blank or a tab, and
`...' is arbitrary).  However, this string should not end in whitespace."
  :type 'string
  :group 'python)

(defcustom py-kill-empty-line t
  "*If t, py-indent-forward-line kills empty lines. "
  :type 'boolean
  :group 'python)

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
      (if (and (py-in-comment-p)(not py-indent-comments))
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
  (forward-line 1)
  (end-of-line)
  (skip-chars-backward " \t\r\n\f"))

(defun py-dedent (&optional arg)
  "Dedent line according to `py-indent-offset'.

With arg, do it that many times.
If point is between indent levels, dedent to next level.
Return indentation reached, if dedent done, nil otherwise.

Affected by `py-dedent-keep-relative-column'. "
  (interactive "*p")
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
    (if py-dedent-keep-relative-column
        (goto-char orig)
      (end-of-line)
      (skip-chars-backward " \t\r\n\f"))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-close-intern (regexp)
  "Core function, internal used only. "
  (let ((cui (ignore-errors (car (py-go-to-keyword regexp -1)))))
    (py-end-base regexp (point))
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
  (let ((erg (py-close-intern py-def-re)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-close-class ()
  "Set indent level to that of beginning of class definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py-close-intern py-class-re)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-close-clause ()
  "Set indent level to that of beginning of clause definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py-close-intern py-clause-re)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-close-block ()
  "Set indent level to that of beginning of block definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py-close-intern py-block-re)))
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
        ;; 	(define-key py-mode-map (kbd (concat "<" py-match-paren-key ">")) 'py-match-paren))
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

;; from sh-beg-end.el. Introduced here for convenience.
(unless (boundp 'empty-line-p-chars)
  (defcustom empty-line-p-chars "^[ \t\f\r]*$"
    "Empty-line-p-chars."
    :type 'regexp
    :group 'convenience))

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
                            (nth 3 (syntax-ppss))))))))))
      (when (interactive-p) (message "%s" erg))
      erg)))

(unless (functionp 'empty-line-p)
  (defun empty-line-p (&optional bound noerror count)
    "Returns t if cursor is at an empty line, nil otherwise."
    (interactive)
    (save-excursion
      (beginning-of-line)
      (when (interactive-p)
        (message "%s" (looking-at empty-line-p-chars)))
      (looking-at empty-line-p-chars))))

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

(defalias 'druck 'py-printform-insert)
(defun py-printform-insert (&optional arg)
  "Inserts a print statement out of current `(car kill-ring)' by default, inserts ARG instead if delivered. "
  (interactive "*")
  (lexical-let* ((name (string-strip (or arg (car kill-ring))))
                 (form (cond ((eq major-mode 'python-mode)
                              (concat "print \"" name ": %s \" % " name)))))
    (insert form)))

(defun py-line-to-printform-python2 (&optional arg)
  "Transforms the item on current in a print statement. "
  (interactive "*")
  (lexical-let* ((name (thing-at-point 'word))
                 (form (cond ((eq major-mode 'python-mode)
                              (concat "print \"" name ": %s \" % " name)))))
    (delete-region (line-beginning-position) (line-end-position))
    (insert form))
  (forward-line 1)
  (back-to-indentation))

(provide 'python-components-extensions)
;;; python-components-extensions.el ends here
