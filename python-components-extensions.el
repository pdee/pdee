;;; python-components-extensions.el --- more editing utilities -*- lexical-binding: t; -*-

;; URL: https://gitlab.com/python-mode-devs

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
  "Indent and move line forward to next indentation.
Returns column of line reached.

If ‘py-kill-empty-line’ is non-nil, delete an empty line.

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
            (when (and py-kill-empty-line (py-empty-line-p) (not (looking-at "[ \t]*\n[[:alpha:]]")) (not (eobp)))
              (delete-region (line-beginning-position) (line-end-position)))))))
    (back-to-indentation)
    (when (or (eq 4 (prefix-numeric-value arg)) (< orig (point))) (setq erg (current-column)))
    erg))

(defun py-dedent-forward-line (&optional arg)
  "Dedent line and move one line forward. "
  (interactive "*p")
  (py-dedent arg)
  (if (eobp)
      (newline 1)
    (forward-line 1))
  (end-of-line))

(defun py-dedent (&optional arg)
  "Dedent line according to ‘py-indent-offset’.

With arg, do it that many times.
If point is between indent levels, dedent to next level.
Return indentation reached, if dedent done, nil otherwise.

Affected by ‘py-dedent-keep-relative-column’. "
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
    erg))

(defun py-class-at-point ()
  "Return class definition as string. "
  (interactive)
  (save-excursion
    (let* ((beg (py-backward-class))
           (end (py-forward-class))
           (res (when (and (numberp beg)(numberp end)(< beg end)) (buffer-substring-no-properties beg end))))
      res)))

(defun py-backward-function ()
  "Jump to the beginning of defun.

Returns position. "
  (interactive "p")
  (py-backward-def-or-class))

(defun py-forward-function ()
  "Jump to the end of function.

Returns position."
  (interactive "p")
  (py-forward-def-or-class))

(defun py-function-at-point ()
  "Return functions definition as string. "
  (interactive)
  (save-excursion
    (let* ((beg (py-backward-function))
           (end (py-forward-function)))
      (when (and (numberp beg)(numberp end)(< beg end)) (buffer-substring-no-properties beg end)))))

;; Functions for marking regions

(defun py-line-at-point ()
  "Return line as string. "
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (line-end-position)))
    (when (and (numberp beg)(numberp end)(< beg end)) (buffer-substring-no-properties beg end))))

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
              (unless (py-empty-line-p) (split-line))
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
      (when (py-empty-line-p) (delete-region (line-beginning-position) (point)))
      (py-backward-statement)
      (unless (< (current-column) cuc)
      (while (and (not (bobp))
                  (< cui (current-column))
                  (py-backward-statement)))))))

(defun py--match-paren-blocks ()
  (cond
   ((and (looking-back "^[ \t]*" (line-beginning-position))(if (eq last-command (quote py-match-paren))(not py--match-paren-forward-p)t)
         ;; (looking-at py-extended-block-or-clause-re)
         (looking-at "[[:alpha:]_]"))
    ;; from beginning of top-level, block, clause, statement
    (py--match-paren-forward))
   (t
    (py--match-paren-backward))))

(defun py-match-paren (&optional arg)
  "If at a beginning, jump to end and vice versa.

When called from within, go to the start.
Matches lists, but also block, statement, string and comment. "
  (interactive "*P")
  (if (eq 4 (prefix-numeric-value arg))
      (insert "%")
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
        (py--match-paren-blocks))))))

(unless (functionp (quote in-string-p))
  (defun in-string-p (&optional pos)
    (interactive)
    (let ((orig (or pos (point))))
      (save-excursion
        (save-restriction
          (widen)
          (beginning-of-defun)
          (numberp
           (progn
             (if (featurep (quote xemacs))
                 (nth 3 (parse-partial-sexp (point) orig)
                      (nth 3 (parse-partial-sexp (point-min) (point))))))))))))

(defun py-documentation (w)
  "Launch PyDOC on the Word at Point"
  (interactive
   (list (let* ((word (py-symbol-at-point))
                (input (read-string
                        (format "pydoc entry%s: "
                                (if (not word) "" (format " (default %s)" word))))))
           (if (string= input "")
               (if (not word) (error "No pydoc args given")
                 word) ;sinon word
             input)))) ;sinon input
  (shell-command (concat py-shell-name " -c \"from pydoc import help;help('" w "')\"") "*PYDOCS*")
  (view-buffer-other-window "*PYDOCS*" t 'kill-buffer-and-window))

(defun pst-here ()
  "Kill previous \"pdb.set_trace()\" and insert it at point. "
  (interactive "*")
  (let ((orig (copy-marker (point))))
    (search-backward "pdb.set_trace()")
    (replace-match "")
    (when (py-empty-line-p)
      (delete-region (line-beginning-position) (line-end-position)))
    (goto-char orig)
    (insert "pdb.set_trace()")))

(defun py-printform-insert (&optional arg strg)
  "Inserts a print statement from `(car kill-ring)'.

With optional \\[universal-argument] print as string"
  (interactive "*P")
  (let* ((name (py--string-strip (or strg (car kill-ring))))
         ;; guess if doublequotes or parentheses are needed
         (numbered (not (eq 4 (prefix-numeric-value arg))))
         (form (if numbered
                   (concat "print(\"" name ": %s \" % (" name "))")
                 (concat "print(\"" name ": %s \" % \"" name "\")"))))
    (insert form)))

(defun py-print-formatform-insert (&optional strg)
  "Inserts a print statement out of current `(car kill-ring)' by default.

print(\"\\nfoo: {}\"\.format(foo))"
  (interactive "*")
  (let ((name (py--string-strip (or strg (car kill-ring)))))
    (insert (concat "print(\"" name ": {}\\n\".format(" name "))"))))

(defun py-line-to-printform-python2 ()
  "Transforms the item on current in a print statement. "
  (interactive "*")
  (let* ((name (py-symbol-at-point))
         (form (concat "print(\"" name ": %s \" % " name ")")))
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
          (t (message "%s" "Can not see \"True or False\" here")))))

(provide 'python-components-extensions)
;;; python-components-extensions.el ends here
