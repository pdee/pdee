;;; python-components-re-forms.el --- Forms start described by a regular-expression

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages, convenience

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

;;; Code:

;;; Beg-end forms
(defun py-beginning-of-block (&optional indent)
  "Returns beginning of block if successful, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-block-re indent)))))
    erg))

(defun py-end-of-block ()
  "Go to the end of block.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let* ((orig (point))
         (erg (py-end-base py-block-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-beginning-of-clause (&optional indent)
  "Returns beginning of clause if successful, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-clause-re indent)))))
    erg))

(defun py-end-of-clause ()
  "Go to the end of clause.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let* ((orig (point))
         (erg (py-end-base py-clause-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-beginning-of-block-or-clause (&optional indent)
  "Returns beginning of block-or-clause if successful, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-block-or-clause-re indent)))))
    erg))

(defun py-end-of-block-or-clause ()
  "Go to the end of block-or-clause.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let* ((orig (point))
         (erg (py-end-base py-block-or-clause-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-beginning-of-def (&optional indent)
  "Returns beginning of def if successful, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-def-re indent)))))
    erg))

(defun py-end-of-def ()
  "Go to the end of def.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let* ((orig (point))
         (erg (py-end-base py-def-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-beginning-of-class (&optional indent)
  "Returns beginning of class if successful, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-class-re indent)))))
    erg))

(defun py-end-of-class ()
  "Go to the end of class.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let* ((orig (point))
         (erg (py-end-base py-class-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-beginning-of-def-or-class (&optional indent)
  "Returns beginning of def-or-class if successful, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-def-or-class-re indent)))))
    erg))

(defun py-end-of-def-or-class ()
  "Go to the end of def-or-class.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (save-restriction
    (widen)
    (let* ((orig (point))
           (pps (syntax-ppss))
           ;; (origline (py-count-lines))
           (erg (if (and (not (nth 8 pps)) (looking-at py-def-or-class-re))
                    (point)
                  (py-go-to-keyword py-def-or-class-re)
                  (when (and (not (nth 8 pps)) (looking-at py-def-or-class-re)) (point))))
           ind)
      (if erg
          (progn
            (setq ind
                  (+ (if py-smart-indentation
                         (save-excursion
                           (goto-char orig)
                           ;; (setq origline (py-count-lines))
                           (py-end-of-statement)
                           (py-end-of-statement)
                           ;; (when (eq origline (py-count-lines)) (py-end-of-statement))
                           (py-guess-indent-offset nil (point)))
                       py-indent-offset)
                     (current-indentation)))
            (py-end-of-statement)
            (forward-line 1)
            (setq erg (py-travel-current-indent ind)))
        (py-look-downward-for-beginning py-def-or-class-re)
        (unless (eobp)
          ;; (py-end-base py-def-or-class-re orig)
          (progn
            (setq ind
                  (+ (if py-smart-indentation
                         (save-excursion
                           (goto-char orig)
                           ;; (setq origline (py-count-lines))
                           (py-end-of-statement)
                           (py-end-of-statement)
                           ;; (when (eq origline (py-count-lines)) (py-end-of-statement))
                           (py-guess-indent-offset nil (point)))
                       py-indent-offset)
                     (current-indentation)))
            (py-end-of-statement)
            (forward-line 1)
            (setq erg (py-travel-current-indent ind)))
          ))
      (if (< orig (point))
          (setq erg (point))
        (setq erg (py-look-downward-for-beginning py-def-or-class-re))
        (when erg
          (progn
            (setq ind (+ py-indent-offset (current-indentation)))
            (py-end-of-statement)
            (forward-line 1)
            (setq erg (py-travel-current-indent ind)))
          ;; (py-end-base py-def-or-class-re orig)
          ))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-beginning-of-if-block (&optional indent)
  "Returns beginning of if-block if successful, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-if-re indent)))))
    erg))

(defun py-end-of-if-block ()
  "Go to the end of if-block.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let* ((orig (point))
         (erg (py-end-base py-if-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-beginning-of-try-block (&optional indent)
  "Returns beginning of try-block if successful, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-try-block-re indent)))))
    erg))

(defun py-end-of-try-block ()
  "Go to the end of try-block.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let* ((orig (point))
         (erg (py-end-base py-try-block-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-beginning-of-minor-block (&optional indent)
  "Returns beginning of minor-block if successful, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-minor-block-re indent)))))
    erg))

(defun py-end-of-minor-block ()
  "Go to the end of minor-block.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let* ((orig (point))
         (erg (py-end-base py-minor-block-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

;; Buffer
(defun py-beginning-of-buffer ()
  "Go to beginning-of-buffer, return position. "
  (let ((erg (unless (bobp)
               (goto-char (point-min)))))
    erg))

(defun py-end-of-buffer ()
  "Go to end-of-buffer, return position.

If already at end-of-buffer and not at EOB, go to end of next line. "
  (let ((erg (unless (eobp)
               (goto-char (point-max)))))
    erg))

(defalias 'py-forward-block 'py-end-of-block)
(defalias 'py-forward-block-or-clause 'py-end-of-block-or-clause)
(defalias 'py-forward-class 'py-end-of-class)
(defalias 'py-forward-clause 'py-end-of-clause)
(defalias 'end-of-def-or-class 'py-end-of-def-or-class)
(defalias 'py-forward-def-or-class 'py-end-of-def-or-class)
(defalias 'py-previous-block 'py-beginning-of-block)
(defalias 'py-goto-block-up 'py-beginning-of-block)
(defalias 'py-backward-block 'py-beginning-of-block)
(defalias 'py-previous-block-or-clause 'py-beginning-of-block-or-clause)
(defalias 'py-goto-block-or-clause-up 'py-beginning-of-block-or-clause)
(defalias 'py-backward-block-or-clause 'py-beginning-of-block-or-clause)
(defalias 'beginning-of-class 'py-beginning-of-class)
(defalias 'py-backward-class 'py-beginning-of-class)
(defalias 'py-previous-class 'py-beginning-of-class)
(defalias 'py-previous-clause 'py-beginning-of-clause)
(defalias 'py-goto-clause-up 'py-beginning-of-clause)
(defalias 'py-backward-clause 'py-beginning-of-clause)
(defalias 'py-backward-def-or-class 'py-beginning-of-def-or-class)
(defalias 'py-previous-def-or-class 'py-beginning-of-def-or-class)

(provide 'python-components-re-forms)
;;; python-components-re-forms.el ends here
