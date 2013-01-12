;;; python-components-up-down.el -- Searching up/downwards in buffer

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

;;; Commentary:

;;; Code:


(defun py-up-statement ()
  "Go to the beginning of next statement upwards in buffer.

Return position if statement found, nil otherwise. "
  (interactive)
  (let ((orig (point))
        erg)
  (if (py-beginning-of-statement-p)
      (setq erg (py-beginning-of-statement))
    (setq erg (and (py-beginning-of-statement) (py-beginning-of-statement))))
  (when (and py-verbose-p (interactive-p)) (message "%s" erg))
  erg))

(defun py-down-statement ()
  "Go to the beginning of next statement downwards in buffer.

Return position if statement found, nil otherwise. "
  (interactive)
  (let* ((orig (point))
         (erg 
          (cond ((py-end-of-statement-p)
                 (setq erg (and (py-end-of-statement) (py-beginning-of-statement))))
                ((ignore-errors (< orig (progn (py-end-of-statement) (py-beginning-of-statement))))
                 (point)) 
                (t (goto-char orig) (and (py-end-of-statement) (py-end-of-statement)(py-beginning-of-statement))))))
          (when (and py-verbose-p (interactive-p)) (message "%s" erg))
          erg))

(defun py-up-base (regexp &optional indent)
  "Go to the beginning of next form upwards in buffer.

With optional arg INDENT check for a required indent.
Return position if form found, nil otherwise. "
  (let* ((orig (point))
         erg)
    (if (bobp)
        (setq erg nil)
      (while (and (re-search-backward regexp nil t 1)
                  (nth 8 (syntax-ppss))))
      (back-to-indentation)
      (when (looking-at regexp) (setq erg (point)))
      (when py-verbose-p (message "%s" erg))
      erg)))

(defun py-down-base (regexp)
  "Go to the beginning of next form below in buffer.

Return position if form found, nil otherwise. "
  (unless (eobp)
    (forward-line 1)
    (beginning-of-line)
    (let* ((orig (point))
           erg)
      (if (eobp)
          (setq erg nil)
        (while (and (re-search-forward regexp nil t 1)
                    (nth 8 (syntax-ppss))))
        (back-to-indentation)
        (when (looking-at regexp) (setq erg (point)))
        (when py-verbose-p (message "%s" erg))
        erg))))

(defun py-up-base-bol (regexp)
  "Go to the beginning of next form upwards in buffer.

Return position if form found, nil otherwise. "
  (let* ((orig (point))
         erg)
    (if (bobp)
        (setq erg nil)
      (while (and (re-search-backward regexp nil t 1)
                  (nth 8 (syntax-ppss))))
      (beginning-of-line)
      (when (looking-at regexp) (setq erg (point)))
      (when py-verbose-p (message "%s" erg))
      erg)))

(defun py-down-base-bol (regexp)
  "Go to the beginning of next form below in buffer.

Return position if form found, nil otherwise. "
  (unless (eobp)
    (forward-line 1)
    (beginning-of-line)
    (let* ((orig (point))
           erg)
      (if (eobp)
          (setq erg nil)
        (while (and (re-search-forward regexp nil t 1)
                    (nth 8 (syntax-ppss))))
        (beginning-of-line)
        (when (looking-at regexp) (setq erg (point)))
        (when py-verbose-p (message "%s" erg))
        erg))))

(defun py-up-block ()
  "Go to the beginning of next block upwards in buffer.

Return position if block found, nil otherwise. "
  (interactive)
  (py-up-base py-block-re))

(defun py-up-minor-block ()
  "Go to the beginning of next minor-block upwards in buffer.

Return position if minor-block found, nil otherwise. "
  (interactive)
  (py-up-base py-minor-block-re))

(defun py-up-clause ()
  "Go to the beginning of next clause upwards in buffer.

Return position if clause found, nil otherwise. "
  (interactive)
  (py-up-base py-clause-re))

(defun py-up-block-or-clause ()
  "Go to the beginning of next block-or-clause upwards in buffer.

Return position if block-or-clause found, nil otherwise. "
  (interactive)
  (py-up-base py-block-or-clause-re))

(defun py-up-def ()
  "Go to the beginning of next def upwards in buffer.

Return position if def found, nil otherwise. "
  (interactive)
  (py-up-base py-def-re))

(defun py-up-class ()
  "Go to the beginning of next class upwards in buffer.

Return position if class found, nil otherwise. "
  (interactive)
  (py-up-base py-class-re))

(defun py-up-def-or-class ()
  "Go to the beginning of next def-or-class upwards in buffer.

Return position if def-or-class found, nil otherwise. "
  (interactive)
  (py-up-base py-def-or-class-re))

(defun py-down-block ()
  "Go to the beginning of next block below in buffer.

Return position if block found, nil otherwise. "
  (interactive)
  (py-down-base py-block-re))

(defun py-down-minor-block ()
  "Go to the beginning of next minor-block below in buffer.

Return position if minor-block found, nil otherwise. "
  (interactive)
  (py-down-base py-minor-block-re))

(defun py-down-clause ()
  "Go to the beginning of next clause below in buffer.

Return position if clause found, nil otherwise. "
  (interactive)
  (py-down-base py-clause-re))

(defun py-down-block-or-clause ()
  "Go to the beginning of next block-or-clause below in buffer.

Return position if block-or-clause found, nil otherwise. "
  (interactive)
  (py-down-base py-block-or-clause-re))

(defun py-down-def ()
  "Go to the beginning of next def below in buffer.

Return position if def found, nil otherwise. "
  (interactive)
  (py-down-base py-def-re))

(defun py-down-class ()
  "Go to the beginning of next class below in buffer.

Return position if class found, nil otherwise. "
  (interactive)
  (py-down-base py-class-re))

(defun py-down-def-or-class ()
  "Go to the beginning of next def-or-class below in buffer.

Return position if def-or-class found, nil otherwise. "
  (interactive)
  (py-down-base py-def-or-class-re))

(defun py-up-block-bol ()
  "Go to the beginning of next block upwards in buffer.

Go to beginning of line.
Return position if block found, nil otherwise. "
  (interactive)
  (py-up-base-bol py-block-re))

(defun py-up-minor-block-bol ()
  "Go to the beginning of next minor-block upwards in buffer.

Go to beginning of line.
Return position if minor-block found, nil otherwise. "
  (interactive)
  (py-up-base-bol py-minor-block-re))

(defun py-up-clause-bol ()
  "Go to the beginning of next clause upwards in buffer.

Go to beginning of line.
Return position if clause found, nil otherwise. "
  (interactive)
  (py-up-base-bol py-clause-re))

(defun py-up-block-or-clause-bol ()
  "Go to the beginning of next block-or-clause upwards in buffer.

Go to beginning of line.
Return position if block-or-clause found, nil otherwise. "
  (interactive)
  (py-up-base-bol py-block-or-clause-re))

(defun py-up-def-bol ()
  "Go to the beginning of next def upwards in buffer.

Go to beginning of line.
Return position if def found, nil otherwise. "
  (interactive)
  (py-up-base-bol py-def-re))

(defun py-up-class-bol ()
  "Go to the beginning of next class upwards in buffer.

Go to beginning of line.
Return position if class found, nil otherwise. "
  (interactive)
  (py-up-base-bol py-class-re))

(defun py-up-def-or-class-bol ()
  "Go to the beginning of next def-or-class upwards in buffer.

Go to beginning of line.
Return position if def-or-class found, nil otherwise. "
  (interactive)
  (py-up-base-bol py-def-or-class-re))

(defun py-down-block-bol ()
  "Go to the beginning of next block below in buffer.

Go to beginning of line
Return position if block found, nil otherwise "
  (interactive)
  (py-down-base-bol py-block-re))

(defun py-down-minor-block-bol ()
  "Go to the beginning of next minor-block below in buffer.

Go to beginning of line
Return position if minor-block found, nil otherwise "
  (interactive)
  (py-down-base-bol py-minor-block-re))

(defun py-down-clause-bol ()
  "Go to the beginning of next clause below in buffer.

Go to beginning of line
Return position if clause found, nil otherwise "
  (interactive)
  (py-down-base-bol py-clause-re))

(defun py-down-block-or-clause-bol ()
  "Go to the beginning of next block-or-clause below in buffer.

Go to beginning of line
Return position if block-or-clause found, nil otherwise "
  (interactive)
  (py-down-base-bol py-block-or-clause-re))

(defun py-down-def-bol ()
  "Go to the beginning of next def below in buffer.

Go to beginning of line
Return position if def found, nil otherwise "
  (interactive)
  (py-down-base-bol py-def-re))

(defun py-down-class-bol ()
  "Go to the beginning of next class below in buffer.

Go to beginning of line
Return position if class found, nil otherwise "
  (interactive)
  (py-down-base-bol py-class-re))

(defun py-down-def-or-class-bol ()
  "Go to the beginning of next def-or-class below in buffer.

Go to beginning of line
Return position if def-or-class found, nil otherwise "
  (interactive)
  (py-down-base-bol py-def-or-class-re))

;; python-components-up-down ends here
(provide 'python-components-up-down)
