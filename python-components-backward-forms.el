;;; python-components-backward-forms.el --- Go to beginning of form or further backward -*- lexical-binding: t; -*-


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

(defun py-backward-region ()
  "Go to the beginning of current region."
  (interactive)
  (let ((beg (region-beginning)))
    (when beg (goto-char beg))))

(defun py-backward-block ()
  "Go to beginning of `block'.

If already at beginning, go one `block' backward.
Return position if successful, nil otherwise"
  (interactive)
  (let (erg)
    (setq erg (py--go-to-keyword 'py-block-re '<))
    (when py-mark-decorators
      (and (py-backward-decorator) (setq erg (point))))
    erg))

(defun py-backward-class ()
  "Go to beginning of `class'.

If already at beginning, go one `class' backward.
Return position if successful, nil otherwise"
  (interactive)
  (let (erg)
    (setq erg (py--go-to-keyword 'py-class-re '<))
    (when py-mark-decorators
      (and (py-backward-decorator) (setq erg (point))))
    erg))

(defun py-backward-def ()
  "Go to beginning of `def'.

If already at beginning, go one `def' backward.
Return position if successful, nil otherwise"
  (interactive)
  (let (erg)
    (setq erg (py--go-to-keyword 'py-def-re '<))
    (when py-mark-decorators
      (and (py-backward-decorator) (setq erg (point))))
    erg))

(defun py-backward-def-or-class ()
  "Go to beginning of `def-or-class'.

If already at beginning, go one `def-or-class' backward.
Return position if successful, nil otherwise"
  (interactive)
  (let (erg)
    (setq erg (py--go-to-keyword 'py-def-or-class-re '<))
    (when py-mark-decorators
      (and (py-backward-decorator) (setq erg (point))))
    erg))

(defun py-backward-block-bol ()
  "Go to beginning of `block', go to BOL.
If already at beginning, go one `block' backward.
Return beginning of `block' if successful, nil otherwise"
  (interactive)
  (and (py-backward-block)
       (progn (beginning-of-line)(point))))

(defun py-backward-class-bol ()
  "Go to beginning of `class', go to BOL.
If already at beginning, go one `class' backward.
Return beginning of `class' if successful, nil otherwise"
  (interactive)
  (and (py-backward-class)
       (progn (beginning-of-line)(point))))

(defun py-backward-def-bol ()
  "Go to beginning of `def', go to BOL.
If already at beginning, go one `def' backward.
Return beginning of `def' if successful, nil otherwise"
  (interactive)
  (and (py-backward-def)
       (progn (beginning-of-line)(point))))

(defun py-backward-def-or-class-bol ()
  "Go to beginning of `def-or-class', go to BOL.
If already at beginning, go one `def-or-class' backward.
Return beginning of `def-or-class' if successful, nil otherwise"
  (interactive)
  (and (py-backward-def-or-class)
       (progn (beginning-of-line)(point))))

(defun py-backward-assignment ()
  "Go to beginning of `assignment'.

If already at beginning, go one `assignment' backward.
Return position if successful, nil otherwise"
  (interactive)
  (py--go-to-keyword 'py-assignment-re '<))

(defun py-backward-block-or-clause ()
  "Go to beginning of `block-or-clause'.

If already at beginning, go one `block-or-clause' backward.
Return position if successful, nil otherwise"
  (interactive)
  (py--go-to-keyword 'py-block-or-clause-re '<))

(defun py-backward-clause ()
  "Go to beginning of `clause'.

If already at beginning, go one `clause' backward.
Return position if successful, nil otherwise"
  (interactive)
  (py--go-to-keyword 'py-clause-re '<))

(defun py-backward-elif-block ()
  "Go to beginning of `elif-block'.

If already at beginning, go one `elif-block' backward.
Return position if successful, nil otherwise"
  (interactive)
  (py--go-to-keyword 'py-elif-re '<))

(defun py-backward-else-block ()
  "Go to beginning of `else-block'.

If already at beginning, go one `else-block' backward.
Return position if successful, nil otherwise"
  (interactive)
  (py--go-to-keyword 'py-else-re '<))

(defun py-backward-except-block ()
  "Go to beginning of `except-block'.

If already at beginning, go one `except-block' backward.
Return position if successful, nil otherwise"
  (interactive)
  (py--go-to-keyword 'py-except-re '<))

(defun py-backward-for-block ()
  "Go to beginning of `for-block'.

If already at beginning, go one `for-block' backward.
Return position if successful, nil otherwise"
  (interactive)
  (py--go-to-keyword 'py-for-re '<))

(defun py-backward-if-block ()
  "Go to beginning of `if-block'.

If already at beginning, go one `if-block' backward.
Return position if successful, nil otherwise"
  (interactive)
  (py--go-to-keyword 'py-if-re '<))

(defun py-backward-minor-block ()
  "Go to beginning of `minor-block'.

If already at beginning, go one `minor-block' backward.
Return position if successful, nil otherwise"
  (interactive)
  (py--go-to-keyword 'py-minor-block-re '<))

(defun py-backward-try-block ()
  "Go to beginning of `try-block'.

If already at beginning, go one `try-block' backward.
Return position if successful, nil otherwise"
  (interactive)
  (py--go-to-keyword 'py-try-re '<))

(defun py-backward-assignment-bol ()
  "Go to beginning of `assignment', go to BOL.
If already at beginning, go one `assignment' backward.
Return beginning of `assignment' if successful, nil otherwise"
  (interactive)
  (and (py-backward-assignment)
       (progn (beginning-of-line)(point))))

(defun py-backward-block-or-clause-bol ()
  "Go to beginning of `block-or-clause', go to BOL.
If already at beginning, go one `block-or-clause' backward.
Return beginning of `block-or-clause' if successful, nil otherwise"
  (interactive)
  (and (py-backward-block-or-clause)
       (progn (beginning-of-line)(point))))

(defun py-backward-clause-bol ()
  "Go to beginning of `clause', go to BOL.
If already at beginning, go one `clause' backward.
Return beginning of `clause' if successful, nil otherwise"
  (interactive)
  (and (py-backward-clause)
       (progn (beginning-of-line)(point))))

(defun py-backward-elif-block-bol ()
  "Go to beginning of `elif-block', go to BOL.
If already at beginning, go one `elif-block' backward.
Return beginning of `elif-block' if successful, nil otherwise"
  (interactive)
  (and (py-backward-elif-block)
       (progn (beginning-of-line)(point))))

(defun py-backward-else-block-bol ()
  "Go to beginning of `else-block', go to BOL.
If already at beginning, go one `else-block' backward.
Return beginning of `else-block' if successful, nil otherwise"
  (interactive)
  (and (py-backward-else-block)
       (progn (beginning-of-line)(point))))

(defun py-backward-except-block-bol ()
  "Go to beginning of `except-block', go to BOL.
If already at beginning, go one `except-block' backward.
Return beginning of `except-block' if successful, nil otherwise"
  (interactive)
  (and (py-backward-except-block)
       (progn (beginning-of-line)(point))))

(defun py-backward-for-block-bol ()
  "Go to beginning of `for-block', go to BOL.
If already at beginning, go one `for-block' backward.
Return beginning of `for-block' if successful, nil otherwise"
  (interactive)
  (and (py-backward-for-block)
       (progn (beginning-of-line)(point))))

(defun py-backward-if-block-bol ()
  "Go to beginning of `if-block', go to BOL.
If already at beginning, go one `if-block' backward.
Return beginning of `if-block' if successful, nil otherwise"
  (interactive)
  (and (py-backward-if-block)
       (progn (beginning-of-line)(point))))

(defun py-backward-minor-block-bol ()
  "Go to beginning of `minor-block', go to BOL.
If already at beginning, go one `minor-block' backward.
Return beginning of `minor-block' if successful, nil otherwise"
  (interactive)
  (and (py-backward-minor-block)
       (progn (beginning-of-line)(point))))

(defun py-backward-try-block-bol ()
  "Go to beginning of `try-block', go to BOL.
If already at beginning, go one `try-block' backward.
Return beginning of `try-block' if successful, nil otherwise"
  (interactive)
  (and (py-backward-try-block)
       (progn (beginning-of-line)(point))))

(provide 'python-components-backward-forms)
;;; python-components-backward-forms.el ends here
