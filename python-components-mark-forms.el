;;; python-components-mark-forms.el --- mark forms -*- lexical-binding: t; -*-

;;This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

;; Copyright (C) 2015-2016 Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>
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

;; This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

;;; Code:


(defun py-mark-comment ()
  "Mark comment at point.

Return beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py--mark-base "comment"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-line ()
  "Mark line at point.

Return beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py--mark-base "line"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-paragraph ()
  "Mark paragraph at point.

Return beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py--mark-base "paragraph"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-expression ()
  "Mark expression at point.

Return beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py--mark-base "expression"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-partial-expression ()
  "Mark partial-expression at point.

Return beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py--mark-base "partial-expression"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-section ()
  "Mark section at point.

Return beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py--mark-base "section"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-top-level ()
  "Mark top-level at point.

Return beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py--mark-base "top-level"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-block ()
  "Mark block, take beginning of line positions. 

Return beginning and end positions of region, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py--mark-base-bol "block"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-block-or-clause ()
  "Mark block-or-clause, take beginning of line positions. 

Return beginning and end positions of region, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py--mark-base-bol "block-or-clause"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-class (&optional arg)
  "Mark class, take beginning of line positions. 

With \\[universal-argument] or `py-mark-decorators' set to `t', decorators are marked too.
Return beginning and end positions of region, a cons. "
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py--mark-base-bol "class" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-clause ()
  "Mark clause, take beginning of line positions. 

Return beginning and end positions of region, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py--mark-base-bol "clause"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-def (&optional arg)
  "Mark def, take beginning of line positions. 

With \\[universal-argument] or `py-mark-decorators' set to `t', decorators are marked too.
Return beginning and end positions of region, a cons. "
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py--mark-base-bol "def" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-def-or-class (&optional arg)
  "Mark def-or-class, take beginning of line positions. 

With \\[universal-argument] or `py-mark-decorators' set to `t', decorators are marked too.
Return beginning and end positions of region, a cons. "
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py--mark-base-bol "def-or-class" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-elif-block ()
  "Mark elif-block, take beginning of line positions. 

Return beginning and end positions of region, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py--mark-base-bol "elif-block"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-else-block ()
  "Mark else-block, take beginning of line positions. 

Return beginning and end positions of region, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py--mark-base-bol "else-block"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-except-block ()
  "Mark except-block, take beginning of line positions. 

Return beginning and end positions of region, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py--mark-base-bol "except-block"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-for-block ()
  "Mark for-block, take beginning of line positions. 

Return beginning and end positions of region, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py--mark-base-bol "for-block"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-if-block ()
  "Mark if-block, take beginning of line positions. 

Return beginning and end positions of region, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py--mark-base-bol "if-block"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-indent ()
  "Mark indent, take beginning of line positions. 

Return beginning and end positions of region, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py--mark-base-bol "indent"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-minor-block ()
  "Mark minor-block, take beginning of line positions. 

Return beginning and end positions of region, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py--mark-base-bol "minor-block"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-statement ()
  "Mark statement, take beginning of line positions. 

Return beginning and end positions of region, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py--mark-base-bol "statement"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-mark-try-block ()
  "Mark try-block, take beginning of line positions. 

Return beginning and end positions of region, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py--mark-base-bol "try-block"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(provide 'python-components-mark-forms)
;;; python-components-mark-forms.el ends here
