;;; python-components-abbrev-propose.el --- initial content for mode-abbrevs -*- lexical-binding: t; -*- 

;; URL: https://gitlab.com/python-mode-devs

;; Keywords: abbrev

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; A slightly modified ‘add-mode-abbrev’ providing a
;; proposal for new abbrevs. Proposal is composed from
;; the initial character(s) of the expansion.

;;; Code:

(defun py-edit-abbrevs ()
  "Jumps to ‘python-mode-abbrev-table’."
  (interactive)
  (save-excursion
    (let ((mat (abbrev-table-name local-abbrev-table)))
      (prepare-abbrev-list-buffer)
      (set-buffer "*Abbrevs*")
      (switch-to-buffer (current-buffer))
      (goto-char (point-min))
      (search-forward (concat "(" (format "%s" mat))))))

(defun py--add-abbrev-propose (table type arg &optional dont-ask)
  (save-excursion
    (let ((orig (point))
          proposal exp name)
      (while (< 0 arg)
        (py-backward-partial-expression)
        (when (looking-at "[[:alpha:]]")
          (setq proposal (concat (downcase (match-string-no-properties 0)) proposal)))
        (setq arg (1- arg)))
      (setq exp (buffer-substring-no-properties (point) orig))
      (setq name
            ;; ask only when interactive
            (if dont-ask
                proposal
              (read-string (format (if exp "%s abbrev for \"%s\": "
                                     "Undefine %s abbrev: ")
                                   type exp) proposal)))
      (set-text-properties 0 (length name) nil name)
      (when (or (null exp)
                (not (abbrev-expansion name table))
                (y-or-n-p (format "%s expands to \"%s\"; redefine? "
                                  name (abbrev-expansion name table))))
        (define-abbrev table (downcase name) exp)))))

(defun py-add-abbrev (arg)
  "Defines python-mode specific abbrev."
  (interactive "p")
  (save-excursion
    (py--add-abbrev-propose
     (if only-global-abbrevs
         global-abbrev-table
       (or local-abbrev-table
           (error "No per-mode abbrev table")))
     "Mode" arg)))

(provide 'python-components-abbrev-propose)
;;; python-components-abbrev-propose.el ends here
