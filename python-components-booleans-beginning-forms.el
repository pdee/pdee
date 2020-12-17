;;; python-components-booleans-beginning-forms.el --- booleans-beginning forms -*- lexical-binding: t; -*-

;;This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

;; Copyright (C) 2015-2020 Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>
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

;; This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

;;; Code:

(defun py--beginning-of-comment-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘comment’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-comment-re)
         (point))))

(defun py--beginning-of-expression-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘expression’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-expression-re)
         (point))))

(defun py--beginning-of-line-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘line’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-line-re)
         (point))))

(defun py--beginning-of-paragraph-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘paragraph’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-paragraph-re)
         (point))))

(defun py--beginning-of-partial-expression-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘partial-expression’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-partial-expression-re)
         (point))))

(defun py--beginning-of-section-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘section’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-section-re)
         (point))))

(defun py--beginning-of-top-level-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘top-level’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-top-level-re)
         (point))))

(defun py--beginning-of-assignment-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘assignment’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-assignment-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-block-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-block-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-block-or-clause-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘block-or-clause’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-block-or-clause-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-class-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘class’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-class-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-clause-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘clause’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-clause-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-def-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘def’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-def-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-def-or-class-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘def-or-class’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-def-or-class-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-elif-block-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘elif-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-elif-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-else-block-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘else-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-else-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-except-block-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘except-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-except-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-for-block-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘for-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-for-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-if-block-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘if-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-if-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-indent-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘indent’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-indent-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-minor-block-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘minor-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-minor-block-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-statement-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘statement’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-statement-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-try-block-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘try-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-try-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-assignment-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘assignment’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-assignment-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-block-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-block-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-block-or-clause-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘block-or-clause’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-block-or-clause-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-class-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘class’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-class-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-clause-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘clause’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-clause-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-def-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘def’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-def-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-def-or-class-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘def-or-class’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-def-or-class-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-elif-block-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘elif-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-elif-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-else-block-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘else-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-else-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-except-block-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘except-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-except-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-for-block-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘for-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-for-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-if-block-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘if-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-if-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-indent-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘indent’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-indent-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-minor-block-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘minor-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-minor-block-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-statement-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘statement’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-statement-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-try-block-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘try-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-try-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(provide 'python-components-booleans-beginning-forms)
;; python-components-booleans-beginning-forms.el ends here
