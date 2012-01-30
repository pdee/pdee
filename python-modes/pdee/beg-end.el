;;; beg-end.el

;; Author: Andreas Roehler
;; <andreas.roehler@online.de>

;; Keywords: languages

;; This file is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software
;; Foundation; either version 2, or (at your option)
;; any later version.
;; This file is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the
;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;; You should have received a copy of the GNU General
;; Public License along with GNU Emacs; see the file
;; COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Major changes to previous version:
;;  Checks for inside comment deactivated, circle-call-bugs

;;; Commentary:

;; Routine detecting nested forms - for all kind of
;; things which have a start- and a end-string.
;; End-string might be the empty string.  Jump to the
;; beginning or end of a form.  Repeated execution
;; forward or backward.  With argument as many times.

;; FixMe: `ar-in-comment-p-atpt' uses
;; thingatpt-utils-base while required from

(require 'misc-utils)

;;; Code:

;; (setq begstr "\\bcase\\b\\|\\bfor\\b\\|\\bfunction\\b\\|\\bif\\b\\|\\bselect\\b\\|\\buntil\\b\\|\\bwhile\\b")
;; (setq endstr "\\bdone\\b\\|\\besac\\b\\|\\bfi\\b")

;; (defun set-beginning-of-form ()
;;   "Set `beginning-of-form' as `beginning-of-defun-function'."

;; (defun set-end-of-form ()
;;   "Set `end-of-form' as `end-of-defun-function'."
;;   (interactive)
;;   (set (make-local-variable 'end-of-defun-function) 'end-of-form))

(defun mark-form (begstr endstr &optional bound noerror count comment)
  (beginning-of-form-base begstr endstr bound noerror count comment)
  (push-mark (point) t t)
  (end-of-form-base begstr endstr bound noerror count comment)
  (kill-new (buffer-substring-no-properties (mark) (point))))

(defun kill-form (begstr endstr &optional bound noerror count comment)
  (beginning-of-form-base begstr endstr bound noerror count comment)
  (push-mark (point) t t)
  (end-of-form-base begstr endstr bound noerror count comment)
  (kill-region (mark) (point)))

(defun beg-end-match-paren-base (begstr endstr &optional iact)
  "Switch between opening and closing programming constructs.
If not at the beginning or the end, to opening first. "
  (cond ((ignore-errors (looking-at begstr))
	 (end-of-form))
	((progn (backward-word)
		(ignore-errors (looking-at endstr)))
	 (beginning-of-form)
	 (when iact (message "%s" (point))) (point))
	(t (beginning-of-form)
	   (when iact (message "%s" (point))) (point))))

(defun beginning-of-form (&optional bound noerror count comment)
  "Goto opening of a programming structure in this level.
Reads strings as arguments from minibuffer.
 Don't use this from a program, use `beginning-of-form-base' instead. "
  (interactive)
  (let ((begstr (read-from-minibuffer "Insert start: "))
	(endstr (read-from-minibuffer "Insert endstr: ")))
    (beginning-of-form-base begstr endstr bound noerror count comment))
  (when (interactive-p) (message "%s" (point))) (point))

(defun end-of-form (&optional iact bound noerror count comment)
  "Goto opening of a programming structure in this level.
Reads strings as arguments from minibuffer.
Set comment to `t' if forms inside comments should match - also for processing comments itself.
 Don't use this from a program, use `end-of-form-base' instead. "
  (interactive "p")
  (let ((begstr (read-from-minibuffer "Insert start: "))
	(endstr (read-from-minibuffer "Insert endstr: ")))
    (end-of-form-base begstr endstr bound noerror count comment))
  (when iact (message "%s" (point))) (point))

(defun ar-leave-begstr-backward (begstr unquoted-beg)
  (let* ((stringcount (length unquoted-beg))
         (collected (char-to-string (char-after)))
         (indx (string-match collected unquoted-beg)))
    (while (and indx (not (ignore-errors (looking-at begstr)))(< 1 stringcount))
      (forward-char -1)
      (setq collected (concat (char-to-string (char-after)) collected))
      (setq indx (string-match collected unquoted-beg))
      (setq stringcount (1- stringcount)))))

(defun ar-leave-endstr-backward (endstr unquoted-end)
  (let* ((stringcount (length unquoted-end))
         (collected (char-to-string (char-after)))
         (indx (string-match collected unquoted-end)))
    (while (and indx (not (ignore-errors (looking-at endstr)))(< 1 stringcount))
      (forward-char -1)
      (setq collected (concat (char-to-string (char-after)) collected))
      (setq indx (string-match collected unquoted-end))
      (setq stringcount (1- stringcount)))))

(defun ar-escaped (&optional iact)
  "Return t if char is preceded by an odd number of backslashes. "
  (interactive "p")
  (let ((orig (point))
        erg)
    (goto-char (match-beginning 0))
    (setq erg (< 0 (abs (% (skip-chars-backward "\\\\")2))))
    (goto-char orig)
    (when iact (message "%s" erg))
    erg))

(defun ar-syntax (&optional arg)
  "Return t if char meant as syntax-symbol. "
  (interactive "p")
  (let ((orig (point))
        erg)
    (goto-char (match-beginning 0))
    (setq erg (looking-back "\\\\s"))
    (goto-char orig)
    (when arg (message "%s" erg))
    erg))

;; should `parse-sexp-ignore-comments' be used?
(defun beginning-of-form-base (begstr &optional endstr bound noerror count comment regexp condition string)
  "Goto opening of a programming structure in this level.
Takes strings as arguments for search.
Set comment to `t' if forms inside comments should match - also for processing comments itself.
Set 7th argument REGEXP t, if beg/end-str are regular expressions.
"
  (let* ((searchform (cond ((and (string= begstr endstr))
                            begstr)
                           ((and begstr endstr)
                            (progn
                              (setq regexp t)
                              (concat begstr "\\|" endstr)))
                           (t begstr)))
         (count (or count 1))
         (nesting count)
         (orig (point))
         beg-pos-delimiter end-pos-delimiter)
    (while
        (and
         (< 0 nesting) (not (bobp)))
      (cond
       ((and (looking-back searchform)
             (goto-char (match-beginning 0)))
        (beginning-of-form-base-intern begstr endstr comment string))
       ((and (or regexp (and begstr endstr))
             (re-search-backward searchform bound noerror count))
        (beginning-of-form-base-intern begstr endstr comment string))
       ((and (not regexp) (not (and begstr endstr))
             (search-backward searchform bound noerror count)
             (goto-char (match-beginning 0)))
        (beginning-of-form-base-intern begstr endstr comment string))
       (t (goto-char (point-min)))))
    (when (and beg-pos-delimiter end-pos-delimiter)
      (list beg-pos-delimiter end-pos-delimiter))))

(defun beginning-of-form-base-intern (begstr endstr comment string)
  (let ((pps (parse-partial-sexp (point-min) (point))))
    ;; in string
    (if (and (not string) (nth 3 pps)(nth 8 pps))
        (goto-char (nth 8 pps))
      (unless (save-match-data (and condition (funcall condition)))
        (save-match-data
          (if
              (ignore-errors (string-match  begstr (match-string-no-properties 0)))
              (if comment
                  (setq nesting (1- nesting))
                (unless (ar-in-comment-p)
                  (setq nesting (1- nesting))))
            (when
                (ignore-errors (string-match endstr (match-string-no-properties 0))))
            (if comment
                ;; non-nesting comments don't matter
                (unless (or (string= "" comment-end)(eq 10 comment-end))
                  (setq nesting (1+ nesting)))
              (unless (ar-in-comment-p)
                (setq nesting (1+ nesting))))))
        (setq beg-pos-delimiter (match-beginning 0))
        (setq end-pos-delimiter (match-end 0))))))

(defun end-of-form-base (begstr endstr &optional bound noerror count comment regexp condition string)
  "Goto closing of a programming structure in this level.
As it stops one char after form, go one char back onto the last char of form.
Set comment to `t' if forms inside comments should match - also for processing comments itself.
If SHOW, display nesting and point in message buffer.
Set 7th argument REGEXP t, if beg/end-strings are regular expressions.
Optional arg CONDITION expects a function whose return value - `t' or a number - is checked for success, otherwise search continues.
"
  (let* ((searchform (cond ((string= begstr endstr)
                            endstr)
                           ((and begstr endstr)
                            (progn
                              (setq regexp t)
                              (concat begstr "\\|" endstr)))
                           (t endstr)))
         (count (or count 1))
         (nesting count)
         (orig (point))
         beg-pos-delimiter end-pos-delimiter)
    (while
        (and
         (< 0 nesting) (not (eobp)))
      (if (and (looking-at searchform)
               (goto-char (match-end 0)))
          (end-of-form-base-intern begstr endstr comment string)
        (if (re-search-forward searchform bound noerror count)
            (progn
              (end-of-form-base-intern begstr endstr comment string)
              (goto-char (1+ (match-beginning 0)))))))
    (if (and beg-pos-delimiter end-pos-delimiter)
        (list beg-pos-delimiter end-pos-delimiter)
      (goto-char orig)
      nil)))

(defun end-of-form-base-intern (begstr endstr comment string)
  (let ((pps (parse-partial-sexp (point-min) (point))))
    ;; in string
    (if (and (not string) (nth 3 pps)(nth 8 pps))
        (progn
          (forward-char 1)
          (while (and (setq pps (parse-partial-sexp (point-min) (point)))(nth 3 pps)(nth 8 pps))
            (forward-char 1)))
      (unless (save-match-data
                (and condition (funcall condition)))
        (save-match-data
          (if
              (string-match endstr
                            (if regexp (match-string-no-properties 0)
                              (match-string-no-properties 0)))
              (if comment
                  (setq nesting (1- nesting))
                (unless (ar-in-comment-p)
                  (setq nesting (1- nesting))))
            ;; got another beginning while moving down
            (if comment
                (setq nesting (1+ nesting))
              (unless (ar-in-comment-p)
                (setq nesting (1+ nesting))))))
        (setq beg-pos-delimiter (match-beginning 0))
        (setq end-pos-delimiter (match-end 0))))))

(defvar match-paren-key-char "%")

(defvar be-match-paren-mode nil)

(defun be-match-paren-mode (&optional iact)
  "Toggle be-match-paren-mode.
If on, inserting of `be-match-paren-char', default is \"%\", moves to the matching opening/closing.
With arg, insert the charakter the key is on
Key per default is \"%\" as with elisp's `match-paren'. "
  (interactive "p")
  (if be-match-paren-mode
      (progn
        (setq be-match-paren-mode nil)
        ;;        (define-key be-mode-map "%" 'self-insert-command)
        (when iact (message "be-match-paren-mode: %s" be-match-paren-mode)))
    (setq be-match-paren-mode t)
    ;;    (define-key be-mode-map "%" 'be-match-paren)
    (when iact (message "be-match-paren-mode: %s" be-match-paren-mode))))

(defun beg-end-match-paren (begstr endstr &optional ins)
  "Go to the matching opening/closing.
First to opening, unless cursor is already there.
With arg, insert the charakter of `sh-match-paren-char'.
Key per default is \"%\" as with elisp's `match-paren'. "
  (if ins
      (insert match-paren-key-char)
    (cond ((ignore-errors (looking-at begstr))
           (end-of-form-base begstr endstr bound noerror count))
          ((ignore-errors (looking-at "\\s("))
           (match-paren arg))
          ((ignore-errors (looking-at "\\s)"))
           (match-paren arg))
          (t (beginning-of-form-base begstr endstr bound noerror count comment)))))

(defun in-form-base (begstr endstr &optional beg end count comment regexp condition)
  "Parse a structure. Return level of nesting, `nil' if not inside.
Takes strings as arguments for search.
Set comment to `t' if forms inside comments should match - also for processing comments itself.
Set REGEXP-QUOTE to t, if end-form should be quoted.
Set 8th argument REGEXP t, if beg/end-str are regular expressions.
"
  (let* ((beg (or beg (point-min)))
         (end (or end (point)))
         (nesting (or count count 0))
         (unquoted-end endstr)
         (beg-end-unequal (not (string= begstr endstr)))
         (begstr (cond
                  ((string= begstr "")
                   nil)
                  (t begstr)))
         (endstr (cond ((string= endstr "")
                        nil)
                       (t endstr)))
         (searchform (cond ((and beg-end-unequal begstr endstr)
                            (concat "\\(" begstr "\\)\\|\\(" endstr "\\)"))
                           (begstr begstr)
                           (endstr endstr)))
         (orig (point))
         beg-pos-delimiter end-pos-delimiter)
    (goto-char beg)
    (while
        (and
         (< (point) end) (not (eobp))
         (re-search-forward searchform end t 1))
      (in-form-base-intern))
    ;;     (when (looking-at searchform) (in-form-base-intern))
    (unless beg-end-unequal (setq nesting (% nesting 2)))
    (goto-char orig)
    nesting))

(defun in-form-base-intern ()
  (if
      (or (and beg-end-unequal begstr (string-match begstr (match-string-no-properties 0)))
          (not beg-end-unequal))
      (if comment
          (unless (and condition (funcall condition))(setq nesting (1+ nesting)))
        (unless (or (and condition (funcall condition))(ar-in-comment-p-lor))
          (setq nesting (1+ nesting))))
    (when (string-match endstr (match-string-no-properties 0))
      (if comment
          (unless (and condition (funcall condition))(setq nesting (1- nesting)))
        (unless (or (and condition (funcall condition))(ar-in-comment-p-lor))
          (setq nesting (1- nesting)))))))

;; Fixme: Instead use of condition `ar-in-comment-p-atpt' is hard-corded for the moment
(defun ar-in-delimiter-base (regexp &optional condition guess-delimiter)
  "REGEXP expected of an unary delimiter, for example
\"\\\\\\\"\\\\\\\"\\\\\\\"\\\\|'''\\\\|\\\\\\\"\\\\|'\" indicating string delimiters in Python.
Optional second arg --a number, nil or `t'-- if interactively called. "
  (let ((orig (point))
        (count 0)
        ;;        (regexp (replace-regexp-in-string "\"" "\\\\\""  regexp))
        tell beglist)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let* ((delimiter (when
                              (looking-at regexp)
                            (setq count (1+ count))
                            (match-string-no-properties 0)))
               (delimiter-p (stringp delimiter)))
          (if delimiter-p
              (progn
                (setq delimiter (concat "\\([^\\]\\)" (replace-regexp-in-string "\"" "\\\\\""  delimiter)))
                (setq beglist (list (match-beginning 0) (match-end 0)))
                (goto-char (match-end 0))
                (ar-in-delimiter-intern count orig beglist delimiter-p delimiter))
            (setq regxep (concat "[^\\]" regexp))
            (ar-in-delimiter-intern count orig beglist nil regexp)))))))

(defun ar-in-delimiter-intern (count orig beglist &optional first old)
  (let (done name this)
    (while (and (not done)(< (point) orig))
      (while (and
              (setq first (re-search-forward regexp orig t 1))(save-match-data (ar-in-comment-p-atpt))))
      (if first
          (progn
            (setq count (1+ count))
            (setq beglist (list (match-beginning 0) (match-end 0)))
            (when (eq 1 (% count 2))
              (goto-char (match-beginning 0))
              (setq delimiter (concat "\\([^\\]\\)" (match-string-no-properties 0) "\\|\\(\\\\\\\\\\)" (match-string-no-properties 0)))
              (setq old delimiter)
              (while (and (setq this (re-search-forward delimiter orig t 1))(save-match-data (ar-in-comment-p-atpt))))
              (if this (setq count (1+ count))
                (setq done t)))
            (setq first nil))
        (setq done t)))
    (setq erg (and (< 0 count)(eq 1 (% count 2))))
    (when (and (< 0 count) erg)
      beglist)))

(provide 'beg-end)

;;; beg-end.el ends here
