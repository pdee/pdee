;;; ar-comment-lor.el --- comment/uncomment line or region

;; Author: Andreas Roehler <andreas.roehler@online.de>, unless indicated otherwise

;; Keywords: wp, lisp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of

;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the

;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,

;; Boston, MA 02110-1301, USA.

;;; Major changes to previous version:
;; Make comment-start, comment-end buffer-local

;;; Commentary:

;; Further rewrite of comment-lor.el.
;; It's suffix `-lor' means `line-or-region': If no
;;  active region exists, line is commented or uncommented.

;; Thus taking action to mark a region is no longer needed,
;; which saves keystrokes.

;; Set out to fix a bug occuring in all Emacsen, reported at
;; http://tracker.xemacs.org/XEmacs/its/issue232
;; it ended up with an alternative comment.el

;; With `ar-c-toggle-comment-style-lor' both C comment-styles
;; are available. Try `ar-omment-c-test-lor' for the results.
;;  `ar-comment-dwim-lor' or `ar-comment-uncomment-lor' from
;;  inside a comment which spans over several lines
;;  will detect that and uncomment.

;; With universal arg, line or region is copied and
;; inserted over commented/uncommented part, thus
;; preserving old state

;; The bug of `indent-new-comment-line' should be gone
;; with the following key:
;; (global-set-key [(meta j)] 'ar-comment-right-margin-lor)

;; The following commands should be available:
;; (displayed here without suffix `lor':
;;                         comment-uncomment
;;                                 /\
;;             (copy/do-not-copy) /  \
;;                               /    \
;;                        comment uncomment
;;                       /\
;;                      /  \
;;                     /    \
;;                plain indent
;;               /\               /\
;;              /  \             /  \
;;             /    \           /    \
;;     span-line single span-line single
;;        /\        /\        /\        /\
;;       /  \      /  \      /  \      /  \
;;      /    \    /    \    /    \    /    \
;;
;;  ------------ boxed not-boxed ----------------

;; ToDo:

;; lor-Comment-uncomment detects only selected
;; comment-style, i.e. "/* */" or "//" in C. When
;; uncommenting, make sure the appropriate style is
;; switched on.

;; ar-comment-plain-box-lor and ar-comment-indent-box-lor
;; behave likewise presently.  Is there a need for two
;; different box-commands?

;;; Code:

(require 'misc-utils)
(require 'beg-end)
(require 'thing-at-point-utils)

;; defvars below should be set already, here just to be sure
(defvar comment-end "")
(make-variable-buffer-local 'comment-end)
(defvar comment-start ";")
(make-variable-buffer-local 'comment-start)
(defvar comment-padding " ")
(make-variable-buffer-local 'comment-padding)

(defvar comment-add 0
  "A var of this name now is used internal only, here set to avoid errors. ")
(make-variable-buffer-local 'comment-add)
;; (setq comment-add 0)

(defcustom ar-comment-fontify-lor t
  "If t, comment-functions call font-lock-fontify-buffer . "
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t))
  :group 'comment)

(defcustom ar-comment-empty-lines-lor nil
  "If nil, `comment-' does not comment out empty lines. "
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 :group 'comment))

(defcustom ar-indent-comment-lor nil
  "If after commenting (indent-according-to-mode) is called. "
  :type 'boolean
  :group 'comment)

(defcustom ar-indent-uncomment-lor nil
  "If after uncommenting (indent-according-to-mode) is called. "
  :type 'boolean
  :group 'comment)

(defcustom ar-comment-stay-on-line-lor nil
  "If after commenting/uncommenting cursor stay on present line. Default is nil"
  :type 'boolean
  :group 'comment)

(defcustom ar-comment-function-lor 'ar-comment-plain-lor
  "If after uncommenting (indent-according-to-mode) is called. "
  :type 'symbol
  :group 'comment)

;; (defalias 'ar-comment-function-lor 'ar-comment-indent-lor)

(defcustom ar-uncomment-function-lor 'ar-uncomment-lor
  "If after uncommenting (indent-according-to-mode) is called. "
  :type 'symbol
  :group 'comment)

(defgroup ar-comment-line-or-region nil
  "Indenting and filling of comments."
  :prefix "ar-"
  :group 'fill)

;; The following should be set in C-mode

(defalias 'ar-comment-toggle-c-style-lor 'ar-c-toggle-comment-style-lor)
(defun ar-c-toggle-comment-style-lor ()
  "Change the C default comment style"
  (interactive)
  (if (= 0 (length comment-end))
      (progn
        (set (make-local-variable 'comment-start) "/* ")
        (set (make-local-variable 'comment-end) " */")
        (when (interactive-p) (message "comment-start: %s comment-end: %s" comment-start comment-end)))
    (progn
      (set (make-local-variable 'comment-start) "/")
      (set (make-local-variable 'comment-end) "")
      (set (make-local-variable 'comment-add) 1))
    (when (interactive-p) (message "comment-start: %s comment-end: %s" comment-start comment-end))))

(defalias 'ar-comment-region-lor 'ar-comment-indent-lor)

(defun ar-comment-plain-lor (&optional copy beg end)
  "Comment line or region at beginning of line. "
  (interactive "*P")
  (let ((copy (or copy (eq 4 (prefix-numeric-value copy)))))
    (ar-comment-base-lor nil nil nil beg end copy)))

(defun ar-comment-indent-lor (&optional copy beg end)
  "Comment line or region, indent comments. "
  (interactive "*P")
  (let ((copy (or copy (eq 4 (prefix-numeric-value copy)))))
    (ar-comment-base-lor t nil nil beg end copy)))

(defun ar-comment-indent-single-lor (&optional copy beg end)
  "Comment line or region,  at beginning of lines. "
  (interactive "*P")
  (let ((copy (or copy (eq 4 (prefix-numeric-value copy)))))
    (ar-comment-base-lor t nil nil beg end copy)))

(defun ar-comment-indent-span-lor (&optional copy beg end)
  "Comment line or region,  at beginning of lines. "
  (interactive "*P")
  (let ((copy (or copy (eq 4 (prefix-numeric-value copy)))))
    (ar-comment-base-lor t nil t beg end copy)))

(defun ar-comment-plain-single-lor (&optional copy beg end)
  "Comment line or region. Use with start-end pairs every line. "
  (interactive "*P")
  (let ((copy (or copy (eq 4 (prefix-numeric-value copy)))))
    (ar-comment-base-lor nil nil nil beg end copy)))

(defun ar-comment-plain-span-lor (&optional copy beg end)
  "Comment line or region, span with one start-end over lines. "
  (interactive "*P")
  (let ((copy (or copy (eq 4 (prefix-numeric-value copy)))))
    (ar-comment-base-lor nil nil t beg end copy)))

(defun ar-comment-indent-box-lor (&optional copy beg end)
  "Comment out line or region, put it inside a box. "
  (interactive "*P")
  (let ((copy (or copy (eq 4 (prefix-numeric-value copy)))))
    (ar-comment-base-lor t t nil beg end copy)))

(defun ar-comment-plain-box-lor (&optional copy beg end)
  "Comment out line or region, put it inside a box. "
  (interactive "*P")
  (let ((copy (or copy (eq 4 (prefix-numeric-value copy)))))
    (ar-comment-base-lor nil t nil beg end copy)))

(defun ar-comment-base-lor (indent box span &optional beg end copy)
  (let* ((orig (point))
         (beg (cond ((and beg end) beg)
		    ((region-active-p)
                     (region-beginning))
		    (t (line-beginning-position))))
	 (end (cond ((and beg end) (copy-marker end))
		    ((region-active-p)
		     (copy-marker (region-end)))
		    (t (copy-marker (line-end-position)))))
	 (line (count-lines (point-min) (if (eq (point) (line-beginning-position)) (1+ (point)) (point))))
         (comment-end-not-empty (not (string= "" comment-end)))
         ;; comment-add should be set by progmodes
	 (add (if comment-end-not-empty 0 comment-add))
         ;; (integerp comment-padding) obsolet, kept for transition
         (comment-padding (if (integerp comment-padding)
                              (make-string comment-padding ? )
                            comment-padding))
	 (maxline 0)
	 first column)
    (when copy
      (back-to-indentation)
      (setq column (current-column))
      (copy-region-as-kill beg end))
    (cond (span
           (if (and span (not comment-end-not-empty))
               (error "%s" "No valid command in this context:
Comments spanning multiple lines need comment-end string")
             (ar-comment-intern-lor beg end indent span add comment-end-not-empty)))
          (box
           (ar-comment-box-intern-lor beg end indent comment-end-not-empty add))
          (t (ar-comment-intern-lor beg end indent span add comment-end-not-empty)))
    (when copy
      (back-to-indentation)
      (indent-to-column column)
      (insert (car kill-ring))
      (unless (looking-at "[ \t]*$")
        (newline-and-indent)))
    (when ar-comment-stay-on-line-lor
      (goto-char orig))
    (when ar-comment-fontify-lor (font-lock-fontify-buffer))))

(defun ar-comment-intern-lor (beg end indent span add comment-end-not-empty)
  (goto-char beg)
  (let (last done)
    (while (< (line-end-position) (1+ end))
      (let ((add (or add 0)))
        (if indent
            (back-to-indentation)
          (beginning-of-line))
        (unless (and span done)
          (ar-comment-start-insert comment-start add)
          ;; (delete-region (point) (progn (skip-chars-forward " \t")(point)))
          (setq done t))
        (when comment-end-not-empty
          (goto-char (line-end-position))
          (skip-chars-backward " \t\r\n")
          (when (stringp comment-padding) (insert comment-padding))
          (when span (setq last (copy-marker (point))))
          (unless span
            (save-excursion
              (insert comment-end))
            (fixup-whitespace))))
      (when indent
        (back-to-indentation)
        (indent-according-to-mode))
      (forward-line 1))
    (when span
      (goto-char last)
      (insert comment-end))))

(defun ar-comment-box-intern-lor (beg end indent comment-end-not-empty add)
  (untabify beg end)
  (let* ((min-max (ar-comment-min-max-col-lor beg end comment-end-not-empty))
         (min-col (car min-max))
         (max-col (cadr min-max))
         (length-comment-start (+ (length (ar-comment-start-without-padding-lor)) (length comment-padding) add))
         (length-comment-end (if (string= "" comment-end)
                                 (+ (length comment-padding) 1 comment-add)
                               (length (ar-comment-end-without-padding-lor))))
         (comment-fill-char (substring (string-strip comment-start) -1))
         comment-fill-column)
    (if (< 0 (- min-col length-comment-start))
        (progn
          (setq min-col (- min-col length-comment-start))
          (setq max-col (+ max-col length-comment-end)))
      (setq max-col (+ max-col length-comment-start length-comment-end)))
    (unless indent (setq max-col (- max-col min-col))
            (setq min-col 0))
    (setq comment-fill-column (- max-col length-comment-end))
    (ar-comment-set-boxes-lor beg end indent min-col max-col add length-comment-start length-comment-end comment-fill-column)
    (ar-comment-set-head-foot-lor beg end min-col max-col add length-comment-start length-comment-end comment-fill-column comment-fill-char)
    (forward-line 1)))

(defun ar-insert-head-foot-intern-lor (comment-fill-column comment-fill-char add length-comment-start length-comment-end comment-end-not-empty)
  (if comment-end-not-empty
      (progn
        (ar-comment-start-insert comment-start add nil t)
        (insert (make-string (- comment-fill-column (current-column)) (aref comment-fill-char 0)))
        (ar-comment-end-box-insert-lor comment-start add t comment-fill-char))
    (insert (make-string (- (+ comment-fill-column length-comment-end) (current-column)) (aref comment-fill-char 0)))))

(defun ar-comment-set-head-foot-lor (beg end min-col max-col add length-comment-start length-comment-end comment-fill-column comment-fill-char)
  (goto-char beg)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-to-column min-col)
  (ar-insert-head-foot-intern-lor comment-fill-column comment-fill-char add length-comment-start length-comment-end comment-end-not-empty)
  (goto-char end)
  (if (bolp)
      (progn
        (newline)
        (forward-line -1))
    (end-of-line)
    (newline))
  (indent-to-column min-col)
  (ar-insert-head-foot-intern-lor comment-fill-column comment-fill-char add length-comment-start length-comment-end comment-end-not-empty))

(defun ar-comment-min-max-col-lor (beg end comment-end-not-empty &optional min-col max-col)
  "Used by `box': Return min and max columns for setting box. "
  (let ((min-col (or min-col 1000000)) (max-col (or max-col 0)))
    (goto-char beg)
    (while (<= (line-end-position) end)
      (back-to-indentation)
      (let ((poscol (current-column)))
        (when (< poscol min-col) (setq min-col poscol))
        (end-of-line)
        (skip-chars-backward " \t")
        (setq poscol (current-column))
        (when (< max-col poscol) (setq max-col poscol))
        (forward-line 1)))
    (list min-col max-col)))

(defun ar-comment-end-box-insert-lor (comment-start &optional add no-padding comment-fill-char)
  "For comment-box style if mode has no comment-end string. "
  (if (string= "" comment-end)
      (let ((add (or add 0)))
        (unless no-padding
          (if (looking-at comment-padding)
              (forward-char (length comment-padding))
            (insert comment-padding)))
        (insert comment-start)
        (while (< 0 add)
          (insert comment-start)
          (setq add (1- add))))
    (if no-padding
        (let ((length-comment-padding (length comment-padding)))
          (insert (concat (make-string length-comment-padding (aref comment-fill-char 0)) (substring comment-end length-comment-padding))))
      (insert comment-end))))

(defun ar-comment-set-boxes-lor (beg end indent min-col max-col add length-comment-start length-comment-end comment-fill-column)
  (goto-char beg)
  (while (<= (line-end-position) end)
    (beginning-of-line)
    (let ((pos (save-excursion (progn (move-to-column min-col t) (point))))
          (no-padding (or (string= "" comment-padding)
                          (eq 0 comment-padding))))
      (skip-chars-forward " \t" pos)
      (when (< (current-column) min-col) (indent-to-column min-col))
      (ar-comment-start-insert comment-start add)
      (fixup-whitespace)
      (end-of-line)
      (skip-chars-backward " \t")
      (skip-chars-forward " \t" comment-fill-column)
      (let ((col (current-column)))
        (when (< col comment-fill-column)
          (insert (make-string (- comment-fill-column col) 32))))
      (ar-comment-end-box-insert-lor comment-start add no-padding comment-fill-char)
      ;; (fixup-whitespace)
      (forward-line 1))))

(defun ar-comment-right-margin-lor (&optional col)
  "Insert a comment after code on current line. "
  (interactive "*")
  (let* ((add comment-add)
         (continue (ar-comment-atpt))
         (col (or col
                  (if continue
                      (progn (goto-char
                              (ar-comment-beginning-position-atpt))
                             ;; needed for //
                             (while (looking-back (regexp-quote comment-start))
                               (goto-char (match-beginning 0)))
                             (current-column))
                    (end-of-line)
                    (skip-chars-backward " \t")
                    (if (looking-at " ")
                        (forward-char 1)
                      (insert " "))
                    (current-column)))))
    (when continue
      (end-of-line)
      (newline)
      (indent-to-column col))
    (ar-comment-start-insert comment-start add)
    (ar-comment-inl-endform-lor)))

(defun ar-comment-inl-endform-lor (&optional span)
  (save-excursion
    (unless (and (string= "" comment-end) (not span))
      (insert comment-end)))
  (unless (or (looking-at comment-padding)
	      (looking-back comment-padding))
    (insert comment-padding)))

(defalias 'ar-uncomment-region-lor 'ar-uncomment-lor)

(defun ar-uncomment-lor (&optional copy beg end)
  (interactive "*P")
  (let ((beg (cond ((and beg end) beg)
		   ((region-active-p)
		    (region-beginning))
		   (t (line-beginning-position))))
	(end (cond ((and beg end) (copy-marker end))
		   ((region-active-p)
		    (copy-marker (region-end)))
		   (t (copy-marker (line-end-position)))))
	first (maxline 0)
	(add comment-add)
        column)
    (when copy
      (back-to-indentation)
      (setq column (current-column))
      (copy-region-as-kill beg end))
    (ar-uncomment-intern-lor beg end add)
    (when ar-indent-uncomment-lor (indent-according-to-mode))
    (if ar-comment-stay-on-line-lor
        (goto-char end)
      (forward-line 1))
    (save-excursion
      (when copy
        (goto-char beg)
        (indent-to-column column)
        (insert (car kill-ring))
        (newline-and-indent)))
    (when ar-comment-fontify-lor (font-lock-fontify-buffer))))

(defun ar-remove-comment-end ()
  " "
  (when (looking-at comment-start)
    (goto-char (match-end 0)))
  (let ((erg (end-of-form-base comment-start comment-end nil t nil 1)))
    (delete-region (car erg) (cadr erg))
    (when (looking-back comment-padding)
      (replace-match ""))))

(defun ar-remove-comment-start (add)
  (unless (looking-at (regexp-quote comment-start))
    (back-to-indentation))
  (while (< -1 add)
    (when
        (looking-at (regexp-quote comment-start))
      (replace-match ""))
    (setq done t)
    (setq add (1- add)))
  (when done
    (if (numberp comment-padding)
        (when (looking-at (make-string comment-padding ?\ ))
          (replace-match ""))
      (when (looking-at comment-padding)
        (replace-match ""))))
  (when ar-indent-uncomment-lor
    (indent-according-to-mode)))

(defun ar-uncomment-intern-lor (beg end add)
  (while (< beg end)
    (let ((add add)
          (comment-nests (not (string= "" comment-end)))
          done)
      (when comment-nests
        (ar-remove-comment-end))
      (ar-remove-comment-start add))
    (setq beg (1+ (line-end-position)))
    (when (< (line-end-position) end)
      (forward-line 1))))

(defun ar-comment-dwim-lor (&optional copy beg end)
  "Modifies `ar-comment-or-uncomment-lor'.
Thus if a region contains lines with one or more comments,
only lines with one comment are uncommented completly.
Calls `comment' at the end of line as `ar-comment-right-margin-lor'.
Comments empty lines ignoring value of `ar-comment-empty-lines-lor'"
  (interactive "*P")
  (let ((copy (or copy (eq 4 (prefix-numeric-value copy))))
        (beg (cond ((and beg end) (copy-marker beg))
                   ((region-active-p)
                    (region-beginning))
                   (t (line-beginning-position))))
        (end (cond ((and beg end)
                    (copy-marker end))
                   ((region-active-p)
                    (copy-marker (region-end)))
                   (t (copy-marker (line-end-position))))))
    (goto-char beg)
    (while (and (empty-line-p) (not (eobp)))
      (forward-line 1))
    (back-to-indentation)
    (if (looking-at comment-start)
        (funcall ar-uncomment-function-lor copy beg end)
      (funcall ar-comment-function-lor copy beg end))))

(defun ar-comment-or-uncomment-lor (&optional copy beg end)
  "Comment line or region, unless it's already commented:
uncomment then.
With region: Only first line is checked for decision.
Thus if first line is not commented, but some lines below are,
region is commented alltogether. "
  (interactive "*P")
  (save-match-data
    (let ((copy (or copy (eq 4 (prefix-numeric-value copy))))
          (beg (cond ((and beg end) beg)
                     ((region-active-p)
                      (region-beginning))
                     (t (line-beginning-position))))
          (end (cond ((and beg end)
                      (copy-marker end))
                     ((region-active-p)
                      (copy-marker (region-end)))
                     (t (copy-marker (line-end-position))))))
      (goto-char beg)
      (back-to-indentation)
      (setq beg (point))
      (cond
       ((ar-in-comment-p-lor)
        (funcall ar-uncomment-function-lor copy beg end))
       ((empty-line-p)
	(forward-line 1))
       (t (funcall ar-comment-function-lor copy beg end))))))

(defun ar-comment-start-insert (comment-start &optional add indent no-padding)
  (let ((add (or add 0)))
    (insert (ar-comment-start-without-padding-lor))
    (while (< 0 add)
      (insert comment-start)
      (setq add (1- add))))
  (unless (or no-padding (looking-at comment-padding))
    (insert comment-padding))
  (when indent (fixup-whitespace)))

(defun ar-comment-start-without-padding-lor ()
  "Returns comment-start stripped by padding chars. "
  (substring comment-start 0 (string-match comment-padding comment-start)))

(defun ar-comment-end-without-padding-lor ()
  "Returns comment-start stripped by padding chars. "
  (substring comment-end (string-match (concat "[^" comment-padding "]") comment-end)))

(defalias 'ar-in-comment-p-lor 'ar-in-comment-p-atpt)

(provide 'ar-comment-lor)
;;; ar-comment-lor.el ends here
