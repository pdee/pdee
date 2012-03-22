;;; misc-utils.el --- Strip CHARS from STRING and more

;; Author: Andreas Roehler <andreas.roehler@easy-emacs.de>, unless indicated otherwise

;; Keywords: convenience

;; This file is free software; you can redistribute it

;; and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Provide some functions for convenience

;; Changes to previous version:

;; Moved forms like
;; `ar-skip-blanks-and-comments-forward-lor' from
;; ar-comment-lor into here

;;; History:
;;

;;; Code:

;; string-strip stuff starts here
(defalias 'string-strip 'strip)
(defalias 'string-strip-right 'rstrip)
(defalias 'string-strip-left 'lstrip)

(defcustom strip-chars-before "[ \t\r\n]*"
  "Regexp indicating which chars shall be stripped before STRING. "

  :type 'string
  :group 'convenience)

;; (setq strip-chars-after "[ \t\r\n]*")
(defcustom strip-chars-after "[ \t\r\n]*\\'"
  "Regexp indicating which chars shall be stripped after STRING. "

  :type 'string
  :group 'convenience)

;;;###autoload
(defun rstrip (str &optional chars-after)
  "Return a string or change region with trailing characters
 removed. Defaults to the value uof
 `strip-chars-after', basically whitespace, which is customizable.
    (rstrip \"spacious \")
    ==> \"spacious\""
  (interactive "*")
  (let ((chars-after (or chars-after t)))
    (strip str nil chars-after)))

(defun lstrip (str &optional chars-before)
  "Return a string or change region with leading characters
 removed. Defaults to the value uof
 `strip-chars-before', basically whitespace, which is customizable.
    (lstrip \"     spacious\")
    ==> \"spacious\""
  (interactive "*")
  (let ((chars-before (or chars-before t)))
    (strip str chars-before)))

(defun strip (&optional str chars-before chars-after)
  "Return a copy of STR, leading and/or trailing CHARS removed.
Default of `CHARS-BEFORE', `CHARS-AFTER': space, tab, carriage return, newline, newpage.
If no string is delivered but region is active, strip region.
    (strip \"   spacious   \")
    ==> \"spacious\""
  (interactive "*")
  (let* ((got-string (stringp str))
         (beg (cond (got-string)
                    ((region-active-p)
                     (region-beginning))
                    (t (point-min))))
         (end (cond (got-string)
                    ((region-active-p)
                     (region-end))
                    (t (point-max))))
         (str (cond (got-string str)
                    (t
                     (buffer-substring-no-properties beg end))))
         (s-c-b (cond ((stringp chars-before)
                       chars-before)
                      ((and (not chars-before) chars-after) "")
                      (t strip-chars-before)))
         (s-c-a (cond ((stringp chars-after)
                       chars-after)
                      ((and chars-before (not chars-after)) "")
                      (t strip-chars-after)))
         newstring erg)
    (save-excursion
      (with-temp-buffer
        (insert str)
        (unless (string= "" s-c-a)
          (skip-chars-backward s-c-a)
          (delete-region (point) (point-max)))
        (goto-char (point-min))
        (unless (string= "" s-c-b)
          (skip-chars-forward s-c-b)
          (delete-region (point-min) (point)))
        (setq erg (buffer-substring-no-properties (point-min) (point-max)))))
    (unless got-string (delete-region beg end)
            (insert erg))
    (when (interactive-p) (message "%s" erg))
    erg))

;; string-strip stuff ends here
(defcustom empty-line-p-chars "^[ \t\r]*$"
  "Empty-line-p-chars."
  :type 'regexp
  :group 'convenience)

(unless (functionp 'empty-line-p)
  (defun empty-line-p (&optional iact)
    "Returns t if cursor is at an empty line, nil otherwise."
    (interactive "p")
    (save-excursion
      (beginning-of-line)
      (when iact
        (message "%s" (looking-at empty-line-p-chars)))
      (looking-at empty-line-p-chars))))

;;;###autoload
(unless (functionp 'in-string-p)
  (defun in-string-p (&optional pos)
    (let ((orig (or pos (point))))
      (save-excursion
        (save-restriction
          (widen)
          (beginning-of-defun)
          (numberp (nth 3 (parse-partial-sexp (point) orig))))))))

(defun ar-in-string-or-comment-p ()
  "Returns beginning position if inside a string or comment, nil otherwise. "
  (interactive)
  (let* ((erg (nth 8 (if (featurep 'xemacs)
                         (parse-partial-sexp (point-min) (point))
                       (syntax-ppss))))
         (la (unless erg (when (or (looking-at "\"")(looking-at comment-start)(looking-at comment-start-skip))
                           (match-beginning 0)))))
    (setq erg (or erg la))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-in-string-p-pps ()
  "Function `in-string-p' returns nil if at the first `\"'.
  Fixed here. Uses `parse-partial-sexp'. "
  (interactive)
  (let* ((erg (nth 8 (parse-partial-sexp (point-min) (point))))
         (la (unless erg (when (looking-at "\"")
                           (match-beginning 0)))))
    (setq erg (or erg la))
    (when (interactive-p) (message "%s" erg))
    erg))

;; should go into base
;; (defun ar-in-string-p ()
;;   "In inside a triplequoted or singlequoted string,
;; parsed by regexp, not syntax related `parse-partial-sexp'"
;;   (interactive)
;;   (let ((erg (or (ar-in-doublequoted-p-atpt)
;;                  (ar-in-singlequoted-p-atpt)
;;                  (ar-in-triplequoted-p-atpt))))
;;     (when (interactive-p) (message "%s" erg))
;;     erg))

(unless (functionp 'empty-stringp)
  (defun empty-stringp (string)
    (string= "" string)))

;; redefining it here as existing function returs false positives
;; with GNU Emacs 23.1.50.1 (i686-pc-linux-gnu, GTK+ Version 2.12.0) of 2009-07-25
(unless (featurep 'xemacs)
  (unless (functionp 'region-active-p)
    (defun region-active-p ()
      "and mark-active transient-mark-mode
 (not (eq (region-beginning) (region-end)"
      (and mark-active transient-mark-mode
           (not (eq (condition-case nil (region-beginning)(error nil)) (condition-case nil (region-end) (error nil))))))))

(unless (functionp 'indent-with-spaces)
  (defun indent-with-spaces (column)
    "Indent with spaces to column "
    (interactive "*")
    (while (< (current-column) column)
      (insert " "))
    (forward-char 1)))

(when (featurep 'xemacs)
  (unless (functionp 'looking-back)
    ;; from GNU Emacs subr.el
    (defun looking-back (regexp &optional limit greedy)
      "Return non-nil if text before point matches regular expression REGEXP.
    Like `looking-at' except matches before point, and is slower.
    LIMIT if non-nil speeds up the search by specifying a minimum
    starting position, to avoid checking matches that would start
    before LIMIT.
    If GREEDY is non-nil, extend the match backwards as far as possible,
    stopping when a single additional previous character cannot be part
    of a match for REGEXP."
      (let ((start (point))
            (pos
             (save-excursion
               (and (re-search-backward (concat "\\(" regexp "\\)") limit t)
                    (point)))))
        (if (and greedy pos)
            (save-restriction
              (narrow-to-region (point-min) start)
              (while (and (> pos (point-min))
                          (save-excursion
                            (goto-char pos)
                            (backward-char 1)
                            (looking-at (concat "\\("  regexp "\\)\\'"))))
                (setq pos (1- pos)))
              (save-excursion
                (goto-char pos)
                (looking-at (concat "\\("  regexp "\\)\\'")))))
        (not (null pos))))))

(when (eq 0 (string-match "^23" emacs-version))
  (defalias 'region-exists-p 'use-region-p))

(defun buffer-narrowed-p ()
  "Returns a list, beg and end position of accessible buffer,
if narrowed, nil otherwise. "
  (interactive)
  (let* ((beg (point-min))
         (end (point-max))
         (erg (/= (buffer-size)
                  (- end
                     beg))))
    (when (interactive-p)
      (if erg
          (message "%s" (list beg end))
        (message "%s" erg)))
    erg))

(defun ar-current-indentation-position ()
  "Returns beginning position of code in line. "
  (interactive)
  (let ((here (point))
        (pos (progn (back-to-indentation)(point))))
    (prog1
        (point)
      (when (interactive-p) (message "%s" pos))
      (goto-char here))))

(defalias 'defun-beginning-position 'function-beginning-position)
(defun function-beginning-position ()
  "Return the position where the current functions definition starts"
  (interactive)
  (save-excursion
    (let* ((orig (point)))
      (beginning-of-defun)
      (when (< (point) orig)
        (when (interactive-p) (message "%s" (point)))
        (point)
        ))))

(defalias 'defun-end-position 'function-end-position)
(defun function-end-position ()
  "Print the position where the current functions definition ends"
  (interactive)
  (save-excursion
    (let ((orig (point)))
      (end-of-defun)
      (when (< orig (point))
        (when (interactive-p) (message "%s" (point)))
        (point)))))

(defalias 'copy-defun 'copy-function)
(defun copy-function ()
  (interactive "p")
  (when (or
         (looking-at
          (if defun-prompt-regexp
              (concat "^\\s(\\|"
                      "\\(" defun-prompt-regexp "\\)\\s(")
            "^\\s("))
         (eq (current-column) 0))
    (end-of-line))
  (save-excursion
    (let ((start (if
                     (looking-at "(defun")
                     (point)
                   (defun-beginning-position)))
          (end (function-end-position)))
      (kill-new (buffer-substring-no-properties start end)))))

(defalias 'kill-defun 'kill-function)
(defun kill-function ()
  (interactive)
  (save-excursion
    ;; don't go backward, if already at beginning
    (when
        (or
         (looking-at (if defun-prompt-regexp
                         (concat "^\\s(\\|"
                                 "\\(" defun-prompt-regexp "\\)\\s(")
                       "^\\s("))
         (eq (current-column) 0))
      (end-of-line))
    (kill-region
     (function-beginning-position) (function-end-position))))

(defun ar-name-of-defun-atpt (&optional pos)
  "Return name of function definition at point.
Goto optional arg POS, if given. "
  (interactive)
  (save-excursion
    (let (name)
      (when pos (goto-char pos))
      (beginning-of-defun)
      (forward-word 1)
      (when (looking-at " *\\([(]+\\) *")
        (setq name (match-string-no-properties 1)))
      (when (interactive-p) (message "%s" name))
      name)))

(defun goto-outbuf (outbuf &optional iact)
  "Go to the end of a named puffer, provide a newline.
Show it, if interactively called. "
  (set-buffer outbuf)
  (when iact (switch-to-buffer (current-buffer)))
  (goto-char (point-max))
  (unless (empty-line-p) (newline)))

(defun ar-whitespaces-backward ()
  (interactive)
  (skip-chars-backward " \t\r\n\f")
  (point))

(defun ar-whitespaces-forward ()
  (interactive)
  (skip-chars-forward " \t\r\n\f")
  (point))

(defun ar-skip-blanks-and-comments-forward-lor (&optional arg)
  "Go forward over empty lines and comments alike.
With negative arg go backward. "
  (interactive "p")
  (ar-skip-blanks-and-comments-lor arg))

(defun ar-skip-blanks-and-comments-backward-lor (&optional arg)
  "Go forward over empty lines and comments alike.
With negative arg go backward. "
  (interactive "p")
  (ar-skip-blanks-and-comments-lor (if (< 0 arg) (- arg) arg)))

;; helpers
(defun ar-skip-blanks-and-comments-lor (&optional arg)
  "Go forward over empty lines and comments alike.
With negative arg go backward. "
  (or arg (setq arg 1))
  (let ((pos (point)))
    (if (< 0 arg)
        (progn
          (skip-chars-forward " \t\r\n")
          (when (ar-in-comment-p)
            (goto-char (ar-comment-end-position-atpt)))
          (when (empty-line-p)
            (forward-line arg))
          (when (> (point) pos)
            (ar-skip-blanks-and-comments-lor arg)))
      (skip-chars-backward " \t\r\n")
      (when (ar-in-comment-p)
        (goto-char (ar-comment-beginning-position-atpt)))
      (when (empty-line-p)
        (forward-line arg))
      (when (< (point) pos)
        (ar-skip-blanks-and-comments-backward-lor arg)))))

(defun ar-skip-chars-to-comment-lor (arg)
  "With positive arg, skip forward until next comment.
  With negative arg, skip backward.
 If no comment found, beg or end of buffer is reached. "
  (if (< 0 arg)
      (while (not (or (ar-in-comment-p)
                      ;; (nth 4 (parse-partial-sexp (point-min) (point)))
                      (eobp)))
        (forward-char 1))
    (while (not (or (ar-in-comment-p)
                    ;; (nth 4 (parse-partial-sexp (point-min) (point)))
                    (bobp)))
      (forward-char -1))))

(defun ar-down-list (&optional arg)
  "Makes `down-list' work also if inside a string or comment. "
  (interactive "p")
  (while (or (when (in-string-p)
               (ar-forward-string-atpt)
               (forward-char 1))
             (when (ar-in-comment-p)
               (ar-forward-comment-atpt)
               (forward-char 1))))
  (down-list arg))

(defun reverse-chars (&optional arg)
  "Reverse reciproke chars as \"[\" to \"]\", upcase or downcase,
Switch `\"' with `''
If over a number, add ARG to it.
With negative arg, substract from number. "
  (interactive "*p")
  (let (i)
    (if (looking-at "[0-9]")
        (ar-add-to-number arg)
      (let* ((cf (char-after))
             (cn (downcase cf)))
        (cond ((or (eq cf 34)(eq cf ?\"))
               (setq cn "'"))
              ((or (eq cf 39)(eq cf ?\'))
               (setq cn "\""))
              ((or (eq cf 43)(eq cf ?\+))
               (setq cn "-"))
              ((or (eq cf 62)(eq cf ?\>))
               (setq cn "<"))
              ((or (eq cf 60)(eq cf ?\<))
               (setq cn ">"))
              ((or (eq cf 187)(eq cf ?\»))
               (setq cn "«"))
              ((or (eq cf 171)(eq cf ?\«))
               (setq cn "»"))
              ((or (eq cf 41)(eq cf ?\)))
               (setq cn "("))
              ((or (eq cf 40)(eq cf ?\())
               (setq cn ")"))
              ((or (eq cf 123) (eq cf ?\{))
               (setq cn "}"))
              ((or (eq cf 125) (eq cf ?\}))
               (setq cn "{"))
              ((or (eq cf 93)(eq cf ?\]))
               (setq cn "["))
              ((or (eq cf 91)(eq cf ?\[))
               (setq cn "]"))
              ((or (eq cf 45)(eq cf ?\-))
               (setq cn "_"))
              ((or (eq cf 95)(eq cf ?\_))
               (setq cn "-"))
              ((or (eq cf 92)(eq cf ?\\))
               (setq cn "/"))
              ((or (eq cf 47)(eq cf ?\/))
               (setq cn "\\"))
              (t (when (eq cf cn)
                   (setq cn (upcase cf)))))
        (delete-char 1)
        (insert cn)))))

(defun ar-switch-quotes (&optional beg end)
  "Replaces `'' with `\"' or `\"' with `'', first found decides. "
  (interactive
   (list (or beg
             (cond ((buffer-narrowed-p)
                    (point-min))
                   ((region-active-p)
                    (region-beginning))
                   (t (point-min))))
         (or end (cond
                  ((buffer-narrowed-p)
                   (point-max))
                  ((region-active-p)
                   (copy-marker (region-end)))
                  (t (point-max))))))
  (barf-if-buffer-read-only)
  (ar-switch-quotes-intern beg end))

(defun ar-switch-quotes-intern (beg end)
  (goto-char beg)
  (let* ((oldquote (progn (re-search-forward "\[\"']" end t 1) (goto-char (match-beginning 0))(char-after)))
         (newquote (if (ignore-errors (eq oldquote 34)) 39 34))
         erg)
    (while (and oldquote (looking-at (char-to-string oldquote)))
      (replace-match (char-to-string newquote))
      (setq erg (search-forward (char-to-string oldquote) end t 1))
      (when erg (goto-char (match-beginning 0))))))

;; * Simple useful functions
;;
;; Date: Thu, 28 Oct 2010 11:56:15 -0700
;; From: Tak Ota <Takaaki.Ota@am.sony.com>
;; To: <emacs-devel@gnu.org>
;; Subject: simple useful functions
;; Envelope-To: andreas.roehler@online.de
;;
;; If you think the following two functions are universally useful please
;; consider incorporating them in simple.el or any appropriate package.
;; If not disregard.
;;
;; -Tak

(defun collect-string (regexp &optional num)
  "Collect strings of REGEXP (or optional NUM paren) from the
current buffer into a collection buffer."
  (interactive "sCollect string (regexp): \nP")
  (let ((collection-buffer
	 (get-buffer-create (format "*Collection of \"%s\" *" regexp))))
    (with-current-buffer collection-buffer (erase-buffer))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(let ((str (match-string (or num 0))))
	  (if str
	      (with-current-buffer collection-buffer
		(insert str)
		(or (zerop (current-column))
		    (insert "\n")))))))
    (pop-to-buffer collection-buffer)
    (goto-char (point-min))))

;; -Tak
(defun source (script &optional shell keep-current-directory)
  "Source the specified shell script.
Source the shell SCRIPT and import the environment into this
emacs.  The optional SHELL specifies the shell other than the
default `shell-file-name'.  When KEEP-CURRENT-DIRECTORY is nil,
which is the default, the current directory is temporarily
changed to the directory where the script resides while sourcing
the script."
  (interactive "fscript file: ")
  (if (null shell)
      (setq shell shell-file-name))
  (with-temp-buffer
    (unless keep-current-directory
      (setq default-directory (file-name-directory script)))
    (call-process shell nil t nil "-c" (concat "source " script "; printenv"))
    (while (re-search-backward "^\\([^=]+\\)=\\(.*\\)$" nil t)
      (setenv (match-string 1) (match-string 2)))))

;;
(defun ar-add-to-number-and-forward (&optional arg)
  (interactive "p*")
  (while (and (not (eobp))(looking-at "[0-9]"))
    (ar-add-to-number arg)
    (re-search-forward "[0-9]" nil t 1)))

(defun ar-decrease-number (arg &optional beg end)
  "Decrease numbers found in buffer or region with count with ARG,
default is -1.
With negative ARG, decrease number resp. to ARG. "
  (interactive (list
                (if current-prefix-arg
                    (prefix-numeric-value current-prefix-arg)
                  1)))
  (ar-add-to-number (- (abs arg)) beg end))

(defalias 'ar-increase-number 'ar-add-to-number)
(defun ar-add-to-number (arg &optional beg end)
  "Raise numbers found in buffer or region with count with ARG,
default is 1.
With negative ARG, decrease number resp. to ARG. "
  (interactive "p*")
  (let* ((beg1 (cond (beg)
                     ((region-active-p)
                      (region-beginning))))
         (end1 (cond (end)
                     ((region-active-p)
                      (copy-marker (region-end)))))
         (collect (eq 4 (prefix-numeric-value arg)))
         (count (abs arg)))
    (let* ((bounds (ar-bounds-of-number-atpt))
           (beg (car bounds))
           (end (cdr bounds)))
      (save-excursion
        (if (and beg end)
            (ar-add-to-number-base arg (buffer-substring-no-properties beg end) beg end beg1 end1 count collect)
          (ar-add-to-number-intern arg beg end beg1 end1 count))))))

(defalias 'ar-increase-number-cummulative 'ar-add-to-number-cummulative)
(defun ar-add-to-number-cummulative (step &optional beg end)
  "Raise numbers found in buffer or region with count starting with STEP, augmenting STEP with STEP each times.
Default for STEP is 1.
With negative ARG, decrease number resp. to ARG. "
  (interactive "p*")
  (let* ((beg1 (cond (beg)
                     ((region-active-p)
                      (region-beginning))))
         (end1 (cond (end)
                     ((region-active-p)
                      (copy-marker (region-end)))))
         (count (abs step))
         (collect t))
    (let* ((bounds (ar-bounds-of-number-atpt))
           (beg (car bounds))
           (end (cdr bounds)))
      (save-excursion
        (if (and beg end)
            (ar-add-to-number-base step (buffer-substring-no-properties beg end) beg end beg1 end1 count collect)
          (ar-add-to-number-intern step beg end beg1 end1 count collect))))))

(defun ar-add-to-number-intern (step beg end beg1 end1 count &optional collect)
  (goto-char beg1)
  (while (and (< (point) end1)(re-search-forward "\\([0-9]+\\)" end1 t))
    (ar-add-to-number-base step (match-string-no-properties 0) (match-beginning 0) (match-end 0) beg1 end1 count collect)))

(defun ar-add-to-number-base (step zeichen beg end beg1 end1 count collect)
  (let ((zahl (if (< 0 step)
                  (+ (string-to-number zeichen) count)
                (- (string-to-number zeichen) count)))
        pos)
    (delete-region beg end)
    (insert (format "%s" zahl))
    (when collect
      (if (< 0 step)
          (setq count (+ count step))
        (setq count (- count step)))
      (when (< (setq pos (point)) end1)
        (ar-add-to-number-intern step nil nil pos end1 count collect)))))

(defun ar-count-lines (&optional beg end)
  "Count from point-min until curser position - (point)

If no optional arguments are given but buffer is narrowed,
use the narrowed part.
If no optional arguments are given but region is active,
use the region.

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=7115"
  (interactive)
  (let ((beg (cond (beg)
                   ((buffer-narrowed-p)
                    (point-min))
                   ((region-active-p)
                    (region-beginning))
                   (t (point-min))))
        (end (cond (end)
                   ((buffer-narrowed-p)
                    (point-max))
                   ((region-active-p)
                    (copy-marker (region-end)))
                   (t (point))))
        erg)
    (if (featurep 'xemacs)
        (setq erg (count-lines beg end))
      (setq erg (1+ (count-matches "[\n\C-m]" beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defvar ar-indentation-registered '()
  "Filled in by `ar-register-indentation' and used by `ar-apply-indentation'")

(defun ar-register-indentation (&optional beg end)
  "Registers current functions indentation into `ar-indentation-registered'.
 `ar-aplly-indentation' indents from current line according to this list "
  (interactive)
  (let ((beg (cond (beg)
                   ((region-active-p)
                    (region-beginning))
                   ((bobp)
                    (point))
                   (t (defun-beginning-position))))
        (end (cond (end)
                   ((region-active-p)
                    (copy-marker (region-end)))
                   ((eobp)
                    (point))
                   (t (defun-end-position))))
        indentlist)
    (ar-register-indentation-intern beg end)))

(defun ar-register-indentation-intern (beg end)
  (let (indentlist)
    (goto-char beg)
    (setq indentlist (cons (current-indentation) indentlist))
    (while (re-search-forward "^[ \t]+" end t)
      (setq indentlist (cons (current-indentation) indentlist)))
    (setq ar-indentation-registered (nreverse indentlist))
    (message "%s" ar-indentation-registered)
    ar-indentation-registered))

(defun ar-apply-indentation (&optional beg)
  "Idents respective values in `ar-indentation-registered'"
  (interactive "*")
  (when beg (goto-char beg))
  (beginning-of-line)
  (dolist (elt ar-indentation-registered)
    (indent-to-column elt)
    (let ((orig (point)))
      (delete-region orig (+ orig (skip-chars-forward " \t"))))
    (forward-line 1)))

;;      	  ((looking-at "\\s(")
;; 	   (match-paren arg))
;; 	  ((looking-at "\\s)")
;; 	   (match-paren arg))

(defvar match-paren-no-use-syntax-pps nil)
;; (define-key py-mode-map [(%)] 'py-match-paren)
;; (global-set-key [(%)] 'match-paren)
;; (setq match-paren-char "%")

(defun ar-match-paren-no-use-syntax-pps ()
  "Toggle match-paren-no-use-syntax-pps. "
  (interactive)
  (setq match-paren-no-use-syntax-pps (not match-paren-no-use-syntax-pps))
  (message "match-paren-no-use-syntax-pps set to: %s" match-paren-no-use-syntax-pps))

(defun match-paren (&optional arg)
  "Go to the matching brace, bracket or parenthesis if on its counterpart.
Otherwise insert the character, the key is assigned to, here `%'.
With \\[universal-argument] insert a `%'. "
  (interactive "P")
  (if arg
      (self-insert-command (if (numberp arg) arg 1))
    (cond
     ((and (not match-paren-no-use-syntax-pps) (looking-at "\\s("))
      (forward-list 1)
      (backward-char 1))
     ((and (not match-paren-no-use-syntax-pps)(looking-at "\\s)"))
      (forward-char 1) (backward-list 1))
     ;; if match-paren-no-syntax-pps
     ((looking-at "(")
      (ar-parentized-end-atpt))
     ((looking-at ")")
      (ar-parentized-beginning-atpt))
     ((looking-at "\\\[")
      (ar-bracketed-end-atpt))
     ((looking-at "]")
      (ar-bracketed-beginning-atpt))
     ((looking-at "{")
      (ar-braced-end-atpt))
     ((looking-at "}")
      (ar-braced-beginning-atpt))

     (t (self-insert-command 1)))))

(defun ar-list-matches (&optional arg)
  "List results of last searches parentized expressions. "
  (interactive)
  (message "%s" (point))
  (let ((arg (or arg 9))
        erg)
    (dotimes (i arg) (setq erg (concat erg (format "%s %s, " i (match-string-no-properties i)))))
    (message "%s"  erg)
    erg))

(defun ar-in-comment-p ()
  "Return the beginning of current line's comment, if inside. "
  (interactive)
  (save-restriction
    (widen)
    (let* ((pps (parse-partial-sexp (line-beginning-position) (point)))
           (erg (when (nth 4 pps) (nth 8 pps))))
      (unless erg
        (when (and comment-start-skip (looking-at comment-start-skip))
          (setq erg (point))))
      erg)))

(defun diff-autosaved (&optional switches)
  "Diff this file with its auto-saved version or vice versa.
With prefix arg, prompt for diff switches."
  (interactive (list diff-switches))
  (let ((dir default-directory)
        (file (file-name-nondirectory (replace-regexp-in-string "#" "" (buffer-file-name))))
        bak ori)
    (if (string-match "^#" file)
	(setq bak file
	      ori (replace-regexp-in-string "#" "" file))
      (setq bak (concat "#" file "#")
            ori file))
    (setq bak (concat dir bak))
    (setq ori (concat dir ori))
    (diff bak ori switches)))

(defun yank-repeat (arg)
  "With numerical ARG, repeat last yank ARG times. "
  (interactive "p*")
  (dotimes (i arg)
    (insert (car kill-ring))))

(defun yank-repeat-newline (arg)
  "With numerical ARG, repeat last yank ARG times,
also insert a newline. "
  (interactive "p*")
  (dotimes (i arg)
    (insert (concat (car kill-ring) "\n"))))

(defvar function-definition-start-re "^[ \t]*(defun")
(make-variable-buffer-local 'function-definition-start-re)

(defun ar-function-name-atpt ()
  (interactive)
  (let ((erg (progn
               (save-excursion
                 (save-restriction
                   (widen)
                   (beginning-of-defun)
                   (when (looking-at (concat "\\(" function-definition-start-re "\\) +\\(\\(\\w+\\|\\s_+\\)*\\)")))
                   (match-string-no-properties 2))))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-goto-function-name-atpt ()
  "Returns function name beginning position. "
  (interactive)

  (let ((erg (progn
               (beginning-of-defun)
               (when (looking-at (concat "\\(" function-definition-start-re "\\) +\\(\\(\\w+\\|\\s_+\\)*\\)")))
               (goto-char (match-beginning 2)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defvar symbol-definition-start-re "^[ \t]*(\\(defun\\|defvar\\|defcustom\\|defconst\\|setq\\)")
;; (setq symbol-definition-start-re "^[ \t]*(\\(defun\\|defvar\\|defcustom\\|defconst\\|setq\\)")

(defun ar-symbol-name-atpt ()
  "The name of the symbol definition at point. "
  (interactive)
  (let ((erg (progn
               (save-excursion
                 (save-restriction
                   (widen)
                   (re-search-backward "^(" nil (quote move) 1)
                   ;; (when (looking-at (concat "\\(" symbol-definition-start-re "\\) +\\(\\(\\w+\\|\\s_+\\)*\\)"))
                   (when (looking-at "(\\([[:alpha:]-]+\\) +\\([[:graph:]]+\\)")
                     (match-string-no-properties 2)))))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-face-atpt ()
  "Report and return face at point. "
  (interactive)
  (let ((erg (get-char-property (point) 'face)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-byte-compile-file (&optional file)
  "When compiling, it fixes indent and trailing whitespace. "
  (interactive "*")
  (let ((orig (point))
        (tab-always-indent t))
    (save-excursion
      (delete-trailing-whitespace)
      (mark-whole-buffer)
      (indent-for-tab-command)
      (byte-compile-file (or file (buffer-file-name))))
    (goto-char orig)))

(defun until-found (search-string liste)
  "Search liste for search-string until found. " 
   (let ((liste liste) element)
     (while liste
       (if (member search-string (car liste))
           (setq element (car liste) liste nil))
       (setq liste (cdr liste)))
     element))

(defun ar-trace-all-functions (&optional beg end)
  "Get all functions traced, `untrace-all' disables it. "
  (interactive)
  (let ((beg (cond (beg)
		   ((region-active-p)
		    (region-beginning))
		   (t (point-min))))
	(end (cond (end (copy-marker end))
		   ((region-active-p)
		    (copy-marker (region-end)))
		   (t (copy-marker (point-max)))))
        (last-pos-line 1)
        this-pos-line)
    (save-excursion
      (catch 'fehler
        (goto-char beg)
        (while (and (not (eobp))
                    (< (point) end))
          (setq last-pos-line (count-lines 1 (point)))
          (message "%s" last-pos-line)
          (forward-sexp 1)
          ;; in comment
	  (while (nth 4 (parse-partial-sexp (line-beginning-position) (point)))
	    (forward-line 1)
	    (end-of-line))
          (message "point %s" (point))
          (setq this-pos-line (count-lines 1 (point)))
          (save-excursion
            (beginning-of-defun)
            (when (looking-at "(defun")
              (search-forward "(defun" (line-end-position) t 1)
              (skip-chars-forward " \t\r\n")
              (trace-function (symbol-at-point))))))))
  (message "%s" " Finished!"))

(defun ar-max-trace ()
  "Detect greatest count in trace-buffer.

Jumps to position, returns count, a number. "
  (interactive) 
  (let ((erg 0)
        pos)
  (goto-char (point-min))
  (while (re-search-forward "\\(| \\)+\\([0-9]+\\)" nil (quote move) 1)
    (when (< erg (string-to-number (match-string-no-properties 2)))
      (setq erg (string-to-number (match-string-no-properties 2)))
      (setq pos (point))))
  (goto-char pos)
  (when (interactive-p) (message "%s" erg))
  erg))

(provide 'misc-utils)
;;; misc-utils.el ends here
