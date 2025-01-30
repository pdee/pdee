;;; python-components-execute-file --- Runs files -*- lexical-binding: t; -*-


;; Copyright (C) 2010-2023 Andreas Röhler, unless
;; indicated otherwise

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>, unless
;; indicated otherwise

;; Version: 0.1

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

;; Keywords: convenience

;;; Commentary:

;; Delivers a set of functions to return, mover over or
;; manipulate a given THING. THING may be a well known
;; form as word, paragraph, but also a char class as
;; ‘alnum’ or a new defined thing.

;; For example ‘ar-alnum-atpt’ will return all
;; alpha-numerical chars below and around cursor as a
;; string. ‘ar-bounds-of-alnum-atpt’ returns the
;; borders of that string as a list and so on.

;; Presently for a given THING the following is
;; implemented:

;; ar-THING-atpt
;; ar-THING-bounds-atpt
;; ar-THING-beginning-position-atpt
;; ar-THING-end-position-atpt
;; ar-THING-beginning-atpt
;; ar-THING-end-atpt
;; ar-THING-length-atpt
;; ar-THING-copy-atpt
;; ar-THING-kill-atpt
;; ar-THING-forward-atpt
;; ar-THING-backward-atpt
;; ar-THING-transpose-atpt
;; ar-THING-sort-atpt
;; ar-THING-check-atpt

;; Beside of the mentioned above, esists still a couple of
;; functions, whose use is much less probable:

;; ar-THING-slash-atpt
;; ar-THING-doublebackslash-atpt
;; ar-THING-doubleslash-atpt
;; ar-THING-delete-in-region
;; ar-blok-THING-atpt
;; ar-THING-escape-atpt
;; ar-THING-doublequote-atpt
;; ar-THING-doublebackslashparen-atpt
;; ar-THING-dollar-atpt
;; ar-THING-equalize-atpt
;; ar-THING-greaterangle-atpt
;; ar-THING-lesserangle-atpt
;; ar-THING-backslash-atpt
;; ar-THING-brace-atpt
;; ar-THING-bracket-atpt
;; ar-comment-THING-atpt
;; ar-commatize-THING-atpt
;; ar-quote-THING-atpt
;; ar-THING-hyphen-atpt
;; ar-THING-mark-atpt
;; ar-THING-hide-atpt
;; ar-THING-show-atpt
;; ar-THING-hide-show-atpt
;; ar-THING-curvedsinglequote-atpt
;; ar-THING-parentize-atpt
;; ar-THING-separate-atpt
;; ar-THING-singlequote-atpt
;; ar-THING-trim-atpt
;; ar-THING-left-trim-atpt
;; ar-THING-right-trim-atpt
;; ar-underscore-THING-atpt
;; ar-whitespace-THING-atpt

;; To see what's implemented, consult contents of
;; variables at the end of this file as
;; ‘ar-paired-delimit-aktiv’, ‘ar-paired-delimited-passiv’, etc.

;; Call one of the test-functions `C-u ar-th-delimtest'
;; with come chars in scratch-buffer
;; or any else changable buffer to get an impression.

;; The idea comes from Mike Williams
;; <mikew@gopher.dosli.govt.nz>, author of
;; thingatpt.el

;; The goal is to have a set of similar forms. For
;; example, to provide a word with double-quotes around
;; it, call ar-doublequote-word-atpt. In a similar way you
;; may double-quote not just a word, but any object
;; instrumented here as THING. To make parentheses
;; around it call ar-parentize-word-atpt, etc.

;; Move-functions of this package differ from common
;; behaviour in such, as ‘ar-forward-word-atpt’ stops
;; not after THING, but on the last char of
;; THING. That's in order to enable a call of
;; thing-at-point functions at the end
;; position. Otherwise, when cursor stops after word
;; (THING) as does ‘forward-word’, ‘ar-word-atpt’ would return
;; nil.

;; To see other features, maybe try ‘ar-separate-list-atpt’
;; or ‘ar-comment-list-atpt’ while point is inside a
;; list. Try it again with an abstract char-class as
;; [:alnum:], i.e. try ‘ar-comment-alnum-atpt’,
;; ‘ar-brace-alnum-atpt’ etc.

;; This utility comes with test-functions which return
;; the possible results of most functions (exception
;; are the kill-fns). Call th-test, th-mv-test
;; or th-delimtest over text. That-delimtest
;; changes but restores the buffer. Customize the speed
;; of execution via ‘ar-th-test-delay’

;; Diffs to basics of required thingatpt.el:
;; ‘bounds-of-thing-at-point’ is replaced by a new
;; ‘ar-th-bounds’, which now first searches
;; backward. As a consequence several
;; ‘beginning-op-at’ and ‘end-op-at’ constructs had
;; to be rewritten.

;; Behavior in general is not validating; i.e. if you
;; call ar-url-atpt and there is no url, all chars at
;; point may be picked, which could be part of a
;; url. Sometimes, however, a kind of validation may be
;; introduced.

;; If calling from a program `bounds-of-THING-atpt' is
;; recommended as an entry-point. It delivers a list
;; with beg and end positions.

;; In case of trouble, please send me a bug report. Any
;; ideas and comments welcome.

;; You might be interested also to visit Drew Adam's
;; http://www.emacswiki.org/emacs/thingatpt+.el
;; which predates this approach and was helpful writing it.

;; Thing-at-point delivers a portion of the
;; buffer. Thats useful, if THING is not as easy to grasp as a word.
;; For example the first string of an objekt like:

;; ("4[[:punct:] \t\r\n]? [[:punct:] \t\r\n]?C[[:punct:] \t\r\n]?.[[:punct:] \t\r\n]?2[[:punct:] \t\r\n]?4[[:punct:] \t\r\n]?6[[:punct:] \t\r\n]?4[[:punct:] \t\r\n]?/[[:punct:] \t\r\n]?0[[:punct:] \t\r\n]?3[[:punct:] \t\r\n]? [[:punct:] \t\r\n]?B" . "blah blub B")

;; Remove comments and put the cursor somewhere into the first
;; string:
;; ‘ar-doublequoted-atpt’ will return it, copied into the kill-ring,
;; enabling yanking it and a lot of further actions.

;; ‘ar-doublequoted-atpt’ here is to
;; (global-set-key [(super \")] 'ar-doublequoted-atpt)

;; alike a range of similar commands exist:
;; (global-set-key [(super \')] 'ar-singlequoted-atpt)
;; (global-set-key [(super \))] 'ar-parentized-atpt)
;; (global-set-key [(super \/)] 'ar-slashed-atpt)
;; (global-set-key [(super \\)] 'ar-backslashed-atpt)
;; (global-set-key [(super \])] 'ar-bracketed-atpt)
;; (global-set-key [(super \})] 'ar-braced-atpt)

;; So far THING is simply picked up.

;; Different approach combines copying, deleting with delimiting

;; if region is active:

;; (global-set-key [(control c) (\")] 'ar-doublequote-or-copy-atpt)

;; will provide doublequotes at beginning and end of region.

;; With negative argument it deletes the doublequoted portion under
;; point.

;; Without any argument these functions return as their simplier
;; counterparts

;; With universal argument [(control u)] delimiters --i.e. doublequotes, slashes, whatever-- are stripped.

;;

;; THING as a buffer substring is determined by
;; move-functions specified for thingatpt, called
;; beginning-op-at and end-op-at. Point is stored
;; after move, beginning and end delivered as pair: as
;; consed bounds-of-thing. It's easy to write your own
;; thing-at-point functions that way. You need the
;; caller and both move forms:

;; (defun MY-FORM-atpt (&optional arg)
;;   " "
;;   (interactive "p")
;;   (ar-th 'MY-FORM arg))

;; (put 'MY-FORM 'beginning-op-at
;;            (lambda () MY-FORWARD-MOVE-CODE))

;; (put 'MY-FORM 'end-op-at
;;      (lambda () MY-BACKWARD-MOVE-CODE))

;; For example if you want to pick all chars at point
;; which are written between a string "AAA" and a
;; "BBB", which may exist as
;; AAA Luckily detected a lot of things! BBB
;; After evaluation of
;; (put 'MY-FORM 'beginning-op-at
;;      (lambda ()
;;        (search-backward "AAA" nil 'move 1)
;;        ;; step chars of search expression back
;;        (forward-char 3)))
;;
;; (put 'MY-FORM 'end-op-at
;;      (lambda ()
;;        (search-forward "BBB" nil 'move 1)
;;        (forward-char -3)))
;; together with the functions definition above, it's ready.
;; M-x MY-FORM-atpt
;; (while point inside) you should see:
;; " Luckily detected a lot of things! "
;; in the minibuffer.

;; Some keys

;; (define-key emacs-lisp-mode-map [(control c)(q)] 'ar-parentized-forward-atpt)
;; (define-key emacs-lisp-mode-map [(super c)())] 'ar-symbol-parentize-atpt)
;; (define-key emacs-lisp-mode-map [(super c)(n)] 'ar-region-parentize-atpt)
;; (global-set-key [(control c)(<)] 'ar-lesserangle-or-copy-atpt)
;; (global-set-key [(control c)(>)] 'ar-greaterangle-or-copy-atpt)
;; (global-set-key [(control c)(")] 'ar-doublequote-or-copy-atpt)
;; (global-set-key [(control c)(')] 'ar-singlequote-or-copy-atpt)
;; (global-set-key [(control c)(()] 'ar-paren-atpt)
;; (global-set-key [(control c)())] 'ar-parentize-or-copy-atpt)
;; (global-set-key [(control c)(/)] 'ar-slash-or-copy-atpt)

;; Execute file given

(defun py-execute-file-ipython (filename)
  "Send file to IPython interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil nil "ipython" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-ipython3 (filename)
  "Send file to IPython3 interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil nil "ipython3" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-jython (filename)
  "Send file to Jython interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil nil "jython" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-python (filename)
  "Send file to Python interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil nil "python" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-python2 (filename)
  "Send file to Python2 interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil nil "python2" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-python3 (filename)
  "Send file to Python3 interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil nil "python3" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-pypy (filename)
  "Send file to PyPy interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil nil "pypy" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file- (filename)
  "Send file to  interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil nil "" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-ipython-dedicated (filename)
  "Send file to a dedicatedIPython interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil t "ipython" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-ipython3-dedicated (filename)
  "Send file to a dedicatedIPython3 interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil t "ipython3" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-jython-dedicated (filename)
  "Send file to a dedicatedJython interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil t "jython" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-python-dedicated (filename)
  "Send file to a dedicatedPython interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil t "python" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-python2-dedicated (filename)
  "Send file to a dedicatedPython2 interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil t "python2" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-python3-dedicated (filename)
  "Send file to a dedicatedPython3 interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil t "python3" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-pypy-dedicated (filename)
  "Send file to a dedicatedPyPy interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil t "pypy" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file--dedicated (filename)
  "Send file to a dedicated interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil t "" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(provide 'python-components-execute-file)
;;; python-components-execute-file.el ends here
