;;; thingatpt-utils-base.el --- th-at-point edit functions

;; Copyright (C) 2010 Andreas Roehler, unless
;; indicated otherwise

;; Author: Andreas Roehler <andreas.roehler@easy-emacs.de>, unless
;; indicated otherwise

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
;; `alnum' or a new defined thing.

;; For example `ar-alnum-atpt' will return all
;; alpha-numerical chars below and around cursor as a
;; string. `ar-bounds-of-alnum-atpt' returns the
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
;; ar-THING-double-backslash-atpt
;; ar-THING-doubleslash-atpt
;; ar-THING-delete-in-region
;; ar-blok-THING-atpt
;; ar-THING-escape-atpt
;; ar-THING-doublequote-atpt
;; ar-THING-doubleslash-paren-atpt
;; ar-THING-slashparen-atpt
;; ar-THING-dollar-atpt
;; ar-THING-equalize-atpt
;; ar-THING-greater-angle-atpt
;; ar-THING-lesser-angle-atpt
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
;; ar-THING-left-right-singlequote-atpt
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
;; `ar-atpt-delimlist', `ar-atpt-delimited-list', etc.

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
;; behaviour in such, as `ar-forward-word-atpt' stops
;; not after THING, but on the last char of
;; THING. That's in order to enable a call of
;; thing-at-point functions at the end
;; position. Otherwise, when cursor stops after word
;; (THING) as does `forward-word', `ar-word-atpt' would return
;; nil. 

;; To see other features, maybe try `ar-separate-list-atpt'
;; or `ar-comment-list-atpt' while point is inside a
;; list. Try it again with an abstract char-class as
;; [:alnum:], i.e. try `ar-comment-alnum-atpt',
;; `ar-brace-alnum-atpt' etc.

;; This utility comes with test-functions which return
;; the possible results of most functions (exception
;; are the kill-fns). Call th-test, th-mv-test
;; or th-delimtest over text. That-delimtest
;; changes but restores the buffer. Customize the speed
;; of execution via `ar-th-test-delay' 

;; Diffs to basics of required thingatpt.el:
;; `bounds-of-thing-at-point' is replaced by a new
;; `ar-th-bounds', which now first searches
;; backward. As a consequence several
;; `beginning-op-at' and `end-op-at' constructs had
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
;; `ar-doublequoted-atpt' will return it, copied into the kill-ring,
;; enabling yanking it and a lot of further actions.

;; `ar-doublequoted-atpt' here is to
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
;; (global-set-key [(control c)(<)] 'ar-lesser-angle-or-copy-atpt)
;; (global-set-key [(control c)(>)] 'ar-greater-angle-or-copy-atpt)
;; (global-set-key [(control c)(")] 'ar-doublequote-or-copy-atpt)
;; (global-set-key [(control c)(')] 'ar-singlequote-or-copy-atpt)
;; (global-set-key [(control c)(()] 'ar-paren-atpt)
;; (global-set-key [(control c)())] 'ar-parentize-or-copy-atpt)
;; (global-set-key [(control c)(/)] 'ar-slash-or-copy-atpt)


;;; Code:

(require 'beg-end)
(require 'sh-beg-end)
(require 'misc-utils)
(require 'thingatpt-highlight)

(when (featurep 'xemacs) (require 'overlay))

(defcustom match-paren-no-use-syntax-pps nil
  "If `match-paren' should avoid scanning lists according to syntax but search regexp based. "
  :type 'boolean
  :group 'convenience)

(defcustom thing-copy-region t
  "If a found THING should be copied into the kill-ring. "
  :type 'boolean
  :group 'convenience)

(defcustom ar-newlines-separate-after 1 
  "How many newlines at-th-separate should insert at the end" 
  
  :type 'number 
  :group 'convenience)

(defcustom ar-newlines-separate-before 1 
  "How many newlines at-th-separate should insert at the end" 
  
  :type 'number 
  :group 'convenience)

;; (defvar th-orig 0
;; "Correct orig according to delimiter-length")

(when (featurep 'xemacs)
  (defcustom alnum "\\sw"
    "Rexexp to specify the character class 
Follows word-syntax. Use something like
   \"[a-zA-ZäöüßÄÖÜ0-9]\" maybe instead.
`unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'convenience))

(when (featurep 'xemacs)
  (defcustom alpha "[a-zA-ZäöüßÄÖÜ]"
    "Rexexp to specify the character class 
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'convenience))

(when (featurep 'xemacs)
  (defcustom ascii "[\000-\177]"
    "Rexexp to specify the character class 
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'convenience))

(when (featurep 'xemacs)
  (defcustom blank "[ \t]"
    "Rexexp to specify the character class 
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'convenience))

(when (featurep 'xemacs)
  (defcustom cntrl "[\000-\006]\016-\037]"
    "Rexexp to specify the character class 
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'convenience))

(when (featurep 'xemacs)
  (defcustom digit "[0-9]"
    "Rexexp to specify the character class 
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'convenience))

(when (featurep 'xemacs)
  (defcustom graph "[\041-\177\241-\377]"
    "Rexexp to specify the character class 
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'convenience))

(when (featurep 'xemacs)
  (defcustom lower "[a-zäöüß]"
    "Rexexp to specify the character class 
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'convenience))

(when (featurep 'xemacs)
  (defcustom multibyte "[.]"
    "Rexexp to specify the character class 
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'convenience))

(when (featurep 'xemacs)
  (defcustom nonascii "[^\040-\177]"
    "Rexexp to specify the character class 
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'convenience))

(when (featurep 'xemacs)
  (defcustom print "[\041-\177\241-\377]"
    "Rexexp to specify the character class 
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'convenience))

(when (featurep 'xemacs)
  (defcustom punct "[.,-_:;?!]"
    "Rexexp to specify the character class 
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'convenience))

(when (featurep 'xemacs)
  (defcustom space "[ \t]"
    "Rexexp to specify the character class 
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'convenience))

(when (featurep 'xemacs)
  (defcustom unibyte "[.]"
    "Rexexp to specify the character class 
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'convenience))

(when (featurep 'xemacs)
  (defcustom upper "[A-ZÄÖÜ]"
    "Rexexp to specify the character class 
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'convenience))

(when (featurep 'xemacs)
  (defcustom xdigit "[0-9.,]"
    "Rexexp to specify the character class 
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'convenience))

(require 'thingatpt-highlight);; ar-insert-put-classes start

;; Alnum

(put 'alnum 'beginning-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
             (while 
                 (and (not (bobp))(looking-at alnum))
               (forward-char -1))
             (unless (bobp) (forward-char 1))) 
         (when
             (looking-at "[[:alnum:]]")
           (skip-chars-backward "[:alnum:]")(point)))))

(put 'alnum 'end-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn (forward-char 1)
                  (while 
                      (looking-at alnum)
                    (forward-char 1)))
	 (skip-chars-forward "[:alnum:]")(point))))

(put 'alnum 'forward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at alnum)
		    (while 
			(looking-at alnum)
		      (forward-char 1)))
		   (t (while (not (looking-at alnum))
			(forward-char 1))
		      (while 
			  (looking-at alnum)
			(forward-char 1))))
	     (forward-char -1))
         (skip-chars-forward "^[:alnum:]")(point))))

(put 'alnum 'backward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at alnum)
		    (while 
			(looking-at alnum)
		      (forward-char -1)))
		   (t (while (not (looking-at alnum))
			(forward-char -1))
		      (while 
			  (looking-at alnum)
			(forward-char -1))))
	     (forward-char 1))
         (if (looking-at "[[:alnum:]]")
             (skip-chars-backward "[:alnum:]")
           (skip-chars-backward "^[:alnum:]")
           (forward-char -1)
           (skip-chars-backward "[:alnum:]"))(point))))


;; Alpha

(put 'alpha 'beginning-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
             (while 
                 (and (not (bobp))(looking-at alpha))
               (forward-char -1))
             (unless (bobp) (forward-char 1))) 
         (when
             (looking-at "[[:alpha:]]")
           (skip-chars-backward "[:alpha:]")(point)))))

(put 'alpha 'end-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn (forward-char 1)
                  (while 
                      (looking-at alpha)
                    (forward-char 1)))
	 (skip-chars-forward "[:alpha:]")(point))))

(put 'alpha 'forward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at alpha)
		    (while 
			(looking-at alpha)
		      (forward-char 1)))
		   (t (while (not (looking-at alpha))
			(forward-char 1))
		      (while 
			  (looking-at alpha)
			(forward-char 1))))
	     (forward-char -1))
         (skip-chars-forward "^[:alpha:]")(point))))

(put 'alpha 'backward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at alpha)
		    (while 
			(looking-at alpha)
		      (forward-char -1)))
		   (t (while (not (looking-at alpha))
			(forward-char -1))
		      (while 
			  (looking-at alpha)
			(forward-char -1))))
	     (forward-char 1))
         (if (looking-at "[[:alpha:]]")
             (skip-chars-backward "[:alpha:]")
           (skip-chars-backward "^[:alpha:]")
           (forward-char -1)
           (skip-chars-backward "[:alpha:]"))(point))))


;; Ascii

(put 'ascii 'beginning-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
             (while 
                 (and (not (bobp))(looking-at ascii))
               (forward-char -1))
             (unless (bobp) (forward-char 1))) 
         (when
             (looking-at "[[:ascii:]]")
           (skip-chars-backward "[:ascii:]")(point)))))

(put 'ascii 'end-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn (forward-char 1)
                  (while 
                      (looking-at ascii)
                    (forward-char 1)))
	 (skip-chars-forward "[:ascii:]")(point))))

(put 'ascii 'forward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at ascii)
		    (while 
			(looking-at ascii)
		      (forward-char 1)))
		   (t (while (not (looking-at ascii))
			(forward-char 1))
		      (while 
			  (looking-at ascii)
			(forward-char 1))))
	     (forward-char -1))
         (skip-chars-forward "^[:ascii:]")(point))))

(put 'ascii 'backward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at ascii)
		    (while 
			(looking-at ascii)
		      (forward-char -1)))
		   (t (while (not (looking-at ascii))
			(forward-char -1))
		      (while 
			  (looking-at ascii)
			(forward-char -1))))
	     (forward-char 1))
         (if (looking-at "[[:ascii:]]")
             (skip-chars-backward "[:ascii:]")
           (skip-chars-backward "^[:ascii:]")
           (forward-char -1)
           (skip-chars-backward "[:ascii:]"))(point))))


;; Blank

(put 'blank 'beginning-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
             (while 
                 (and (not (bobp))(looking-at blank))
               (forward-char -1))
             (unless (bobp) (forward-char 1))) 
         (when
             (looking-at "[[:blank:]]")
           (skip-chars-backward "[:blank:]")(point)))))

(put 'blank 'end-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn (forward-char 1)
                  (while 
                      (looking-at blank)
                    (forward-char 1)))
	 (skip-chars-forward "[:blank:]")(point))))

(put 'blank 'forward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at blank)
		    (while 
			(looking-at blank)
		      (forward-char 1)))
		   (t (while (not (looking-at blank))
			(forward-char 1))
		      (while 
			  (looking-at blank)
			(forward-char 1))))
	     (forward-char -1))
         (skip-chars-forward "^[:blank:]")(point))))

(put 'blank 'backward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at blank)
		    (while 
			(looking-at blank)
		      (forward-char -1)))
		   (t (while (not (looking-at blank))
			(forward-char -1))
		      (while 
			  (looking-at blank)
			(forward-char -1))))
	     (forward-char 1))
         (if (looking-at "[[:blank:]]")
             (skip-chars-backward "[:blank:]")
           (skip-chars-backward "^[:blank:]")
           (forward-char -1)
           (skip-chars-backward "[:blank:]"))(point))))


;; Cntrl

(put 'cntrl 'beginning-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
             (while 
                 (and (not (bobp))(looking-at cntrl))
               (forward-char -1))
             (unless (bobp) (forward-char 1))) 
         (when
             (looking-at "[[:cntrl:]]")
           (skip-chars-backward "[:cntrl:]")(point)))))

(put 'cntrl 'end-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn (forward-char 1)
                  (while 
                      (looking-at cntrl)
                    (forward-char 1)))
	 (skip-chars-forward "[:cntrl:]")(point))))

(put 'cntrl 'forward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at cntrl)
		    (while 
			(looking-at cntrl)
		      (forward-char 1)))
		   (t (while (not (looking-at cntrl))
			(forward-char 1))
		      (while 
			  (looking-at cntrl)
			(forward-char 1))))
	     (forward-char -1))
         (skip-chars-forward "^[:cntrl:]")(point))))

(put 'cntrl 'backward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at cntrl)
		    (while 
			(looking-at cntrl)
		      (forward-char -1)))
		   (t (while (not (looking-at cntrl))
			(forward-char -1))
		      (while 
			  (looking-at cntrl)
			(forward-char -1))))
	     (forward-char 1))
         (if (looking-at "[[:cntrl:]]")
             (skip-chars-backward "[:cntrl:]")
           (skip-chars-backward "^[:cntrl:]")
           (forward-char -1)
           (skip-chars-backward "[:cntrl:]"))(point))))


;; Digit

(put 'digit 'beginning-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
             (while 
                 (and (not (bobp))(looking-at digit))
               (forward-char -1))
             (unless (bobp) (forward-char 1))) 
         (when
             (looking-at "[[:digit:]]")
           (skip-chars-backward "[:digit:]")(point)))))

(put 'digit 'end-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn (forward-char 1)
                  (while 
                      (looking-at digit)
                    (forward-char 1)))
	 (skip-chars-forward "[:digit:]")(point))))

(put 'digit 'forward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at digit)
		    (while 
			(looking-at digit)
		      (forward-char 1)))
		   (t (while (not (looking-at digit))
			(forward-char 1))
		      (while 
			  (looking-at digit)
			(forward-char 1))))
	     (forward-char -1))
         (skip-chars-forward "^[:digit:]")(point))))

(put 'digit 'backward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at digit)
		    (while 
			(looking-at digit)
		      (forward-char -1)))
		   (t (while (not (looking-at digit))
			(forward-char -1))
		      (while 
			  (looking-at digit)
			(forward-char -1))))
	     (forward-char 1))
         (if (looking-at "[[:digit:]]")
             (skip-chars-backward "[:digit:]")
           (skip-chars-backward "^[:digit:]")
           (forward-char -1)
           (skip-chars-backward "[:digit:]"))(point))))


;; Graph

(put 'graph 'beginning-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
             (while 
                 (and (not (bobp))(looking-at graph))
               (forward-char -1))
             (unless (bobp) (forward-char 1))) 
         (when
             (looking-at "[[:graph:]]")
           (skip-chars-backward "[:graph:]")(point)))))

(put 'graph 'end-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn (forward-char 1)
                  (while 
                      (looking-at graph)
                    (forward-char 1)))
	 (skip-chars-forward "[:graph:]")(point))))

(put 'graph 'forward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at graph)
		    (while 
			(looking-at graph)
		      (forward-char 1)))
		   (t (while (not (looking-at graph))
			(forward-char 1))
		      (while 
			  (looking-at graph)
			(forward-char 1))))
	     (forward-char -1))
         (skip-chars-forward "^[:graph:]")(point))))

(put 'graph 'backward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at graph)
		    (while 
			(looking-at graph)
		      (forward-char -1)))
		   (t (while (not (looking-at graph))
			(forward-char -1))
		      (while 
			  (looking-at graph)
			(forward-char -1))))
	     (forward-char 1))
         (if (looking-at "[[:graph:]]")
             (skip-chars-backward "[:graph:]")
           (skip-chars-backward "^[:graph:]")
           (forward-char -1)
           (skip-chars-backward "[:graph:]"))(point))))


;; Lower

(put 'lower 'beginning-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
             (while 
                 (and (not (bobp))(looking-at lower))
               (forward-char -1))
             (unless (bobp) (forward-char 1))) 
         (when
             (looking-at "[[:lower:]]")
           (skip-chars-backward "[:lower:]")(point)))))

(put 'lower 'end-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn (forward-char 1)
                  (while 
                      (looking-at lower)
                    (forward-char 1)))
	 (skip-chars-forward "[:lower:]")(point))))

(put 'lower 'forward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at lower)
		    (while 
			(looking-at lower)
		      (forward-char 1)))
		   (t (while (not (looking-at lower))
			(forward-char 1))
		      (while 
			  (looking-at lower)
			(forward-char 1))))
	     (forward-char -1))
         (skip-chars-forward "^[:lower:]")(point))))

(put 'lower 'backward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at lower)
		    (while 
			(looking-at lower)
		      (forward-char -1)))
		   (t (while (not (looking-at lower))
			(forward-char -1))
		      (while 
			  (looking-at lower)
			(forward-char -1))))
	     (forward-char 1))
         (if (looking-at "[[:lower:]]")
             (skip-chars-backward "[:lower:]")
           (skip-chars-backward "^[:lower:]")
           (forward-char -1)
           (skip-chars-backward "[:lower:]"))(point))))


;; Nonascii

(put 'nonascii 'beginning-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
             (while 
                 (and (not (bobp))(looking-at nonascii))
               (forward-char -1))
             (unless (bobp) (forward-char 1))) 
         (when
             (looking-at "[[:nonascii:]]")
           (skip-chars-backward "[:nonascii:]")(point)))))

(put 'nonascii 'end-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn (forward-char 1)
                  (while 
                      (looking-at nonascii)
                    (forward-char 1)))
	 (skip-chars-forward "[:nonascii:]")(point))))

(put 'nonascii 'forward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at nonascii)
		    (while 
			(looking-at nonascii)
		      (forward-char 1)))
		   (t (while (not (looking-at nonascii))
			(forward-char 1))
		      (while 
			  (looking-at nonascii)
			(forward-char 1))))
	     (forward-char -1))
         (skip-chars-forward "^[:nonascii:]")(point))))

(put 'nonascii 'backward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at nonascii)
		    (while 
			(looking-at nonascii)
		      (forward-char -1)))
		   (t (while (not (looking-at nonascii))
			(forward-char -1))
		      (while 
			  (looking-at nonascii)
			(forward-char -1))))
	     (forward-char 1))
         (if (looking-at "[[:nonascii:]]")
             (skip-chars-backward "[:nonascii:]")
           (skip-chars-backward "^[:nonascii:]")
           (forward-char -1)
           (skip-chars-backward "[:nonascii:]"))(point))))


;; Print

(put 'print 'beginning-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
             (while 
                 (and (not (bobp))(looking-at print))
               (forward-char -1))
             (unless (bobp) (forward-char 1))) 
         (when
             (looking-at "[[:print:]]")
           (skip-chars-backward "[:print:]")(point)))))

(put 'print 'end-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn (forward-char 1)
                  (while 
                      (looking-at print)
                    (forward-char 1)))
	 (skip-chars-forward "[:print:]")(point))))

(put 'print 'forward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at print)
		    (while 
			(looking-at print)
		      (forward-char 1)))
		   (t (while (not (looking-at print))
			(forward-char 1))
		      (while 
			  (looking-at print)
			(forward-char 1))))
	     (forward-char -1))
         (skip-chars-forward "^[:print:]")(point))))

(put 'print 'backward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at print)
		    (while 
			(looking-at print)
		      (forward-char -1)))
		   (t (while (not (looking-at print))
			(forward-char -1))
		      (while 
			  (looking-at print)
			(forward-char -1))))
	     (forward-char 1))
         (if (looking-at "[[:print:]]")
             (skip-chars-backward "[:print:]")
           (skip-chars-backward "^[:print:]")
           (forward-char -1)
           (skip-chars-backward "[:print:]"))(point))))


;; Punct

(put 'punct 'beginning-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
             (while 
                 (and (not (bobp))(looking-at punct))
               (forward-char -1))
             (unless (bobp) (forward-char 1))) 
         (when
             (looking-at "[[:punct:]]")
           (skip-chars-backward "[:punct:]")(point)))))

(put 'punct 'end-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn (forward-char 1)
                  (while 
                      (looking-at punct)
                    (forward-char 1)))
	 (skip-chars-forward "[:punct:]")(point))))

(put 'punct 'forward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at punct)
		    (while 
			(looking-at punct)
		      (forward-char 1)))
		   (t (while (not (looking-at punct))
			(forward-char 1))
		      (while 
			  (looking-at punct)
			(forward-char 1))))
	     (forward-char -1))
         (skip-chars-forward "^[:punct:]")(point))))

(put 'punct 'backward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at punct)
		    (while 
			(looking-at punct)
		      (forward-char -1)))
		   (t (while (not (looking-at punct))
			(forward-char -1))
		      (while 
			  (looking-at punct)
			(forward-char -1))))
	     (forward-char 1))
         (if (looking-at "[[:punct:]]")
             (skip-chars-backward "[:punct:]")
           (skip-chars-backward "^[:punct:]")
           (forward-char -1)
           (skip-chars-backward "[:punct:]"))(point))))


;; Space

(put 'space 'beginning-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
             (while 
                 (and (not (bobp))(looking-at space))
               (forward-char -1))
             (unless (bobp) (forward-char 1))) 
         (when
             (looking-at "[[:space:]]")
           (skip-chars-backward "[:space:]")(point)))))

(put 'space 'end-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn (forward-char 1)
                  (while 
                      (looking-at space)
                    (forward-char 1)))
	 (skip-chars-forward "[:space:]")(point))))

(put 'space 'forward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at space)
		    (while 
			(looking-at space)
		      (forward-char 1)))
		   (t (while (not (looking-at space))
			(forward-char 1))
		      (while 
			  (looking-at space)
			(forward-char 1))))
	     (forward-char -1))
         (skip-chars-forward "^[:space:]")(point))))

(put 'space 'backward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at space)
		    (while 
			(looking-at space)
		      (forward-char -1)))
		   (t (while (not (looking-at space))
			(forward-char -1))
		      (while 
			  (looking-at space)
			(forward-char -1))))
	     (forward-char 1))
         (if (looking-at "[[:space:]]")
             (skip-chars-backward "[:space:]")
           (skip-chars-backward "^[:space:]")
           (forward-char -1)
           (skip-chars-backward "[:space:]"))(point))))


;; Upper

(put 'upper 'beginning-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
             (while 
                 (and (not (bobp))(looking-at upper))
               (forward-char -1))
             (unless (bobp) (forward-char 1))) 
         (when
             (looking-at "[[:upper:]]")
           (skip-chars-backward "[:upper:]")(point)))))

(put 'upper 'end-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn (forward-char 1)
                  (while 
                      (looking-at upper)
                    (forward-char 1)))
	 (skip-chars-forward "[:upper:]")(point))))

(put 'upper 'forward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at upper)
		    (while 
			(looking-at upper)
		      (forward-char 1)))
		   (t (while (not (looking-at upper))
			(forward-char 1))
		      (while 
			  (looking-at upper)
			(forward-char 1))))
	     (forward-char -1))
         (skip-chars-forward "^[:upper:]")(point))))

(put 'upper 'backward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at upper)
		    (while 
			(looking-at upper)
		      (forward-char -1)))
		   (t (while (not (looking-at upper))
			(forward-char -1))
		      (while 
			  (looking-at upper)
			(forward-char -1))))
	     (forward-char 1))
         (if (looking-at "[[:upper:]]")
             (skip-chars-backward "[:upper:]")
           (skip-chars-backward "^[:upper:]")
           (forward-char -1)
           (skip-chars-backward "[:upper:]"))(point))))


;; Xdigit

(put 'xdigit 'beginning-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
             (while 
                 (and (not (bobp))(looking-at xdigit))
               (forward-char -1))
             (unless (bobp) (forward-char 1))) 
         (when
             (looking-at "[[:xdigit:]]")
           (skip-chars-backward "[:xdigit:]")(point)))))

(put 'xdigit 'end-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn (forward-char 1)
                  (while 
                      (looking-at xdigit)
                    (forward-char 1)))
	 (skip-chars-forward "[:xdigit:]")(point))))

(put 'xdigit 'forward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at xdigit)
		    (while 
			(looking-at xdigit)
		      (forward-char 1)))
		   (t (while (not (looking-at xdigit))
			(forward-char 1))
		      (while 
			  (looking-at xdigit)
			(forward-char 1))))
	     (forward-char -1))
         (skip-chars-forward "^[:xdigit:]")(point))))

(put 'xdigit 'backward-op-at
     (lambda ()
       (if (featurep 'xemacs)
           (progn
	     (cond ((looking-at xdigit)
		    (while 
			(looking-at xdigit)
		      (forward-char -1)))
		   (t (while (not (looking-at xdigit))
			(forward-char -1))
		      (while 
			  (looking-at xdigit)
			(forward-char -1))))
	     (forward-char 1))
         (if (looking-at "[[:xdigit:]]")
             (skip-chars-backward "[:xdigit:]")
           (skip-chars-backward "^[:xdigit:]")
           (forward-char -1)
           (skip-chars-backward "[:xdigit:]"))(point))))


;; Delimited forms start

;; Braced

(put 'braced 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "{"))
           (list (match-beginning 0)(match-end 0))
         (beginning-of-form-base "{" "}" nil 'move 1 nil 'ar-syntax t))))

(put 'braced 'end-op-at
     (lambda ()
       (when (looking-at "{")
         (goto-char (match-end 0))
         (end-of-form-base "{" "}" nil 'move 1 nil 'ar-syntax t))))

(put 'braced 'forward-op-at
     (lambda ()
       (skip-chars-forward "^{")(point)))

(put 'braced 'backward-op-at
     (lambda ()
       (search-backward "}" nil 'move 1)))

;; Bracketed

(put 'bracketed 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\\\\\\\["))
           (list (match-beginning 0)(match-end 0))
         (beginning-of-form-base "\\\[" "]" nil 'move 1 nil t 'ar-syntax t))))

(put 'bracketed 'end-op-at
     (lambda ()
       (when (looking-at "\\\\\\\[")
         (goto-char (match-end 0))
         (end-of-form-base "\\\[" "]" nil 'move 1 nil t 'ar-syntax t))))

(put 'bracketed 'forward-op-at
     (lambda ()
       (skip-chars-forward "^\\\[")(point)))

(put 'bracketed 'backward-op-at
     (lambda ()
       (search-backward "]" nil 'move 1)))

;; Lesser-Angled

(put 'lesser-angled 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "<"))
           (list (match-beginning 0)(match-end 0))
         (beginning-of-form-base "<" ">" nil 'move 1 nil 'ar-syntax t))))

(put 'lesser-angled 'end-op-at
     (lambda ()
       (when (looking-at "<")
         (goto-char (match-end 0))
         (end-of-form-base "<" ">" nil 'move 1 nil 'ar-syntax t))))

(put 'lesser-angled 'forward-op-at
     (lambda ()
       (skip-chars-forward "^<")(point)))

(put 'lesser-angled 'backward-op-at
     (lambda ()
       (search-backward ">" nil 'move 1)))

;; Greater-Angled

(put 'greater-angled 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at ">"))
           (list (match-beginning 0)(match-end 0))
         (beginning-of-form-base ">" "<" nil 'move 1 nil 'ar-syntax t))))

(put 'greater-angled 'end-op-at
     (lambda ()
       (when (looking-at ">")
         (goto-char (match-end 0))
         (end-of-form-base ">" "<" nil 'move 1 nil 'ar-syntax t))))

(put 'greater-angled 'forward-op-at
     (lambda ()
       (skip-chars-forward "^>")(point)))

(put 'greater-angled 'backward-op-at
     (lambda ()
       (search-backward "<" nil 'move 1)))

;; Left-Right-Singlequoted

(put 'left-right-singlequoted 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "‘"))
           (list (match-beginning 0)(match-end 0))
         (beginning-of-form-base "‘" "’" nil 'move 1 nil 'ar-syntax t))))

(put 'left-right-singlequoted 'end-op-at
     (lambda ()
       (when (looking-at "‘")
         (goto-char (match-end 0))
         (end-of-form-base "‘" "’" nil 'move 1 nil 'ar-syntax t))))

(put 'left-right-singlequoted 'forward-op-at
     (lambda ()
       (skip-chars-forward "^‘")(point)))

(put 'left-right-singlequoted 'backward-op-at
     (lambda ()
       (search-backward "’" nil 'move 1)))

;; Parentized

(put 'parentized 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "("))
           (list (match-beginning 0)(match-end 0))
         (beginning-of-form-base "(" ")" nil 'move 1 nil 'ar-syntax t))))

(put 'parentized 'end-op-at
     (lambda ()
       (when (looking-at "(")
         (goto-char (match-end 0))
         (end-of-form-base "(" ")" nil 'move 1 nil 'ar-syntax t))))

(put 'parentized 'forward-op-at
     (lambda ()
       (skip-chars-forward "^(")(point)))

(put 'parentized 'backward-op-at
     (lambda ()
       (search-backward ")" nil 'move 1)))

;; Delimited forms end

;; ar-unpaired-delimited-raw start

;; Backslashed
(put 'backslashed 'beginning-op-at
     (lambda ()
       (let* ((backslashed "\\")
              (bounds (ar-in-delimiter-base backslashed)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'backslashed 'end-op-at
     (lambda ()
       (let* ((backslashed "\\")
              (erg (looking-at backslashed)))
         (when erg 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base backslashed backslashed nil 'move 1 nil t)))
         erg)))

(put 'backslashed 'forward-op-at
     (lambda ()
       (let ((backslashed "\\")
             bounds)
         (while (and (not (eobp))(progn (forward-char 1) t)(skip-chars-forward "^\\") (not (car-safe (ar-in-delimiter-base backslashed)))))
         (unless (eobp)
           (setq bounds (end-of-form-base backslashed backslashed nil 'move 1 nil t))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'backslashed 'backward-op-at
     (lambda ()
       (let ((backslashed "\\")
             bounds)
         (while (and (not (eobp))
                     (progn (forward-char -1) t)
                     (skip-chars-backward "^\\") 
                     (not (car-safe (ar-in-delimiter-base backslashed)))))
         (unless (bobp)
           (setq bounds (beginning-of-form-base backslashed backslashed nil 'move 1 nil t))
           (ignore-errors (goto-char (car bounds))))
         bounds)))

;; Dollared
(put 'dollared 'beginning-op-at
     (lambda ()
       (let* ((dollared "\\$")
              (bounds (ar-in-delimiter-base dollared)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'dollared 'end-op-at
     (lambda ()
       (let* ((dollared "\\$")
              (erg (looking-at dollared)))
         (when erg 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base dollared dollared nil 'move 1 nil t)))
         erg)))

(put 'dollared 'forward-op-at
     (lambda ()
       (let ((dollared "\\$")
             bounds)
         (while (and (not (eobp))(progn (forward-char 1) t)(skip-chars-forward "^\\$") (not (car-safe (ar-in-delimiter-base dollared)))))
         (unless (eobp)
           (setq bounds (end-of-form-base dollared dollared nil 'move 1 nil t))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'dollared 'backward-op-at
     (lambda ()
       (let ((dollared "\\$")
             bounds)
         (while (and (not (eobp))
                     (progn (forward-char -1) t)
                     (skip-chars-backward "^\\$") 
                     (not (car-safe (ar-in-delimiter-base dollared)))))
         (unless (bobp)
           (setq bounds (beginning-of-form-base dollared dollared nil 'move 1 nil t))
           (ignore-errors (goto-char (car bounds))))
         bounds)))

;; Doublequoted
(put 'doublequoted 'beginning-op-at
     (lambda ()
       (let* ((doublequoted "\"")
              (bounds (ar-in-delimiter-base doublequoted)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'doublequoted 'end-op-at
     (lambda ()
       (let* ((doublequoted "\"")
              (erg (looking-at doublequoted)))
         (when erg 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base doublequoted doublequoted nil 'move 1 nil t 'ar-escaped)))
         erg)))

(put 'doublequoted 'forward-op-at
     (lambda ()
       (let ((doublequoted "\"")
             bounds)
         (while (and (not (eobp))(progn (forward-char 1) t)(skip-chars-forward "^\"") (not (car-safe (ar-in-delimiter-base doublequoted)))))
         (unless (eobp)
           (setq bounds (end-of-form-base doublequoted doublequoted nil 'move 1 nil t))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'doublequoted 'backward-op-at
     (lambda ()
       (let ((doublequoted "\"")
             bounds)
         (while (and (not (eobp))
                     (progn (forward-char -1) t)
                     (skip-chars-backward "^\"") 
                     (not (car-safe (ar-in-delimiter-base doublequoted)))))
         (unless (bobp)
           (setq bounds (beginning-of-form-base doublequoted doublequoted nil 'move 1 nil t))
           (ignore-errors (goto-char (car bounds))))
         bounds)))

;; Equalized
(put 'equalized 'beginning-op-at
     (lambda ()
       (let* ((equalized "=")
              (bounds (ar-in-delimiter-base equalized)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'equalized 'end-op-at
     (lambda ()
       (let* ((equalized "=")
              (erg (looking-at equalized)))
         (when erg 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base equalized equalized nil 'move 1 nil t)))
         erg)))

(put 'equalized 'forward-op-at
     (lambda ()
       (let ((equalized "=")
             bounds)
         (while (and (not (eobp))(progn (forward-char 1) t)(skip-chars-forward "^=") (not (car-safe (ar-in-delimiter-base equalized)))))
         (unless (eobp)
           (setq bounds (end-of-form-base equalized equalized nil 'move 1 nil t))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'equalized 'backward-op-at
     (lambda ()
       (let ((equalized "=")
             bounds)
         (while (and (not (eobp))
                     (progn (forward-char -1) t)
                     (skip-chars-backward "^=") 
                     (not (car-safe (ar-in-delimiter-base equalized)))))
         (unless (bobp)
           (setq bounds (beginning-of-form-base equalized equalized nil 'move 1 nil t))
           (ignore-errors (goto-char (car bounds))))
         bounds)))

;; Hyphened
(put 'hyphened 'beginning-op-at
     (lambda ()
       (let* ((hyphened "-")
              (bounds (ar-in-delimiter-base hyphened)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'hyphened 'end-op-at
     (lambda ()
       (let* ((hyphened "-")
              (erg (looking-at hyphened)))
         (when erg 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base hyphened hyphened nil 'move 1 nil t)))
         erg)))

(put 'hyphened 'forward-op-at
     (lambda ()
       (let ((hyphened "-")
             bounds)
         (while (and (not (eobp))(progn (forward-char 1) t)(skip-chars-forward "^-") (not (car-safe (ar-in-delimiter-base hyphened)))))
         (unless (eobp)
           (setq bounds (end-of-form-base hyphened hyphened nil 'move 1 nil t))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'hyphened 'backward-op-at
     (lambda ()
       (let ((hyphened "-")
             bounds)
         (while (and (not (eobp))
                     (progn (forward-char -1) t)
                     (skip-chars-backward "^-") 
                     (not (car-safe (ar-in-delimiter-base hyphened)))))
         (unless (bobp)
           (setq bounds (beginning-of-form-base hyphened hyphened nil 'move 1 nil t))
           (ignore-errors (goto-char (car bounds))))
         bounds)))

;; Quoted
(put 'quoted 'beginning-op-at
     (lambda ()
       (let* ((quoted "\"\\|'")
              (bounds (ar-in-delimiter-base quoted)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'quoted 'end-op-at
     (lambda ()
       (let* ((quoted "\"\\|'")
              (erg (looking-at quoted)))
         (when erg 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base quoted quoted nil 'move 1 nil t)))
         erg)))

(put 'quoted 'forward-op-at
     (lambda ()
       (let ((quoted "\"\\|'")
             bounds)
         (while (and (not (eobp))(progn (forward-char 1) t)(skip-chars-forward "^\"\\|'") (not (car-safe (ar-in-delimiter-base quoted)))))
         (unless (eobp)
           (setq bounds (end-of-form-base quoted quoted nil 'move 1 nil t))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'quoted 'backward-op-at
     (lambda ()
       (let ((quoted "\"\\|'")
             bounds)
         (while (and (not (eobp))
                     (progn (forward-char -1) t)
                     (skip-chars-backward "^\"\\|'") 
                     (not (car-safe (ar-in-delimiter-base quoted)))))
         (unless (bobp)
           (setq bounds (beginning-of-form-base quoted quoted nil 'move 1 nil t))
           (ignore-errors (goto-char (car bounds))))
         bounds)))

;; Singlequoted
(put 'singlequoted 'beginning-op-at
     (lambda ()
       (let* ((singlequoted "'")
              (bounds (ar-in-delimiter-base singlequoted)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'singlequoted 'end-op-at
     (lambda ()
       (let* ((singlequoted "'")
              (erg (looking-at singlequoted)))
         (when erg 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base singlequoted singlequoted nil 'move 1 nil t)))
         erg)))

(put 'singlequoted 'forward-op-at
     (lambda ()
       (let ((singlequoted "'")
             bounds)
         (while (and (not (eobp))(progn (forward-char 1) t)(skip-chars-forward "^'") (not (car-safe (ar-in-delimiter-base singlequoted)))))
         (unless (eobp)
           (setq bounds (end-of-form-base singlequoted singlequoted nil 'move 1 nil t))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'singlequoted 'backward-op-at
     (lambda ()
       (let ((singlequoted "'")
             bounds)
         (while (and (not (eobp))
                     (progn (forward-char -1) t)
                     (skip-chars-backward "^'") 
                     (not (car-safe (ar-in-delimiter-base singlequoted)))))
         (unless (bobp)
           (setq bounds (beginning-of-form-base singlequoted singlequoted nil 'move 1 nil t))
           (ignore-errors (goto-char (car bounds))))
         bounds)))

;; Slashed
(put 'slashed 'beginning-op-at
     (lambda ()
       (let* ((slashed "/")
              (bounds (ar-in-delimiter-base slashed)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'slashed 'end-op-at
     (lambda ()
       (let* ((slashed "/")
              (erg (looking-at slashed)))
         (when erg 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base slashed slashed nil 'move 1 nil t)))
         erg)))

(put 'slashed 'forward-op-at
     (lambda ()
       (let ((slashed "/")
             bounds)
         (while (and (not (eobp))(progn (forward-char 1) t)(skip-chars-forward "^/") (not (car-safe (ar-in-delimiter-base slashed)))))
         (unless (eobp)
           (setq bounds (end-of-form-base slashed slashed nil 'move 1 nil t))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'slashed 'backward-op-at
     (lambda ()
       (let ((slashed "/")
             bounds)
         (while (and (not (eobp))
                     (progn (forward-char -1) t)
                     (skip-chars-backward "^/") 
                     (not (car-safe (ar-in-delimiter-base slashed)))))
         (unless (bobp)
           (setq bounds (beginning-of-form-base slashed slashed nil 'move 1 nil t))
           (ignore-errors (goto-char (car bounds))))
         bounds)))

;; Underscored
(put 'underscored 'beginning-op-at
     (lambda ()
       (let* ((underscored "_")
              (bounds (ar-in-delimiter-base underscored)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'underscored 'end-op-at
     (lambda ()
       (let* ((underscored "_")
              (erg (looking-at underscored)))
         (when erg 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base underscored underscored nil 'move 1 nil t)))
         erg)))

(put 'underscored 'forward-op-at
     (lambda ()
       (let ((underscored "_")
             bounds)
         (while (and (not (eobp))(progn (forward-char 1) t)(skip-chars-forward "^_") (not (car-safe (ar-in-delimiter-base underscored)))))
         (unless (eobp)
           (setq bounds (end-of-form-base underscored underscored nil 'move 1 nil t))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'underscored 'backward-op-at
     (lambda ()
       (let ((underscored "_")
             bounds)
         (while (and (not (eobp))
                     (progn (forward-char -1) t)
                     (skip-chars-backward "^_") 
                     (not (car-safe (ar-in-delimiter-base underscored)))))
         (unless (bobp)
           (setq bounds (beginning-of-form-base underscored underscored nil 'move 1 nil t))
           (ignore-errors (goto-char (car bounds))))
         bounds)))

;; Whitespaced
(put 'whitespaced 'beginning-op-at
     (lambda ()
       (let* ((whitespaced " ")
              (bounds (ar-in-delimiter-base whitespaced)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'whitespaced 'end-op-at
     (lambda ()
       (let* ((whitespaced " ")
              (erg (looking-at whitespaced)))
         (when erg 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base whitespaced whitespaced nil 'move 1 nil t)))
         erg)))

(put 'whitespaced 'forward-op-at
     (lambda ()
       (let ((whitespaced " ")
             bounds)
         (while (and (not (eobp))(progn (forward-char 1) t)(skip-chars-forward "^ ") (not (car-safe (ar-in-delimiter-base whitespaced)))))
         (unless (eobp)
           (setq bounds (end-of-form-base whitespaced whitespaced nil 'move 1 nil t))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'whitespaced 'backward-op-at
     (lambda ()
       (let ((whitespaced " ")
             bounds)
         (while (and (not (eobp))
                     (progn (forward-char -1) t)
                     (skip-chars-backward "^ ") 
                     (not (car-safe (ar-in-delimiter-base whitespaced)))))
         (unless (bobp)
           (setq bounds (beginning-of-form-base whitespaced whitespaced nil 'move 1 nil t))
           (ignore-errors (goto-char (car bounds))))
         bounds)))

;; ar-unpaired-delimited-raw end

;; ar-atpt-python-quoted-raw start

;; Backslashed
(put 'backslashed 'beginning-op-at
     (lambda ()
       (let* ((backslashed "\\")
              (bounds (ar-in-delimiter-base backslashed)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'backslashed 'end-op-at
     (lambda ()
       (let* ((backslashed "\\")
              (erg (looking-at backslashed)))
         (when erg 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base backslashed backslashed nil 'move 1 nil t)))
         erg)))

(put 'backslashed 'forward-op-at
     (lambda ()
       (let ((backslashed "\\")
             bounds)
         (while (and (not (eobp))(progn (forward-char 1) t)(skip-chars-forward "^\\") (not (car-safe (ar-in-delimiter-base backslashed)))))
         (unless (eobp)
           (setq bounds (end-of-form-base backslashed backslashed nil 'move 1 nil t))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'backslashed 'backward-op-at
     (lambda ()
       (let ((backslashed "\\")
             bounds)
         (while (and (not (eobp))
                     (progn (forward-char -1) t)
                     (skip-chars-backward "^\\") 
                     (not (car-safe (ar-in-delimiter-base backslashed)))))
         (unless (bobp)
           (setq bounds (beginning-of-form-base backslashed backslashed nil 'move 1 nil t))
           (ignore-errors (goto-char (car bounds))))
         bounds)))

;; Dollared
(put 'dollared 'beginning-op-at
     (lambda ()
       (let* ((dollared "\\$")
              (bounds (ar-in-delimiter-base dollared)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'dollared 'end-op-at
     (lambda ()
       (let* ((dollared "\\$")
              (erg (looking-at dollared)))
         (when erg 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base dollared dollared nil 'move 1 nil t)))
         erg)))

(put 'dollared 'forward-op-at
     (lambda ()
       (let ((dollared "\\$")
             bounds)
         (while (and (not (eobp))(progn (forward-char 1) t)(skip-chars-forward "^\\$") (not (car-safe (ar-in-delimiter-base dollared)))))
         (unless (eobp)
           (setq bounds (end-of-form-base dollared dollared nil 'move 1 nil t))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'dollared 'backward-op-at
     (lambda ()
       (let ((dollared "\\$")
             bounds)
         (while (and (not (eobp))
                     (progn (forward-char -1) t)
                     (skip-chars-backward "^\\$") 
                     (not (car-safe (ar-in-delimiter-base dollared)))))
         (unless (bobp)
           (setq bounds (beginning-of-form-base dollared dollared nil 'move 1 nil t))
           (ignore-errors (goto-char (car bounds))))
         bounds)))

;; Doublequoted
(put 'doublequoted 'beginning-op-at
     (lambda ()
       (let* ((doublequoted "\"")
              (bounds (ar-in-delimiter-base doublequoted)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'doublequoted 'end-op-at
     (lambda ()
       (let* ((doublequoted "\"")
              (erg (looking-at doublequoted)))
         (when erg 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base doublequoted doublequoted nil 'move 1 nil t 'ar-escaped)))
         erg)))

(put 'doublequoted 'forward-op-at
     (lambda ()
       (let ((doublequoted "\"")
             bounds)
         (while (and (not (eobp))(progn (forward-char 1) t)(skip-chars-forward "^\"") (not (car-safe (ar-in-delimiter-base doublequoted)))))
         (unless (eobp)
           (setq bounds (end-of-form-base doublequoted doublequoted nil 'move 1 nil t))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'doublequoted 'backward-op-at
     (lambda ()
       (let ((doublequoted "\"")
             bounds)
         (while (and (not (eobp))
                     (progn (forward-char -1) t)
                     (skip-chars-backward "^\"") 
                     (not (car-safe (ar-in-delimiter-base doublequoted)))))
         (unless (bobp)
           (setq bounds (beginning-of-form-base doublequoted doublequoted nil 'move 1 nil t))
           (ignore-errors (goto-char (car bounds))))
         bounds)))

;; Equalized
(put 'equalized 'beginning-op-at
     (lambda ()
       (let* ((equalized "=")
              (bounds (ar-in-delimiter-base equalized)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'equalized 'end-op-at
     (lambda ()
       (let* ((equalized "=")
              (erg (looking-at equalized)))
         (when erg 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base equalized equalized nil 'move 1 nil t)))
         erg)))

(put 'equalized 'forward-op-at
     (lambda ()
       (let ((equalized "=")
             bounds)
         (while (and (not (eobp))(progn (forward-char 1) t)(skip-chars-forward "^=") (not (car-safe (ar-in-delimiter-base equalized)))))
         (unless (eobp)
           (setq bounds (end-of-form-base equalized equalized nil 'move 1 nil t))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'equalized 'backward-op-at
     (lambda ()
       (let ((equalized "=")
             bounds)
         (while (and (not (eobp))
                     (progn (forward-char -1) t)
                     (skip-chars-backward "^=") 
                     (not (car-safe (ar-in-delimiter-base equalized)))))
         (unless (bobp)
           (setq bounds (beginning-of-form-base equalized equalized nil 'move 1 nil t))
           (ignore-errors (goto-char (car bounds))))
         bounds)))

;; Hyphened
(put 'hyphened 'beginning-op-at
     (lambda ()
       (let* ((hyphened "-")
              (bounds (ar-in-delimiter-base hyphened)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'hyphened 'end-op-at
     (lambda ()
       (let* ((hyphened "-")
              (erg (looking-at hyphened)))
         (when erg 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base hyphened hyphened nil 'move 1 nil t)))
         erg)))

(put 'hyphened 'forward-op-at
     (lambda ()
       (let ((hyphened "-")
             bounds)
         (while (and (not (eobp))(progn (forward-char 1) t)(skip-chars-forward "^-") (not (car-safe (ar-in-delimiter-base hyphened)))))
         (unless (eobp)
           (setq bounds (end-of-form-base hyphened hyphened nil 'move 1 nil t))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'hyphened 'backward-op-at
     (lambda ()
       (let ((hyphened "-")
             bounds)
         (while (and (not (eobp))
                     (progn (forward-char -1) t)
                     (skip-chars-backward "^-") 
                     (not (car-safe (ar-in-delimiter-base hyphened)))))
         (unless (bobp)
           (setq bounds (beginning-of-form-base hyphened hyphened nil 'move 1 nil t))
           (ignore-errors (goto-char (car bounds))))
         bounds)))

;; Quoted
(put 'quoted 'beginning-op-at
     (lambda ()
       (let* ((quoted "\"\\|'")
              (bounds (ar-in-delimiter-base quoted)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'quoted 'end-op-at
     (lambda ()
       (let* ((quoted "\"\\|'")
              (erg (looking-at quoted)))
         (when erg 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base quoted quoted nil 'move 1 nil t)))
         erg)))

(put 'quoted 'forward-op-at
     (lambda ()
       (let ((quoted "\"\\|'")
             bounds)
         (while (and (not (eobp))(progn (forward-char 1) t)(skip-chars-forward "^\"\\|'") (not (car-safe (ar-in-delimiter-base quoted)))))
         (unless (eobp)
           (setq bounds (end-of-form-base quoted quoted nil 'move 1 nil t))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'quoted 'backward-op-at
     (lambda ()
       (let ((quoted "\"\\|'")
             bounds)
         (while (and (not (eobp))
                     (progn (forward-char -1) t)
                     (skip-chars-backward "^\"\\|'") 
                     (not (car-safe (ar-in-delimiter-base quoted)))))
         (unless (bobp)
           (setq bounds (beginning-of-form-base quoted quoted nil 'move 1 nil t))
           (ignore-errors (goto-char (car bounds))))
         bounds)))

;; Singlequoted
(put 'singlequoted 'beginning-op-at
     (lambda ()
       (let* ((singlequoted "'")
              (bounds (ar-in-delimiter-base singlequoted)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'singlequoted 'end-op-at
     (lambda ()
       (let* ((singlequoted "'")
              (erg (looking-at singlequoted)))
         (when erg 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base singlequoted singlequoted nil 'move 1 nil t)))
         erg)))

(put 'singlequoted 'forward-op-at
     (lambda ()
       (let ((singlequoted "'")
             bounds)
         (while (and (not (eobp))(progn (forward-char 1) t)(skip-chars-forward "^'") (not (car-safe (ar-in-delimiter-base singlequoted)))))
         (unless (eobp)
           (setq bounds (end-of-form-base singlequoted singlequoted nil 'move 1 nil t))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'singlequoted 'backward-op-at
     (lambda ()
       (let ((singlequoted "'")
             bounds)
         (while (and (not (eobp))
                     (progn (forward-char -1) t)
                     (skip-chars-backward "^'") 
                     (not (car-safe (ar-in-delimiter-base singlequoted)))))
         (unless (bobp)
           (setq bounds (beginning-of-form-base singlequoted singlequoted nil 'move 1 nil t))
           (ignore-errors (goto-char (car bounds))))
         bounds)))

;; Slashed
(put 'slashed 'beginning-op-at
     (lambda ()
       (let* ((slashed "/")
              (bounds (ar-in-delimiter-base slashed)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'slashed 'end-op-at
     (lambda ()
       (let* ((slashed "/")
              (erg (looking-at slashed)))
         (when erg 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base slashed slashed nil 'move 1 nil t)))
         erg)))

(put 'slashed 'forward-op-at
     (lambda ()
       (let ((slashed "/")
             bounds)
         (while (and (not (eobp))(progn (forward-char 1) t)(skip-chars-forward "^/") (not (car-safe (ar-in-delimiter-base slashed)))))
         (unless (eobp)
           (setq bounds (end-of-form-base slashed slashed nil 'move 1 nil t))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'slashed 'backward-op-at
     (lambda ()
       (let ((slashed "/")
             bounds)
         (while (and (not (eobp))
                     (progn (forward-char -1) t)
                     (skip-chars-backward "^/") 
                     (not (car-safe (ar-in-delimiter-base slashed)))))
         (unless (bobp)
           (setq bounds (beginning-of-form-base slashed slashed nil 'move 1 nil t))
           (ignore-errors (goto-char (car bounds))))
         bounds)))

;; Underscored
(put 'underscored 'beginning-op-at
     (lambda ()
       (let* ((underscored "_")
              (bounds (ar-in-delimiter-base underscored)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'underscored 'end-op-at
     (lambda ()
       (let* ((underscored "_")
              (erg (looking-at underscored)))
         (when erg 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base underscored underscored nil 'move 1 nil t)))
         erg)))

(put 'underscored 'forward-op-at
     (lambda ()
       (let ((underscored "_")
             bounds)
         (while (and (not (eobp))(progn (forward-char 1) t)(skip-chars-forward "^_") (not (car-safe (ar-in-delimiter-base underscored)))))
         (unless (eobp)
           (setq bounds (end-of-form-base underscored underscored nil 'move 1 nil t))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'underscored 'backward-op-at
     (lambda ()
       (let ((underscored "_")
             bounds)
         (while (and (not (eobp))
                     (progn (forward-char -1) t)
                     (skip-chars-backward "^_") 
                     (not (car-safe (ar-in-delimiter-base underscored)))))
         (unless (bobp)
           (setq bounds (beginning-of-form-base underscored underscored nil 'move 1 nil t))
           (ignore-errors (goto-char (car bounds))))
         bounds)))

;; Whitespaced
(put 'whitespaced 'beginning-op-at
     (lambda ()
       (let* ((whitespaced " ")
              (bounds (ar-in-delimiter-base whitespaced)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'whitespaced 'end-op-at
     (lambda ()
       (let* ((whitespaced " ")
              (erg (looking-at whitespaced)))
         (when erg 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base whitespaced whitespaced nil 'move 1 nil t)))
         erg)))

(put 'whitespaced 'forward-op-at
     (lambda ()
       (let ((whitespaced " ")
             bounds)
         (while (and (not (eobp))(progn (forward-char 1) t)(skip-chars-forward "^ ") (not (car-safe (ar-in-delimiter-base whitespaced)))))
         (unless (eobp)
           (setq bounds (end-of-form-base whitespaced whitespaced nil 'move 1 nil t))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'whitespaced 'backward-op-at
     (lambda ()
       (let ((whitespaced " ")
             bounds)
         (while (and (not (eobp))
                     (progn (forward-char -1) t)
                     (skip-chars-backward "^ ") 
                     (not (car-safe (ar-in-delimiter-base whitespaced)))))
         (unless (bobp)
           (setq bounds (beginning-of-form-base whitespaced whitespaced nil 'move 1 nil t))
           (ignore-errors (goto-char (car bounds))))
         bounds)))

;; ar-atpt-python-quoted-raw end

;; Abbrev
(put 'abbrev 'beginning-op-at
     (lambda ()
       (when
           (looking-at "[[:alnum:]]")
         (skip-chars-backward "[:alnum:].-")(point))))

(put 'abbrev 'end-op-at
     (lambda ()
       (skip-chars-forward "[:alnum:].-")(point)))

;; Acronym
(put 'acronym 'beginning-op-at
     (lambda ()
       (ar-th-gotobeg 'symbol)))

(put 'acronym 'end-op-at
     (lambda ()
       (when (or (looking-at "[\({]\\([A-Z][A-Za-z -]+\\)[\)}]")
                 (looking-at "\\(\\<[A-Z][A-Za-z]*[A-Z][A-Za-z]*\\>\\)"))
         (goto-char (match-end 0)))))

;; Angled
(put 'angled-no-nest 'beginning-op-at
     (lambda ()
       (let ((orig (point)) 
             (forward-form (cond ((equal (char-after) ?>) "<")
                                 ((equal (char-after) ?<) ">"))))
         (if (ignore-errors (looking-at "[<>]"))
             (progn
               (search-forward forward-form nil 'move 1)
               (search-backward forward-form nil 'move 1)
               orig)
           (re-search-backward "[<>]" nil 'move 1)))))

(put 'angled-no-nest 'end-op-at
     (lambda ()
       (let ((forward-form (cond ((equal (char-after) ?>) "<")
                                 ((equal (char-after) ?<) ">"))))
         (search-forward forward-form nil 'move 1))))

(defalias 'ar-angled-atpt 'ar-angled-no-nest-atpt)
(defalias 'ar-bounds-of-angled-atpt 'ar-bounds-of-angled-no-nest-atpt)

(put 'angled-greater-no-nest 'beginning-op-at
     (lambda ()
       (if (looking-at ">")
           (point)
         (re-search-backward ">" nil 'move 1))))

(put 'angled-greater-no-nest 'end-op-at
     (lambda ()
       (re-search-forward ">" nil 'move 1)))

(put 'angled-lesser-no-nest 'beginning-op-at
     (lambda ()
       (if (looking-at "<")
           (point)
         (re-search-backward "<" nil 'move 1))))

(put 'angled-lesser-no-nest 'end-op-at
     (lambda ()
       (re-search-forward "<" nil 'move 1)))

(put 'angled-greater-nested 'beginning-op-at
     (lambda ()
       (if (looking-at ">")
           (point)
         (beginning-of-form-base ">" "<" nil 'move nil nil t))))

(put 'angled-greater-nested 'end-op-at
     (lambda ()
       (end-of-form-base ">" "<" nil 'move nil nil t)))

(put 'angled-lesser-nested 'beginning-op-at
     (lambda ()
       (if (looking-at "<")
           (point)
         (beginning-of-form-base "<" ">" nil 'move nil nil t))))

(put 'angled-lesser-nested 'end-op-at
     (lambda ()
       (end-of-form-base "<" ">" nil 'move nil nil t)))

;; Buffer
(put 'buffer 'beginning-op-at
     (lambda ()
       (let ((pos (point-min)))
         (unless (eq (point) pos)
           (goto-char pos)
           pos))))

(put 'buffer 'end-op-at
     (lambda ()
       (let ((pos (point-max)))
         (unless (eq (point) pos)
           (goto-char pos)
           pos))))

;; Comment
(put 'comment 'beginning-op-at
     (lambda ()
       (let* ((orig (point)) 
              (nesting (not (or (string= "" comment-end)(eq 10 comment-end))))
              (erg (when nesting
                     (if (looking-at comment-start)
                         (match-beginning 0)
                       (car-safe (beginning-of-form-base comment-start comment-end nil 'move 1 t)))))
              last)
         (unless erg
           (when (looking-at comment-start-skip)
             (setq erg (point)))
           (while (and (setq last (point))
                       (setq erg (nth 8 (syntax-ppss)))
                       (goto-char erg)
                       (skip-chars-backward " \t\r\n\f")))
           (skip-chars-forward " \t\r\n\f") 
           (setq erg (point)))
         ;; (while (and (not (bobp))(forward-line -1)(looking-at (concat "[ \t]*" (regexp-quote comment-start))))
         ;;   (setq erg (point)))) 
         (when erg (goto-char erg))
         erg)))

(put 'comment 'end-op-at
     (lambda ()
       (let* ((nesting (not (or (string= "" comment-end)(eq 10 comment-end))))
              (erg
               (when nesting
                 (when (looking-at (concat "[ \t]*" (regexp-quote comment-start)))
                   (progn
                     (goto-char (match-end 0))
                     (ignore-errors (cadr (end-of-form-base comment-start comment-end nil 'move 1 t))))))))
         (unless erg
           (when
               (looking-at (concat "[ \t]*" (regexp-quote comment-start)))
             (setq erg (line-end-position))
             (while (and (not (eobp))(forward-line 1)(looking-at (concat "[ \t]*" (regexp-quote comment-start)))) 
               (setq erg (line-end-position)))))
         erg)))

(put 'comment 'forward-op-at
     (lambda ()
       (search-forward comment-start nil 'move 1)))

(put 'comment 'backward-op-at
     (lambda ()
       (search-backward comment-start nil 'move 1)
       (goto-char (1+ (match-end 0)))))

;;; CSV
;; Inspired by
;;; csv-mode.el --- major mode for editing comma-separated value files
;; Author: Francis J. Wright <F.J.Wright at qmul.ac.uk>
;; URL: http://centaur.maths.qmul.ac.uk/Emacs/
(defcustom ar-csv-separator-atpt ";"
  "Char to distinguish datasets in a `comma`-separated row" 
  :type 'string
  :group 'convenience)
;; (when (boundp 'csv-separators)
;; (setq ar-separator-atpt csv-separators))

(put 'csv 'beginning-op-at
     (lambda ()
       (skip-chars-backward (concat "^" ar-csv-separator-atpt))(point)))

(put 'csv 'end-op-at
     (lambda ()
       (skip-chars-forward (concat "^" ar-csv-separator-atpt))(point)))

;; DATE
(put 'date 'beginning-op-at
     (lambda ()
       ;; provide for the case, we are over a
       ;; string-delimiter as `"'
       (when
           (and (not (eq 32 (if (featurep 'xemacs)
                                (encode-char (char-after) 'ucs)
                              (char-after)))) 
                (or (bobp)
                    (eq 32 (if (featurep 'xemacs)
                               (encode-char (char-before) 'ucs)
                             (char-before)))))
         (forward-char 1)
         ;; as the bounds-function checks position, correct it
         ;; (setq th-orig 1)
         ) 
       (skip-chars-backward "0-9 .-")
       (skip-chars-forward " ")(point)))

(put 'date 'end-op-at
     (lambda ()
       (skip-chars-forward "0-9 .-")
       (skip-chars-backward " ")(point)))

;; Defun
(put 'defun 'beginning-op-at (lambda (&optional arg) (beginning-of-defun (or arg 1))(point)))

(put 'defun 'end-op-at (lambda (&optional arg)(end-of-defun (or arg 1))(point)))

;; Delimited
(put 'delimited 'beginning-op-at
     (lambda ()
       (let ((begdel (concat (regexp-quote th-beg-delimiter) ar-delimiters-atpt))
             pos)
         (cond ((looking-at "[({\\[]")) 
               ((looking-at "]")
                (beginning-of-form-base "[" "]" nil 'move nil nil t))
               ((looking-at "}")
                (beginning-of-form-base "{" "}" nil 'move nil nil t))
               ((looking-at ")")
                (beginning-of-form-base "(" ")" nil 'move nil nil t))
               ((looking-at (concat "[" begdel "]"))
                (ar-set-delimiter-zeichen)
                (let ((pos (nth 8 (parse-partial-sexp (point-min) (point)))))
                  (when pos
                    (goto-char pos))))
               ((setq pos (nth 8 (parse-partial-sexp (point-min) (point))))
                (goto-char pos)
                (ar-set-delimiter-zeichen))
               (t (while (and (not (bobp))(re-search-backward (concat "[" begdel "]") nil 'move 1)(looking-at "\""))
                    (re-search-backward (concat "[" begdel "]") nil 'move 1))
                  (if (looking-at (concat "[" begdel "]"))
                      (ar-set-delimiter-zeichen) 
                    (setq ar-delimiter-zeichen-atpt nil)))))))

(put 'delimited 'end-op-at
     (lambda ()
       (cond ((looking-at "\\[")
              (end-of-form-base "[" "]" nil 'move nil nil t))
             ((looking-at "{")
              (end-of-form-base "{" "}" nil 'move nil nil t))
             ((looking-at "(")
              ;; (end-of-form-base "(" ")" nil 'move nil nil t)
              (forward-list))
             (t (forward-char 1)
                (search-forward (char-to-string ar-delimiter-zeichen-atpt) nil nil 1)))))
  
  (defun ar-set-delimiter-zeichen ()
  (setq ar-delimiter-zeichen-atpt
        (if (featurep 'xemacs)
            (encode-char (char-after) 'ucs)
          (char-after))))

(defvar ar-delimiter-zeichen-atpt nil
  "Delimiter char found at place, search it backward then")
(make-variable-buffer-local 'ar-delimiter-zeichen-atpt)

(defcustom ar-use-parse-partial-sexp t
  "When nil, parse symbolic expressions by regexp. "
  :type 'boolean
  :group 'convenience)

(defcustom ar-delimiters-atpt "'#\$/=?!:;"
  "Specify the delimiter chars. Doublequote is treated by default, not accepted here. "
  :type 'string
  :group 'convenience)

(defcustom th-beg-delimiter "{>[(/"
  "Specify the delimiter char."
  :type 'string
  :group 'convenience)

(defcustom th-end-delimiter "}]<)/"
  "Specify the delimiter char."
  :type 'string
  :group 'convenience)

;; Email
(put 'email 'beginning-op-at
     (lambda ()
       (when
           (looking-at "[^ \t]")
         (re-search-backward "[,;][[:graph:]]\\|<[[:graph:]]\\|^[[:graph:]]\\|[^[:graph:]][[:graph:]]" (line-beginning-position) t 1)
         (when (looking-at "[[:space:];,<]") 
           (forward-char 1))))) 
  
  (put 'email 'end-op-at 
  (lambda ()
    (when (looking-at "[ <]\\{0,1\\}\\([\041-\132\136-\176]+@[\041-\132\136-\176]+\\)[;,> \t\n]*")
    (goto-char (match-end 1))
    (skip-chars-backward "[[:punct:]]"))(point)))

;; Filename
(if (featurep 'xemacs)
    (defcustom thingatpt-file-name-chars "-~/A-Za-z0-9ÄÖÜäöüß_.$?={}#%,: " 
      "Characters forseen in filenames. "
      :type 'string
      :group 'convenience)
  
  (defcustom thingatpt-file-name-chars "-~/[:alnum:]_.$?={}#%,:" 
    "Characters forseen in filenames. "
    :type 'string
    :group 'convenience)) 

(put 'filename 'beginning-op-at
     (lambda ()
       (unless (looking-back " \t\n\r")
         (skip-chars-backward thingatpt-file-name-chars))(point)))

(put 'filename 'end-op-at
     (lambda ()
       (re-search-forward (concat "\\=[" thingatpt-file-name-chars "]*")
                          nil t)
       (skip-chars-backward ": ")(point)))

;; Floats
(put 'float 'beginning-op-at
     (lambda ()
       (when (numberp (read (buffer-substring-no-properties (point) (1+ (point)))))
         (skip-chars-backward "0-9.,"))(point)))

(put 'float 'end-op-at (lambda () (skip-chars-forward "[0-9.,]")(point)))

;; Function
(put 'function 'beginning-op-at
     (lambda ()
       (cond
        ((eq (point) (defun-beginning-position))
         (point))
        (t (beginning-of-defun)
           (point)))))

(put 'function 'end-op-at
     (lambda ()
       (end-of-defun)
       (when (string= major-mode "emacs-lisp-mode")
         (skip-chars-backward " \t\r\n"))(point))) 

;; IP
(put 'ip 'beginning-op-at
     (lambda ()
       (unless (looking-at "\\s-")
         (skip-chars-backward "0-9."))(point)))

(put 'ip 'end-op-at
     (lambda ()
       (when (looking-at "[0-9]\\{1,3\\}.[0-9-]\\{1,3\\}.[0-9]\\{1,3\\}.[0-9]\\{1,3\\}")
         (goto-char (match-end 0)))(point)))

;; ISBN
(put 'isbn 'beginning-op-at
     (lambda ()
       (unless (looking-at "\\s-")
         (skip-chars-backward "0-9-")(point))))

(put 'isbn 'end-op-at
     (lambda ()
       (when (looking-at "[0-9]\\{1,3\\}[0-9-]\\{7,12\\}[0-9X]\\{0,1\\}")
         (goto-char (match-end 0)))))

;; Lines
(put 'line 'beginning-op-at (lambda () (beginning-of-line)(point)))

(put 'line 'end-op-at (lambda () (end-of-line)(point)))

;; Markup
(defcustom markup-startstring-atpt "<[^<>]+>" 
  "Defining the beginning of a markup using ar-markup-atpt functions. "
  :type 'string
  :group 'convenience)

(defcustom markup-endstring-atpt "</[^<>]+>" 
  "Defining the end of a markup using ar-markup-atpt functions. "
  :type 'string
  :group 'convenience)

(put 'markup 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at markup-startstring-atpt))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base markup-startstring-atpt markup-endstring-atpt nil 'move nil nil t))))

(put 'markup 'end-op-at
     (lambda ()
       (lexical-let ((this-end (when (looking-at markup-startstring-atpt) 
                                 (match-string-no-properties 0))))
         (when (stringp this-end)
           (setq this-end (replace-regexp-in-string "<" "</" this-end)) 
           (end-of-form-base markup-startstring-atpt this-end nil 'move nil nil t)))))

;; Markup-no-nest
;; (put 'markup-no-nest 'beginning-op-at
;;      (lambda ()
;;        (if (ignore-errors (looking-at markup-startstring-atpt))
;;            (point)
;;          (unless (bobp) (forward-char -1))
;;          (while (and (not (bobp) (not (ignore-errors (looking-at markup-startstring-atpt)))))
;;            (forward-char -1))
;;          (when (ignore-errors (looking-at markup-startstring-atpt))
;;            (point)))))
;; 
;; (put 'markup-no-nest 'end-op-at
;;      (lambda ()
;;        (when (ignore-errors (looking-at markup-startstring-atpt))
;;          (re-search-forward markup-endstring-atpt nil 'move 1)
;;          (when (ignore-errors (looking-at markup-startstring-atpt))
;;            (point)))))

;; Ml-data
(put 'ml-data 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at markup-startstring-atpt))
           (match-end 0)
         (beginning-of-form-base markup-startstring-atpt markup-endstring-atpt nil 'move nil nil t)
         (when (ignore-errors (looking-at markup-startstring-atpt))
           (match-end 0)))))

(put 'ml-data 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at markup-startstring-atpt)) 
         (end-of-form-base markup-startstring-atpt markup-endstring-atpt nil 'move nil nil t)
         (re-search-backward markup-endstring-atpt nil 'move 1))))

;; Ml-tag
(put 'ml-tag 'beginning-op-at
     (lambda ()
       (if (ignore-errors
             (or
              (looking-at markup-startstring-atpt)
              (looking-at markup-endstring-atpt)))
           (list (point) (1+ (point)))
         (unless (bobp) (forward-char -1))
         (while
             (and (not (bobp))
                  (not
                   (ignore-errors
                     (or
                      (looking-at markup-startstring-atpt)
                      (looking-at markup-endstring-atpt)))))
           (forward-char -1))
         (when
             (ignore-errors
               (or
                (looking-at markup-startstring-atpt)
                (looking-at markup-endstring-atpt)))
           (list (point) (1+ (point)))))))

(put 'ml-tag 'end-op-at
     (lambda ()
       (when (ignore-errors (or
                             (looking-at markup-startstring-atpt)
                             (looking-at markup-endstring-atpt)))
         (list (1- (match-end 0))(match-end 0)))))

;; Number
(put 'number 'beginning-op-at
     (lambda ()
       (when
           (string-match "[0-9]" (buffer-substring-no-properties (point) (1+ (point))))
         (skip-chars-backward "0-9")(point))))

(put 'number 'end-op-at
     (lambda ()
       (skip-chars-forward "0-9")(point)))

;; Name
(defcustom ar-name-chars-atpt "a-zA-Z_;-"
  "Name is just a identifier for general use, described by chars composing it. "
  :type 'regexp
  :group 'convenience)

(put 'name 'beginning-op-at
     (lambda ()
       (skip-chars-backward ar-name-chars-atpt)
       (point)))

(put 'name 'end-op-at
     (lambda ()
       (when (looking-at (concat "[" ar-name-chars-atpt "]"))
         (skip-chars-forward ar-name-chars-atpt)
         ;; name may contain char `:' but not at the end, as
         ;; messages tend to insert it there 
         (skip-chars-forward ar-name-chars-atpt)
         (skip-chars-backward ":")
         (point))))

;; Page
(put 'page 'beginning-op-at
     (lambda ()
       (backward-page)(point)))

(put 'page 'end-op-at
     (lambda ()
       (forward-page)(point)))

;; Paragraph
(put 'paragraph 'beginning-op-at
     (lambda ()
       (unless (looking-back paragraph-start)
         (backward-paragraph)(point))))

(put 'paragraph 'end-op-at
     (lambda ()
       (when (looking-back paragraph-start)
         (forward-paragraph)(point))))

;; Paren
(put 'paren 'beginning-op-at
     (lambda ()
       (cond
        ((looking-at "\\s)")
         (forward-char 1) (backward-list 1))
        (t (while
               (and (not (looking-at "\\s(")) (not (bobp)))
             (forward-char -1))(point)))))

(put 'paren 'end-op-at
     (lambda ()
       (forward-list 1)))

;; Phone
(put 'phone 'beginning-op-at
     (lambda ()
       (when
           (and (looking-at "[0-9 \t.()-]")
                (not (eq (char-before) ?+)))
         (re-search-backward "[^0-9 \t.()-][0-9 ()\t-]+" (line-beginning-position) nil 1) (forward-char 1)(point)))) 

(put 'phone 'end-op-at
     (lambda ()
       (when
           (looking-at "[0-9;, \t()-]")
         (re-search-forward "[0-9 \t.()-]+[^0-9 \t-]" (1+ (line-end-position)) nil 1) (forward-char -1))(point))) 

;; Region
(defvar ar-region-end-atpt nil)
(put 'region 'beginning-op-at
     (lambda ()
       (setq ar-region-end-atpt (region-end))
       (goto-char (region-beginning))))

(put 'region 'end-op-at
     (lambda ()
       (goto-char ar-region-end-atpt)))

;; Sentence
(put 'sentence 'beginning-op-at
     (lambda ()
       (backward-sentence)))

(put 'sentence 'end-op-at
     (lambda ()
       (forward-sentence)))

;; Sexp
(put 'sexp 'beginning-op-at
     (lambda ()
       (unless (looking-back "[ \f\n\r\t]") (ignore-errors (backward-sexp)))(point)))

(put 'sexp 'end-op-at
     (lambda ()
       (forward-sexp)(point)))

;; Strings
(put 'string 'beginning-op-at
     (lambda ()
       (if ar-use-parse-partial-sexp
           (let* ((pps (parse-partial-sexp (point-min) (point)))
                  (pos8 (nth 8 pps)))
             (when (nth 3 pps) 
               (goto-char pos8)))
         (when
             (re-search-backward "\\([^\\\\]\\)\\(\"\\)" nil 'move 1)
           (goto-char (match-beginning 2))))
       (when (looking-at "\"*")
         (list (match-beginning 0) (match-end 0)))))

(put 'string 'end-op-at
     (lambda ()
       (forward-char 1)
       (if ar-use-parse-partial-sexp
           (let* ((orig (point)) 
                  (pps (parse-partial-sexp (point-min) (point)))
                  (char (char-to-string (nth 3 pps)))
                  (done t))
             (progn
               (while (and (not (eobp)) (prog1 done (forward-char 1))
                           (setq done (skip-chars-forward (concat "^" char)))
                           
                           (nth 5 (parse-partial-sexp orig (point)))))
               (when (and (< orig (point))(looking-at char))
                 (list (match-beginning 0) (match-end 0)))))
         (when (re-search-forward "[^\\\\]\"" nil 'move 1)
           (list (match-beginning 0) (match-end 0))))))


;; Sh-struct
(put 'sh-struct 'beginning-op-at
     'sh-beginning-of-form)

(put 'sh-struct 'end-op-at
     (lambda ()
       (when (looking-at ar-beginning-sh-struct-atpt)
         (sh-end-of-form)
         (forward-char 1)(point)))) 

(put 'sh-struct 'forward-op-at
     (lambda ()
       (re-search-forward ar-beginning-sh-struct-atpt nil 'move 1))) 

(put 'sh-struct 'backward-op-at
     (lambda ()
       (re-search-backward ar-end-sh-struct-atpt nil 'move 1)))

;; Symbol
(put 'symbol 'beginning-op-at
     (lambda ()
       (unless (looking-at "\\s-")
         (skip-syntax-backward "w_.")(point))))

(put 'symbol 'end-op-at
     (lambda ()
       (skip-syntax-forward "w_.")(point)))

;; Triplequoted
(put 'triplequoted 'beginning-op-at
     (lambda ()
       (let* ((triplequoted "\"\"\"\\|'''")
              (bounds (ar-in-delimiter-base triplequoted)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'triplequoted 'end-op-at
     (lambda ()
       (let* ((triplequoted "\"\"\"\\|'''")
              (erg (looking-at triplequoted)))
         (when erg 
           (setq triplequoted (match-string-no-properties 0)) 
           (goto-char (match-end 0))
           (setq erg (end-of-form-base triplequoted triplequoted nil 'move 1 nil nil 'ar-escaped)))
         erg)))

(put 'triplequoted 'forward-op-at
     (lambda ()
       (let ((triplequoted "\"\"\"\\|'''")
             bounds)
         (while (and (search-forward triplequoted nil 'move 1)
                     (not (ar-in-delimiter-base triplequoted))))
         (unless (eobp)
           (setq bounds (end-of-form-base triplequoted triplequoted nil 'move 1 nil nil 'ar-escaped))
           (ignore-errors (goto-char (1- (cadr bounds)))))
         bounds)))

(put 'triplequoted 'backward-op-at
     (lambda ()
       (let ((triplequoted "\"\"\"\\|'''")
             erg)
         (while (and (search-backward triplequoted nil 'move 1)
                     (not (setq erg (ar-in-delimiter-base triplequoted)))))
         (when erg (goto-char erg))
         erg)))

;; Triplequoted-Dq
(put 'triplequoted-dq 'beginning-op-at
     (lambda ()
       (let* ((triplequoted-dq "\"\"\"")
              (bounds (ar-in-delimiter-base triplequoted-dq)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'triplequoted-dq 'end-op-at
     (lambda ()
       (let* ((triplequoted-dq "\"\"\"")
              (erg (looking-at triplequoted-dq)))
         (when erg 
           (goto-char (match-end 0))
           (while (and (search-forward triplequoted-dq nil 'move 1)
                       (ar-in-delimiter-base triplequoted-dq)))
           (when (looking-back triplequoted-dq)
             (list (match-beginning 0) (match-end 0)))))))

(put 'triplequoted-dq 'forward-op-at
     (lambda ()
       (let ((triplequoted-dq "\"\"\""))
         (while (and (search-forward triplequoted-dq nil 'move 1)
                     (not (ar-in-delimiter-base triplequoted-dq)))))))

(put 'triplequoted-dq 'backward-op-at
     (lambda ()
       (let ((triplequoted-dq "\"\"\""))
         (while (and (search-backward triplequoted-dq nil 'move 1)
                     (not (ar-in-delimiter-base triplequoted-dq)))))))

;; Triplequoted-Sq
(put 'triplequoted-sq 'beginning-op-at
     (lambda ()
       (let* ((triplequoted-sq "'''")
              (bounds (ar-in-delimiter-base triplequoted-sq)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'triplequoted-sq 'end-op-at
     (lambda ()
       (let* ((triplequoted-sq "'''")
              (erg (looking-at triplequoted-sq)))
         (when erg 
           (goto-char (match-end 0))
           (while (and (search-forward triplequoted-sq nil 'move 1)
                       (ar-in-delimiter-base triplequoted-sq)))
           (when (looking-back triplequoted-sq)
             (list (match-beginning 0) (match-end 0)))))))

(put 'triplequoted-sq 'forward-op-at
     (lambda ()
       (let ((triplequoted-sq "'''"))
         (while (and (search-forward triplequoted-sq nil 'move 1)
                     (not (ar-in-delimiter-base triplequoted-sq)))))))

(put 'triplequoted-sq 'backward-op-at
     (lambda ()
       (let ((triplequoted-sq "'''"))
         (while (and (search-backward triplequoted-sq nil 'move 1)
                     (not (ar-in-delimiter-base triplequoted-sq)))))))

;; Url
;; use thingatpt.el's form here too
(put 'url 'end-op-at (get 'url 'end-op))
(put 'url 'beginning-op-at (get 'url 'beginning-op))

(defcustom url-at-point-chars ":/?#[]@!$&()*+,;=[:alnum:]-._~"
  "Chars which might compose a URL. " 
  :type 'string
  :group 'convenience)

;; Whitespace
(put 'whitespace 'beginning-op-at
     (lambda () (when (looking-at "[ \t]") (skip-chars-backward "[ \t\r\n[:blank:]]")(point))))

(put 'whitespace 'end-op-at (lambda () (skip-chars-forward "[ \t\r\n[:blank:]]")(point)))

;; Word
(put 'word 'beginning-op-at
     (lambda () (when (looking-at "\\w")
                  (unless (looking-back "\\W")
                    (forward-word -1))
                  (point))))

(put 'word 'end-op-at
     (lambda () (and (looking-back "\\W")(looking-at "\\w"))
       (forward-word 1)(point))) 

;; Word-alpha-only
(put 'word-alpha-only 'beginning-op-at
     (lambda () (when (looking-at "[[:alpha:]]")
                  (unless (looking-back "[^[:alpha:]]")
                    (skip-chars-backward "[:alpha:]")
                    (point)))))

(put 'word-alpha-only 'end-op-at
     (lambda () (when (and (looking-back "[^[:alpha:]]")(looking-at "[[:alpha:]]"))
                  (skip-chars-forward "[:alpha:]")
                  (point)))) 

(defun ar-in-string-p (&optional condition)
  "In inside a double- triple- or singlequoted string,
  parsed by regexp, not syntax related `parse-partial-sexp'. 
  
  Optional arg CONDITION accepts a function. If CONDITION returns `t', results of in-string-p at this point are discarded, search continuous.
  Returns beginning-delimiter positions, a list. "
  (interactive)
  (let ((tq (ar-in-triplequoted-p-atpt))
        erg)
    (if tq
        (setq erg tq)
      (setq erg (or 
                 (ar-in-doublequoted-p-atpt condition)
                 (ar-in-singlequoted-p-atpt condition)))
      (when (car-safe erg)
        (save-excursion
          (goto-char (car-safe erg))
          (when (ar-in-triplequoted-p-atpt)(setq erg nil)))))
    (when (interactive-p) (message "%s" erg))
    erg))


;; ML data-forms start

;; Begin-End-Quoted
(put 'begin-end-quoted 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\\begin{quote}"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\\begin{quote}" "\\end{quote}" nil (quote move) 1 nil nil nil))))

(put 'begin-end-quoted 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\\begin{quote}"))
         (goto-char (match-end 0)) 
         (end-of-form-base "\\begin{quote}" "\\end{quote}" nil (quote move) 1 nil nil nil))))


;; Blok
(put 'blok 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "{%"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "{%" "%}" nil (quote move) 1 nil t nil))))

(put 'blok 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "{%"))
         (goto-char (match-end 0)) 
         (end-of-form-base "{%" "%}" nil (quote move) 1 nil t nil))))


;; Double-Backslashed
(put 'double-backslashed 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\\\\"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\\\\" "\\\\" nil (quote move) 1 nil nil (quote ar-escaped)))))

(put 'double-backslashed 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\\\\"))
         (goto-char (match-end 0)) 
         (end-of-form-base "\\\\" "\\\\" nil (quote move) 1 nil nil (quote ar-escaped)))))


;; Doubleslashed
(put 'doubleslashed 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "//"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "//" "//" nil (quote move) 1 nil nil (quote ar-escaped)))))

(put 'doubleslashed 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "//"))
         (goto-char (match-end 0)) 
         (end-of-form-base "//" "//" nil (quote move) 1 nil nil (quote ar-escaped)))))


;; Doubleslashed-Paren
(put 'doubleslashed-paren 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\\\\\\\\("))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\\\\\\\\(" "\\\\\\\\)" nil (quote move) 1 nil nil (quote ar-escaped)))))

(put 'doubleslashed-paren 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\\\\\\\\("))
         (goto-char (match-end 0)) 
         (end-of-form-base "\\\\\\\\(" "\\\\\\\\)" nil (quote move) 1 nil nil (quote ar-escaped)))))


;; Tabledata-P
(put 'tabledata-p 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "<td[^>]*>"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "<td[^>]*>" "</td>" nil (quote move) 1 nil nil nil))))

(put 'tabledata-p 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "<td[^>]*>"))
         (goto-char (match-end 0)) 
         (end-of-form-base "<td[^>]*>" "</td>" nil (quote move) 1 nil nil nil))))


;; Slashed-Paren
(put 'slashed-paren 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\\\\("))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\\\\(" "\\\\)" nil (quote move) 1 nil nil (quote ar-escaped)))))

(put 'slashed-paren 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\\\\("))
         (goto-char (match-end 0)) 
         (end-of-form-base "\\\\(" "\\\\)" nil (quote move) 1 nil nil (quote ar-escaped)))))


;; Xsl-Stylesheet-P
(put 'xsl-stylesheet-p 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "<xsl:stylesheet[^<]+>.*$"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "<xsl:stylesheet[^<]+>.*$" "</xsl:stylesheet>" nil (quote move) 1 nil nil nil))))

(put 'xsl-stylesheet-p 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "<xsl:stylesheet[^<]+>.*$"))
         (goto-char (match-end 0)) 
         (end-of-form-base "<xsl:stylesheet[^<]+>.*$" "</xsl:stylesheet>" nil (quote move) 1 nil nil nil))))


;; Xsl-Template-P
(put 'xsl-template-p 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "<xsl:template[^<]+>.*$"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "<xsl:template[^<]+>.*$" "</xsl:template>" nil (quote move) 1 nil nil nil))))

(put 'xsl-template-p 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "<xsl:template[^<]+>.*$"))
         (goto-char (match-end 0)) 
         (end-of-form-base "<xsl:template[^<]+>.*$" "</xsl:template>" nil (quote move) 1 nil nil nil))))


;; ML data-forms end

;; ar-insert-thingatpt-th-funktionen start

(defun ar-th (thing &optional arg no-delimiters iact) 
  "Returns a buffer substring according to THING.
  THING may be a well known form as `symbol',
  `list', `sexp', `defun' or a newly defined THING.
  When mark-thingatpt is `t' - the default - a found THING 
  is set as current region, enabling further action on them
  
  If ARG is greater 1, the arg-th thing forward is return, with
  negative value before 
  If NO-DELIMITERS, set by user functions
  with universal-argument for example, THING returned is
  stripped by delimiters resp. markup "
  (let ((no-delimiters (or no-delimiters (eq 4 (prefix-numeric-value arg))))
        (arg (or arg (setq arg 1))))
    (when (symbolp arg) (setq arg '-1)) 
    (if (and (not (eq 1 arg))(not (eq 4 (prefix-numeric-value arg))))
        (when (or (< 1 arg) (> 1 arg))
          (ar-th-forward thing arg (interactive-p)))
      (condition-case nil
          (let* ((bounds (ar-th-bounds thing no-delimiters))
                 (type 
                  (when (and
                         (car bounds)(cdr bounds))
                    (buffer-substring-no-properties (car bounds) (cdr bounds))))
                 (old-zmacs (when (featurep 'xemacs) zmacs-regions)))
            
            (when (and (stringp type) (or thing-copy-region iact))
              (when (featurep 'xemacs) (setq zmacs-regions t))
              (ar-th-mark thing bounds))
            (when (and (stringp type) (or thing-copy-region iact)) (kill-new type))
            (when (featurep 'xemacs)
              (setq zmacs-regions old-zmacs))
            (when iact (message "%s" type))
            type)
        (error nil)))))

(defun ar-th-bounds (thing &optional no-delimiters iact)
  "Determine the start and end buffer locations for the THING at point.
  THING is a symbol which specifies the kind entity you want.

  A boolean value NO-DELIMITERS says if THING boundaries should extend to markups, delimiters or not.
  Call THING by his name, i.e. ar-word-atpt etc. IACT is t, if function has been called interactively "
  (ignore-errors 
    (save-excursion
      (let* ((orig (point))
             (beg (funcall (get thing 'beginning-op-at)))
             (end (funcall (get thing 'end-op-at))))
        (when (and beg end)
          (if (numberp beg)
              (when no-delimiters
                (progn
                  (setq beg (1+ beg))
                  (setq end (1- end))))
            (if no-delimiters
                (progn
                  (setq beg (cadr beg))
                  (setq end (car end)))
              (setq beg (car beg))
              (setq end (cadr end))))
          ;;           \([0-9][0-9]\) permit = orig end
          (when (and (not (eq beg end))(or (eobp)(<= orig end)))
            (when iact (message "%s" (cons beg end)))
            (cons beg end)))))))

(defun ar-th-beg (thing &optional arg iact)
  (condition-case nil 
      (let* ((bounds (ar-th-bounds thing arg))
	     (beg (car bounds)))
	(when iact
	  (message "   %s " beg)
          (kill-new (format "%s" beg))) 
	beg)
    (error nil)))

(defun ar-th-end (thing &optional arg iact)
  (condition-case nil 
      (let* ((bounds (ar-th-bounds thing arg))
	     (end (cdr bounds)))
	(when iact
	  (message "   %s "  end)) 
	end)
    (error nil)))

(defun ar-th-gotobeg (thing &optional arg iact)
  "Goto char beginning, core function "
  (condition-case nil 
      (let* ((bounds (ar-th-bounds thing arg))
	     (beg (car bounds)))
	(when iact
	  (message "   %s " beg)
          (kill-new (format "%s" beg)))
	(goto-char beg))
    (error nil)))

(defun ar-th-gotoend (thing &optional arg iact)
  "Goto char end, core function "
  (condition-case nil 
      (let* ((bounds (ar-th-bounds thing arg))
	     (end (cdr bounds)))
	(when iact
	  (message "   %s " end)
          (kill-new (format "%s" end)))
	(goto-char (1- end)))
    (error nil)))

(defun ar-th-length (thing &optional arg iact)
  (ignore-errors 
    (let* ((bounds (ar-th-bounds thing arg))
           (length (abs (- (car bounds) (cdr bounds)))))
      (when iact
        (message "   %s " (format "%s" length)))
      length)))

(defun ar-th-ratio-base (cla elt &optional beg end ratio iact)
  (let ((beg
         (cond (beg beg)
               ((region-active-p)
                (region-beginning))
               (t
                (funcall (intern-soft (concat "ar-" (format "%s" elt) "-beginning-position-atpt"))))))
	(end
         (cond (end (copy-marker end))
               ((region-active-p)
                (copy-marker (region-end)))
               (t
                (condition-case nil (copy-marker (funcall (intern-soft (concat "ar-" (format "%s" elt) "-end-position-atpt")))) (error nil))))))
    (ar-th-ratio elt cla beg end ratio iact)))

(defun ar-th-ratio (thing cla &optional beg end ratio iact)
  (condition-case nil
      (let* ((beg (or beg (car (ar-th-bounds thing))))
             (end (or end (cdr (ar-th-bounds thing))))
             (matchcount 0)
             (erg 0)
             (countfunction
              (cond ((member cla ar-atpt-classes)
                     (if (featurep 'xemacs)
                         '(string-to-number (string-strip (count-matches (eval cla)) nil "a-z "))
                       '(string-to-number (format "%f" (count-matches (concat "[[:" (format "%s" cla) ":]]") (or beg (point-min)) (or end (point-max)))))))
                    (t '(lambda ()
                          (goto-char beg)
                          (while (and (< (point) end)
                                      (funcall (intern-soft (concat "ar-forward-" (format "%s" cla) "-atpt"))))
                            (setq matchcount (1+ matchcount)))
                          matchcount))))
             len)
        (save-restriction
          (save-excursion
            (narrow-to-region beg end)
            (setq matchcount (eval countfunction))
            (if ratio
                (progn
                  (setq len (string-to-number (format "%f" (- end beg))))
                  (setq erg (/ matchcount len))
                  (when iact (message "erg: %f" erg))
                  erg)
              (when iact (message "%s" matchcount))
              matchcount))))
    (error nil)))

(defun ar-th-escape (thing &optional iact)
  " "
  (condition-case nil 
      (let ((escaped (regexp-quote (buffer-substring-no-properties (ar-th-beg thing)(ar-th-end thing)))))
	(when escaped
          (progn
            (unless (string= escaped (car kill-ring)) (kill-new escaped))
            (when iact
              (message "   %s" escaped))
            escaped)))
    (error nil)))

(defun ar-th-copy (thing &optional arg iact)
  (condition-case nil 
      (let ((newcopy (ar-th thing)))
	(when newcopy
          (progn
            (unless (string= newcopy (car kill-ring)) (kill-new newcopy))
            (when iact
              (message "   %s" newcopy))
            newcopy)))
    (error nil)))

(defun ar-th-trim (thing &optional left right)
  "Trims given THING at point.
If boundaries of thing are know, use `ar-th-trim-base' directly. "
  (let* ((bounds (ar-th-bounds thing))
         (ap (car bounds))
         (ep (cdr bounds)))
    (ar-th-trim-base ap ep left right)))

(defun ar-th-trim-base (ap ep left right)
  "Trim buffer-substring resp. to args starting-point, end-point, left-trim, right-trim. "
  (cond ((and left right)
         (goto-char ep)
         (delete-char -1)
         (goto-char ap)
         (delete-char 1))
        (right
         (goto-char ep)
         (delete-char -1))
        (left
         (goto-char ap)
         (delete-char 1))
        (t (goto-char ep)
           (delete-char -1)
           (goto-char ap)
           (delete-char 1))))

(defun ar-th-trim-left (thing)
  (ar-th-trim thing t))

(defun ar-th-trim-right (thing)
  (ar-th-trim thing nil t))

(defun ar-th-peel (thing &optional arg iact)
  "Remove the outer element of an hierarchical form.

\(foo (bar baz)) --> (bar baz)
--^-----------

\[foo [bar baz]] --> [bar baz]
--^-----------

Inspired by stuff like `paredit-splice-sexp-killing-backward'; however, instead of working `-backward' or `-forward' deletes expression at point.

"
  (let ((outer (ar-th-bounds thing))
        inner) 
    (when (eq (point) (car outer))(forward-char 1))
    (skip-syntax-forward "^(")
    (setq inner (point)) 
    (goto-char (cdr outer))
    (if (< 0 (skip-syntax-backward "^)"))
        (progn 
          (delete-region (point) (cdr outer)))
      (delete-char -1) 
      (goto-char inner)
      (delete-region (car outer) (point)))))

(defun ar-th-comment (thing &optional arg iact) 
  "Comment or uncomment THING "
  (condition-case nil 
      (let* ((bounds (ar-th-bounds thing))
	     (beg (car bounds))
	     (end (cdr bounds)))
	(if (and beg end)
	    (progn 
	      (goto-char beg)
              ;;               (if (functionp 'ar-comment-indent-lor)
              ;;                   (ar-comment-indent-lor arg beg end)
              (comment-dwim nil)
              ;;)
              nil)))
    (error nil)))

(defun ar-th-delete-in-region (thing beg end &optional iact) 
  "Delete THING in region. Delete line, if empty afterwards. "
  (condition-case nil 
      (save-excursion
        (goto-char beg)
        (while (< (ar-th-forward thing) end)
          (let ((bounds (ar-th-bounds thing)))
            (delete-region (car bounds) (cdr bounds))
            (when iact (message "%s at pos %d %d %s " thing (car bounds) (cdr bounds) "deleted"))
            (when (empty-line-p)
              (delete-region (line-beginning-position) (1+ (line-end-position)))))))))

(defun ar-th-mark (thing &optional bounds) 
  " "
  (condition-case nil 
      (let* ((bounds (or bounds (ar-th-bounds thing))))
	(goto-char (car bounds))
	(push-mark (point) t t)
	(goto-char (cdr bounds))
	(exchange-point-and-mark))
    (error nil)))

;; uses sgml-tag from sgml-mode.el
(defun ar-th-hide (thing &optional beg end) 
  "Hide visibility of existing things at point. "
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t) bounds)
    (unless (and beg end)
      (setq bounds (ar-th-bounds thing))
      (setq beg (car bounds))
      (setq end (cdr bounds)))
    (if (and beg end)
        (progn 
          (put-text-property beg end 'category 'sgml-tag)
          (set-buffer-modified-p modified))
      (error (concat "No " (format "%s" thing) " at point!")))))

(defun ar-th-separate (thing &optional arg iact) 
  " "
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing))
             (beg (copy-marker (car bounds)))
             (end (copy-marker (cdr bounds))))
        (goto-char beg)
        (when (not (looking-back "^[ \t\r\f\n]+"))
          (newline ar-newlines-separate-before))
        (indent-according-to-mode)
        (save-excursion
          (goto-char end)
          (cond ((eobp)
                 (newline ar-newlines-separate-after))
                ((looking-at "[ \f\r\t\n]+")
                 nil)
                (t (newline ar-newlines-separate-after))))
        (list beg end))
    (error nil)))

(defun ar-th-show (thing &optional beg end) 
  "Remove invisibility of existing things at point. "
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t) bounds)
    (unless (and beg end)
      (setq bounds (ar-th-bounds thing))
      (setq beg (car bounds))
      (setq end (cdr bounds)))
    (if (and beg end)
        (progn 
          (remove-text-properties beg end '(category nil))
          (set-buffer-modified-p modified))
      (error (concat "No " (format "%s" thing) " at point!")))))

(defun ar-th-hide-show (thing &optional beg end) 
  "Toggle visibility of existing things at point. "
  (interactive) 
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t) erg bounds)
    (unless (and beg end)
      (setq bounds (ar-th-bounds thing))
      (setq beg (car bounds))
      (setq end (cdr bounds)))
    (if (eq 'sgml-tag (get-text-property beg 'category))
        (remove-text-properties beg end '(category nil))
      (put-text-property beg end 'category 'sgml-tag))
    (set-buffer-modified-p modified)))

(defun ar-thing-in-thing (thing-1th thing-2th th-function &optional iact beg-2th end-2th)
  "Addresses things of 1th kind within the borders of the 2th,
If optional positions BEG-2TH END-2TH are given, works on them instead.
With optional arg IACT, the resulting list is sent to the message-buffer too. "
  (let ((beg (cond (beg-2th beg-2th)
                   ((region-active-p)
                    (region-beginning))))
        (end (cond (end-2th end-2th)
                   ((region-active-p)
                    (copy-marker (region-end)))))
        bounds erg res)
    (unless (and beg end)
      (setq bounds (ar-th-bounds thing-2th t))
      (setq beg (car bounds))
      (setq end (copy-marker (cdr bounds))))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char beg)
        (if (eq th-function 'ar-th-sort)
            (ar-th-sort thing-1th nil beg end nil nil nil)
          (while (ar-th-forward thing-1th)
            (add-to-list 'erg
                         (funcall th-function thing-1th))
            (when (< (point) (match-end 0))(goto-char (match-end 0)))))))
    (when iact
      (if erg (message "%s" erg)
        (message "%s" "nil")))
    erg))

(defun ar-th-kill (thing &optional arg iact) 
  " "
  (condition-case nil 
      (let* ((bounds (ar-th-bounds thing)) 
	     (beg (car bounds))
	     (end (cdr bounds)))
	(kill-region beg end))
    (error nil)))

(defun ar-th-kill-backward (thing &optional arg iact) 
  " "
  (ar-th-backward thing arg iact) 
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing))
             (beg (car bounds))
             (end (cdr bounds)))
        (kill-region beg end))
    (error nil)))

(defun ar-th-commatize (thing &optional arg iact) 
  " "
  (condition-case nil 
      (let* ((bounds (ar-th-bounds thing)) 
	     (beg (car bounds))
	     (end (cdr bounds)))
	(goto-char end)
        (insert ","))
    (error nil)))

(defun ar-th-quote (thing &optional arg iact) 
  " "
  (condition-case nil 
      (let* ((bounds (ar-th-bounds thing)) 
	     (beg (car bounds))
	     (end (cdr bounds)))
        (goto-char beg)
        (insert "'"))
    (error nil)))

(defun ar-th-forward (thing &optional arg iact) 
  "Return args THING from point, if any, searches backward with negative argument. "
  (ignore-errors
    (let* ((orig (point))
           (arg (or arg 1))
           (bounds (ar-th-bounds thing))
           (ap (car bounds))
           (ep (cdr bounds))
           erg)
      (if (< 0 arg)
          (progn
            (if (ignore-errors (< orig (1- ep)))
                (goto-char (1- ep))
              (when (ignore-errors (eq orig (1- ep)))
                (forward-char 1))
              (while (and (< 0 arg)(not (eobp)))
                (if (functionp (get thing 'forward-op-at))
                    (progn
                      (funcall (get thing 'forward-op-at))
                      (setq bounds (ar-th-bounds thing)))
                  (while (and (not (eobp))(or (not (setq bounds (ar-th-bounds thing)))))
                    (if (and (eq thing 'comment) (equal comment-end ""))(forward-line 1) (forward-char 1))))
                ;; (forward-char 1)))
                (setq arg (1- arg)))
              (setq ap (car bounds))
              (setq ep (cdr bounds))
              (if (and ep (< orig (1- ep)))
                  (progn
                    (when iact 
                      (push-mark ep)
                      (goto-char ap)
                      (exchange-point-and-mark)
                      (kill-new (buffer-substring-no-properties ap ep))
                      (message "%s" ep))
                    (goto-char (1- ep))
                    ep)
                (when iact (message "%s" nil))
                nil)))
        (while
            (and (> 0 arg)(not (bobp)))
          (if (ignore-errors (< ap orig))
              (goto-char ap)
            (when (ignore-errors (eq orig ap) (forward-char -1)))
            (if (functionp (get thing 'backward-op-at))
                (progn
                  (funcall (get thing 'backward-op-at))
                  (setq bounds (ar-th-bounds thing)))
              (while
                  (and (not (bobp))(not (setq bounds (ar-th-bounds thing))))
                (forward-char -1))))
          (setq arg (1+ arg))) 
        (setq ap (car bounds))
        (setq ep (cdr bounds))
        (if (and ap (< ap orig))
            (progn
              (if iact
                  (progn
                    (goto-char ep)
                    (push-mark ap)
                    (exchange-point-and-mark)
                    (kill-new (buffer-substring-no-properties ap ep))
                    (when iact (message "%s" ap)))
                (goto-char ap))
              ap)
          (when iact (message "%s" nil))
          nil)))))

(defun ar-th-un-ml (thing &optional beg end)
  (save-excursion
    (save-restriction
      (when (and beg end)
        (narrow-to-region beg end))
      (let\* ((startstring (eval (intern-soft (concat (format "%s" thing) "-startstring-atpt"))))
              (endstring (concat (eval (intern-soft (concat (format "%s" thing) "-endstring-atpt")))))
              (begstringpos
               (progn
                 (beginning-of-form-base startstring endstring)
                 (if (looking-at startstring)
                     (list (match-beginning 0) (match-end 0))
                   (error "Can't see startstring"))))
              (thisbeg (copy-marker (car begstringpos)))
              thisend)
             (forward-char 1)
             (end-of-form-base startstring endstring)
             (when (looking-back endstring)
               (replace-match "")
               (setq thisend (copy-marker (point)))
               (delete-region (car begstringpos) (cadr begstringpos))
               (list thisbeg thisend))))
    (widen)))

(defun ar-th-backward (thing &optional arg iact) 
  "Returns beg and end of THING before point as a list. "
  (condition-case nil
      (ar-th-forward thing (- (or arg 1)) iact)
    (error nil)))

(defun ar-th-before (thing &optional arg iact)
  " "
  (let* ((arg (or arg -1)) 
         (bounds-point-at (ar-th-bounds thing))
         (beg (car-safe bounds-point-at))
         bounds)
    (when beg (goto-char beg))
    (setq bounds (ar-th-backward thing (abs arg)))
    (ar-th thing arg iact)))

(defun ar-th-bounds-before (thing &optional arg iact)
  " "
  (save-excursion 
    (let* ((arg (or arg -1)) 
	   (bounds-point-at (ar-th-bounds thing))
	   (beg (car-safe bounds-point-at))
	   (bounds
            (if beg (progn
                      (goto-char beg)
                      (ar-th-bounds thing))
              (ar-th-backward thing (abs arg))
              (ar-th-bounds thing))))
      (when iact (message "%s" bounds))
      bounds)))

(defun ar-th-before-beg-pos (thing &optional arg iact)
  " "
  (save-excursion 
    (let* ((arg (or arg -1)) 
           (bounds-point-at (ar-th-bounds thing))
           (beg (car-safe bounds-point-at))
           bounds)
      (when beg (goto-char beg))
      (ar-th-backward thing (abs arg))
      (let ((beg (car (ar-th-bounds thing))))
        (when iact (message "%s" beg)) beg))))

(defun ar-th-before-end-pos (thing &optional arg iact)
  " "
  (save-excursion 
    (let* ((arg (or arg -1)) 
           (bounds-point-at (ar-th-bounds thing))
           (beg (car-safe bounds-point-at))
           bounds)
      (when beg (goto-char beg))
      (ar-th-backward thing (abs arg))
      (let ((end (cdr-safe (ar-th-bounds thing))))
        (when iact (message "%s" end)) end))))

(defun ar-th-after (thing &optional arg iact)
  " "
  (let* ((arg (or arg 1)) 
         (bounds-point-at (ar-th-bounds thing))
         (end (cdr-safe bounds-point-at))
         bounds)
    (when end (goto-char end))
    (ar-th-forward thing arg)
    (ar-th thing arg iact)))

(defun ar-th-bounds-after (thing &optional no-delimiters iact)
  " "
  (save-excursion 
    (let ((bounds 
           (progn
             (ar-th-forward thing no-delimiters)
             (ar-th-bounds thing no-delimiters))))
      (when iact (message "%s" bounds)) bounds)))

(defun ar-th-after-beg-pos (thing &optional arg iact)
  " "
  (save-excursion 
    (let* ((arg (or arg 1)) 
           (bounds-point-at (ar-th-bounds thing))
           (end (cdr-safe bounds-point-at))
           bounds)
      (when end (goto-char end))
      (let*  ((bounds (progn (ar-th-forward thing arg)
                             (ar-th-bounds thing)))
              (beg (car-safe bounds)))
        (when iact (message "%s" beg)) beg))))

(defun ar-th-after-end-pos (thing &optional arg iact)
  " "
  (save-excursion 
    (let* ((arg (or arg 1)) 
           (bounds-point-at (ar-th-bounds thing))
           (end (cdr-safe bounds-point-at))
           bounds)
      (when end (goto-char end))
      (let*  ((bounds (progn
                        (ar-th-forward thing arg)
                        (ar-th-bounds thing)))
              (end (cdr-safe bounds)))
        (when iact (message "%s" end)) end))))

(defvar paired-start-pos nil)

(defun ar-th-transpose (thing &optional arg iact)
  "Returns position, when called from a program
 end of transposed section. "
  (let* ((pos (copy-marker (point)))
         (first (ar-th-bounds thing))
         (pos1 (if (ignore-errors (<= (car first) pos))
                   first
                 (ar-th-bounds-before thing)))
         (pos2 (progn
                 (when (ignore-errors (< 1 arg))
                   (ar-th-forward thing arg))
                 (ar-th-bounds-after thing)))
         (a (car pos1))
         (b (copy-marker (cdr pos1)))
         (c (car pos2))
         (d (copy-marker (cdr pos2))))
    (transpose-regions a b c d)
    (if iact
        (progn
          (message "%s" (point))
          (point))
      (goto-char d)
      d)))

;; credits to sort-subr, sort.el
;; (reverse nextrecfun endrecfun &optional startkeyfun endkeyfun predicate)
(defun ar-th-sort (thing reverse beg end startkeyfun endkeyfun predicate)
  (save-excursion
    (save-restriction
      (unless (buffer-narrowed-p)(narrow-to-region beg end))
      (goto-char (point-min))
      (let ((reverse (or reverse nil))
            (startkeyfun (or startkeyfun nil))
            (endkeyfun (or endkeyfun nil))
            (predicate (or predicate nil))
            (this-beg (ar-th-beg thing 0)))
        (while (not (or (eobp)(stringp (ar-th thing))))
          (forward-char 1))
        (if (eq thing 'number)
            (ar-sort-numbers-subr reverse
                                  (function (lambda () (if (ar-th-forward thing) (ar-th-gotobeg thing) (goto-char (point-max)))))
                                  (function (lambda () (ar-th-gotoend thing)(forward-char 1))) startkeyfun endkeyfun predicate)
          (sort-subr reverse
                     (function (lambda () (if (ar-th-forward thing) (ar-th-gotobeg thing) (goto-char (point-max)))))
                     (function (lambda () (ar-th-gotoend thing)(forward-char 1))) startkeyfun endkeyfun predicate))))))

(defun ar-sort-numbers-subr (reverse nextrecfun endrecfun
                                     &optional startkeyfun endkeyfun predicate)
  "A patched sort-subr. Divides buffer into records and sort them.

We divide the accessible portion of the buffer into disjoint pieces
called sort records.  A portion of each sort record (perhaps all of
it) is designated as the sort key.  The records are rearranged in the
buffer in order by their sort keys.  The records may or may not be
contiguous.

Usually the records are rearranged in order of ascending sort key.
If REVERSE is non-nil, they are rearranged in order of descending sort key.
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order.

The next four arguments are functions to be called to move point
across a sort record.  They will be called many times from within sort-subr.

NEXTRECFUN is called with point at the end of the previous record.
It moves point to the start of the next record.
It should move point to the end of the buffer if there are no more records.
The first record is assumed to start at the position of point when sort-subr
is called.

ENDRECFUN is called with point within the record.
It should move point to the end of the record.

STARTKEYFUN moves from the start of the record to the start of the key.
It may return either a non-nil value to be used as the key, or
else the key is the substring between the values of point after
STARTKEYFUN and ENDKEYFUN are called.  If STARTKEYFUN is nil, the key
starts at the beginning of the record.

ENDKEYFUN moves from the start of the sort key to the end of the sort key.
ENDKEYFUN may be nil if STARTKEYFUN returns a value or if it would be the
same as ENDRECFUN.

PREDICATE is the function to use to compare keys.  If keys are numbers,
it defaults to `<', otherwise it defaults to `string<'."
  ;; Heuristically try to avoid messages if sorting a small amt of text.
  (let ((messages (> (- (point-max) (point-min)) 50000)))
    (save-excursion
      (if messages (message "Finding sort keys..."))
      (let* ((sort-lists (sort-build-lists nextrecfun endrecfun
					   startkeyfun endkeyfun))
	     (old (reverse sort-lists))
	     (case-fold-search sort-fold-case))
	(if (null sort-lists)
	    ()
	  (or reverse (setq sort-lists (nreverse sort-lists)))
	  (if messages (message "Sorting records..."))
	  (setq sort-lists
		(sort sort-lists
                      (lambda (a b) 
                        (< (string-to-number (buffer-substring-no-properties (caar a) (cdar a)))(string-to-number (buffer-substring-no-properties (caar b)(cdar b)))))))
	  (if reverse (setq sort-lists (nreverse sort-lists)))
	  (if messages (message "Reordering buffer..."))
	  (sort-reorder-buffer sort-lists old)))
      (if messages (message "Reordering buffer... Done"))))
  nil)

(defun ar-th-delim (thing &optional arg beg-char end-char iact)
  "Process begin and end of region according to value of
  `delim-action'
  If no region is active, process borders of THING-at-point
  according to value of delim-action-beginning- resp. -end-position
  Default is symbol-at.
  With \C-u or arg `escaped' to `t': insert escaped doublequotes"
  (interactive "*p")
  (save-restriction
    (let ((orig (point))
          done narrow)
      (if (if (featurep 'xemacs)
              (region-active-p)
            (use-region-p))
          (progn
            (narrow-to-region (region-beginning) (region-end))
            (goto-char (point-min))
            (setq orig (point)))
        (setq narrow t))
      (ar-th-delim-intern narrow)
      ;; (forward-char 1)
      (while (and (or (not done) (< orig (point))) (funcall (intern-soft (concat "ar-forward-" (prin1-to-string thing) "-atpt"))))
        (ar-th-delim-intern)
        (forward-char 1)
        (setq done t)))))

(defun ar-th-delim-intern (&optional narrow)
  (ignore-errors
    (let* ((pos (copy-marker (point)))
           (bounds (ar-th-bounds thing))
           (beg (car bounds))
           (end (copy-marker (cdr bounds)))
           (begstr (or beg-char th-beg-delimiter))
           (endstr (or end-char th-end-delimiter)))
      (when narrow (narrow-to-region beg end))
      (when beg
        (goto-char beg)
        (delim-slash-function arg)
        (insert begstr)
        (goto-char end)
        (delim-slash-function arg)
        (insert endstr)
        (setq done t)
        (when iact (message "%s" bounds))))))

(defun delim-slash-function (arg)
  " "
  (when (eq arg 2)
    (insert "\\"))
  (when (eq arg 4)
    (insert "\\\\")))

;;;###autoload 
(defun ar-th-base-copy-or (kind arg &optional iact)
  " "
  (let* ((expr (format "%s" kind))
         (arg (if arg (prefix-numeric-value arg) 1))
         (suffix
          (when (or (member kind ar-atpt-delimlist) 
                    ;; (loop for e in ar-atpt-delimlist-unpaired if (member kind e) return e))
                    (member kind ar-atpt-delimlist-unpaired))
            (if (string-match "e$" expr)
                "d" "ed")))
         erg)
    (if (< 0 arg)
        (if (region-active-p)
            (setq erg (funcall (intern-soft (concat "ar-" expr "-region-atpt")) arg))
          (setq erg (funcall (intern-soft (concat "ar-" expr suffix "-atpt")) arg)))
      (setq erg (funcall (intern-soft (concat "ar-kill-" expr suffix "-atpt")) arg)))))
     
;; ar-python-triplequote-raw start

(defun ar-th-triplequote (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "\"\"\"\\\\\\\\\|\'\'\'" "\"\"\"\\\\\\\\\|\'\'\'" iact))

(defun ar-th-triplequote-dq (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "\"\"\"" "\"\"\"" iact))

(defun ar-th-triplequote-sq (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "'''" "'''" iact))

;; ar-insert-delimit-unpaired start

(defun ar-th-backslash (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "\\" "\\" iact))

(defun ar-th-dollar (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "$" "$" iact))

(defun ar-th-doublequote (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "\"" "\"" iact))

(defun ar-th-equalize (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "=" "=" iact))

(defun ar-th-hyphen (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "-" "-" iact))

(defun ar-th-singlequote (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "'" "'" iact))

(defun ar-th-slash (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "/" "/" iact))

(defun ar-th-underscore (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "_" "_" iact))

(defun ar-th-whitespace (thing &optional arg iact)
  " "
  (ar-th-delim thing arg " " " " iact))
;; ar-insert-delimit-unpaired end

;; ar-insert-delimit-forms start

(defun ar-th-brace (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "{" "}" iact))

(defun ar-th-bracket (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "[" "]" iact))

(defun ar-th-lesser-angle (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "<" ">" iact))

(defun ar-th-greater-angle (thing &optional arg iact)
  " "
  (ar-th-delim thing arg ">" "<" iact))

(defun ar-th-left-right-singlequote (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "‘" "’" iact))

(defun ar-th-parentize (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "(" ")" iact))
;; ar-insert-delimit-forms end

;; ar-atpt-data-forms-aktiv start

(defun ar-th-begin-end-quote (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "\\begin{quote}" "\\end{quote}" iact))

(defun ar-th-blok (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "{%" "%}" iact))

(defun ar-th-double-backslash (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "\\\\" "\\\\" iact))

(defun ar-th-doubleslash (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "//" "//" iact))

(defun ar-th-doubleslash-paren (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "\\\\(" "\\\\)" iact))

(defun ar-th-tabledata (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "<td[^>]*>" "</td>" iact))

(defun ar-th-slash-paren (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "\\(" "\\)" iact))

(defun ar-th-xsl-stylesheet (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "<xsl:stylesheet[^<]+>.*$" "</xsl:stylesheet>" iact))

(defun ar-th-xsl-template (thing &optional arg iact)
  " "
  (ar-th-delim thing arg "<xsl:template[^<]+>.*$" "</xsl:template>" iact))
;; ar-atpt-data-forms-aktiv end


(defun ar-th-slash-base (thing startstring endstring &optional arg iact)
  (let* ((bounds (ar-th-bounds thing arg iact))
         (beg (car bounds))
         (end (copy-marker (cdr bounds))))
    (goto-char end)
    (forward-char -1)
    (insert endstring)
    (goto-char beg)
    (insert startstring)
    (goto-char (1+ end))))
          


;; ar-insert-thingatpt-syntax-funktionen start

(defun ar-syntax-class-atpt (&optional pos) 
  "Return the syntax class part of the syntax at point. "
  (interactive)
  (let* ((pos (or pos (point))) 
         (erg (logand (car (syntax-after pos)) 65535)))
    (when (interactive-p) (message "%s" erg)) erg))

(defun syntax-class-bfpt () 
  "Return the syntax class part of the syntax at point. "
  (interactive)
  (let ((erg (logand (car (syntax-after (1- (point)))) 65535)))
    (when (interactive-p) (message "%s" erg)) erg))

(defun ar-syntax-atpt (&optional docu pos) 
  (interactive)
  (when pos
    (goto-char pos))
  (let* ((elt (car (if (featurep 'xemacs)
                       (char-syntax (char-after))
                     (syntax-after (point)))))
         (stax (cond ((eq elt 0) "0 whitespace")
                     ((eq elt 5) "5 close parenthesis")
                     ((eq elt 10) "10 character quote")
                     ((eq elt 1) "1 punctuation")
                     ((eq elt 6) "6 expression prefix")
                     ((eq elt 11) "11 comment-start")
                     ((eq elt 2) "2 word")
                     ((eq elt 7) "7 string quote")
                     ((eq elt 12) "12 comment-end")
                     ((eq elt 3) "3 symbol")
                     ((eq elt 8) "8 paired delimiter")
                     ((eq elt 13) "13 inherit")
                     ((eq elt 4) "4 open parenthesis")
                     ((eq elt 9) "9 escape")
                     ((eq elt 14) "14 generic comment")
                     ((eq elt 15) "15 generic string"))))
    (when (interactive-p)
      (message (format "%s" stax)))
    (if docu
        (format "%s" stax)
      elt)))

(defun ar-syntax-in-region-atpt (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (let (erg)
      (while (< (point) end)
        (setq erg (concat erg "\n" "\"" (char-to-string (char-after)) "\"" "  is " (ar-syntax-atpt t)))
        (forward-char 1))
      (message "%s" erg))))

(defun syntax-bfpt () 
  (interactive)
  (let ((stax (syntax-after (1- (point)))))
    (when (interactive-p)
      (message (format "%s" stax)))
    stax))

;; Listen start

(setq ar-atpt-unpaired-delimited-extended-list
      (list
       '(backslashed "\\")
       '(dollared "\\$")
       '(doublequoted "\"")
       '(equalized "=")
       '(hyphened "-")
       '(quoted "\"\\|'")
       '(singlequoted "'")
       '(slashed "/")
       '(underscored "_")
       '(whitespaced " ")))

(setq ar-atpt-data-forms-aktiv
      (list
       'begin-end-quote
       'blok
       'double-backslash
       'doubleslash
       'doubleslash-paren
       'tabledata
       'slash-paren
       'xsl-stylesheet
       'xsl-template))

(setq ar-atpt-data-forms-passiv
      (list
       'begin-end-quoted
       'blok
       'double-backslashed
       'doubleslashed
       'doubleslashed-paren
       'tabledata-p
       'slashed-paren
       'xsl-stylesheet-p
       'xsl-template-p))

(setq ar-atpt-python-list (list 'py-block 'py-block-or-clause 'py-class 'py-clause 'py-def-or-class 'py-def 'py-expression 'py-partial-expression 'py-statement 'py-string))

(setq ar-atpt-python-quoted (list 'triplequoted 'triplequoted-dq 'triplequoted-sq))

(setq ar-python-triplequote (list 'triplequote 'triplequote-dq 'triplequote-sq))

(setq ar-atpt-markup-list (list 'begin-end-quote 'blok 'double-backslashed 'doubleslashed 'doubleslashed-paren 'markup 'ml-data 'ml-attribut 'ml-tag 'slashed-paren 'tabledata 'xsl-stylesheet 'xsl-template))

(setq ar-atpt-delimlist (list 'brace 'bracket 'lesser-angle 'greater-angle 'left-right-singlequote 'parentize))

(setq ar-atpt-delimited-list (list 'braced 'bracketed 'lesser-angled 'greater-angled 'left-right-singlequoted 'parentized))

(setq ar-atpt-delimlist-unpaired (list 'backslash 'dollar 'doublequote 'equalize 'hyphen 'singlequote 'slash 'underscore 'whitespace))

(setq ar-atpt-unpaired-delimited-list (list 'backslashed 'dollared 'doublequoted 'equalized 'hyphened 'quoted 'singlequoted 'slashed 'underscored 'whitespaced))

(setq ar-atpt-classes (list 'alnum 'alpha 'ascii 'blank 'cntrl 'digit 'graph 'lower 'nonascii 'print 'punct 'space 'upper 'xdigit))

(setq ar-atpt-region-only (list 'region))

(setq ar-atpt-rest-list (list 'abbrev 'acronym 'angled-no-nest 'greater-angled-nested 'lesser-angled-nested 'buffer 'comment 'csv 'date 'defun 'delimited 'email 'filename 'float 'function 'ip 'isbn 'line 'name 'number 'page 'paragraph 'paren 'phone 'region 'sentence 'sexp 'string 'sh-struct 'symbol 'url 'word 'word-alpha-only))

(setq ar-atpt-major-forms-restricted-list (list 'buffer 'page 'paragraph 'region))

(setq ar-atpt-counts-list (list 'angled-no-nest 'greater-angled-nested 'lesser-angled-nested 'csv 'line 'paragraph 'region 'sentence 'string 'buffer))

(setq ar-atpt-unary-operations-raw (list 'commatize 'quote))



(provide 'thingatpt-utils-base)
;;; thingatpt-utils-base.el ends here
