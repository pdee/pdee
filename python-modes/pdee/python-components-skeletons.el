;;; python-components-skeletons.el --- make use of python.el's templates for compound statements

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

;; Extractions and adaption done by Andreas Roehler,
;; andreas.roehler@online.de

;; Original Author: Dave Love <fx@gnu.org>
;; Maintainer: FSF
;; Created: Nov 2003
;; Keywords: languages

;; Python-components-mode started from python-mode.el
;; and python.el. Tim Peters, Barry A. Warsaw, Skip
;; Montanaro, Ken Manheimer, Dave Love and many others
;; wrote major parts.

;; Extractions done in order to make forms available for python-mode.el
;; Thanks to Dave Love providing that stuff.

;; Abbrev-mode is needed for template insert of common loops in Python. 
;; After typing a `for', user is prompted for the variable etc.

;;; Code:

;; 

;; "class"        0    ""                       python-insert-class
;; "def"          0    ""                       python-insert-def
;; "for"          0    ""                       python-insert-for
;; "if"           1    ""                       python-insert-if
;; "try/except"   0    ""                       python-insert-try/except
;; "try/finally"  0    ""                       python-insert-try/finally
;; "while"        0    ""                       python-insert-while

;;;; Skeletons


(defcustom python-use-skeletons nil
  "Non-nil means template skeletons will be automagically inserted.
This happens when pressing \"if<SPACE>\", for example, to prompt for
the if condition."
  :type 'boolean
  :group 'python)

(define-abbrev-table 'python-mode-abbrev-table ()
  "Abbrev table for Python mode."
  :case-fixed t
  ;; Allow / inside abbrevs.
  :regexp "\\(?:^\\|[^/]\\)\\<\\([[:word:]/]+\\)\\W*"
  ;; Only expand in code.
  :enable-function (lambda () (not (python-in-string/comment))))

(eval-when-compile
  ;; Define a user-level skeleton and add it to the abbrev table.
(defmacro def-python-skeleton (name &rest elements)
  (let* ((name (symbol-name name))
	 (function (intern (concat "python-insert-" name))))
    `(progn
       ;; Usual technique for inserting a skeleton, but expand
       ;; to the original abbrev instead if in a comment or string.
       (when python-use-skeletons
         (define-abbrev python-mode-abbrev-table ,name ""
           ',function
           nil t))                      ; system abbrev
       (define-skeleton ,function
	 ,(format "Insert Python \"%s\" template." name)
	 ,@elements)))))
(put 'def-python-skeleton 'lisp-indent-function 2)

;; From `skeleton-further-elements' set below:
;;  `<': outdent a level;
;;  `^': delete indentation on current line and also previous newline.
;;       Not quite like `delete-indentation'.  Assumes point is at
;;       beginning of indentation.

(def-python-skeleton if
  "Condition: "
  "if " str ":" \n
  > -1	   ; Fixme: I don't understand the spurious space this removes.
  _ \n
  ("other condition, %s: "
   <			; Avoid wrong indentation after block opening.
   "elif " str ":" \n
   > _ \n nil)
  '(python-else) | ^)

(define-skeleton python-else
  "Auxiliary skeleton."
  nil
  (unless (eq ?y (read-char "Add `else' clause? (y for yes or RET for no) "))
    (signal 'quit t))
  < "else:" \n
  > _ \n)

(def-python-skeleton while
  "Condition: "
  "while " str ":" \n
  > -1 _ \n
  '(python-else) | ^)

(def-python-skeleton for
  "Target, %s: "
  "for " str " in " (skeleton-read "Expression, %s: ") ":" \n
  > -1 _ \n
  '(python-else) | ^)

(def-python-skeleton try/except
  nil
  "try:" \n
  > -1 _ \n
  ("Exception, %s: "
   < "except " str '(python-target) ":" \n
   > _ \n nil)
  < "except:" \n
  > _ \n
  '(python-else) | ^)

(define-skeleton python-target
  "Auxiliary skeleton."
  "Target, %s: " ", " str | -2)

(def-python-skeleton try/finally
  nil
  "try:" \n
  > -1 _ \n
  < "finally:" \n
  > _ \n)

(def-python-skeleton def
  "Name: "
  "def " str " (" ("Parameter, %s: " (unless (equal ?\( (char-before)) ", ")
		     str) "):" \n
  "\"\"\"" - "\"\"\"" \n     ; Fixme:  extra space inserted -- why?).
  > _ \n)

(def-python-skeleton class
  "Name: "
  "class " str " (" ("Inheritance, %s: "
		     (unless (equal ?\( (char-before)) ", ")
		     str)
  & ")" | -2				; close list or remove opening
  ":" \n
  "\"\"\"" - "\"\"\"" \n
  > _ \n)

(defvar python-default-template "if"
  "Default template to expand by `python-expand-template'.
Updated on each expansion.")

(defun python-expand-template (name)
  "Expand template named NAME.
Interactively, prompt for the name with completion."
  (interactive
   (list (completing-read (format "Template to expand (default %s): "
				  python-default-template)
			  python-mode-abbrev-table nil t nil nil
                          python-default-template)))
  (if (equal "" name)
      (setq name python-default-template)
    (setq python-default-template name))
  (let ((sym (abbrev-symbol name python-mode-abbrev-table)))
    (if sym
        (abbrev-insert sym)
      (error "Undefined template: %s" name))))

(provide 'python-components-skeletons)
;;; python-components-skeletons.el ends here

