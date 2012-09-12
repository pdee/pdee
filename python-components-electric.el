;;; python-components-electric.el --- Python-mode electric inserts

;; Maintainer: Andreas Roehler <andreas.roehler@online.de>
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
(require 'python-components-macros) 

(defun py-electric-colon (arg)
  "Insert a colon and indent accordingly.

If a numeric argument ARG is provided, that many colons are inserted
non-electrically.

Electric behavior is inhibited inside a string or
comment or by universal prefix C-u.

Switched by `py-electric-colon-active-p', default is nil
See also `py-electric-colon-greedy-p' "
  (interactive "*P")
  (cond ((not py-electric-colon-active-p)
         (self-insert-command (prefix-numeric-value arg)))
        ((eq 4 (prefix-numeric-value arg))
         (self-insert-command 1))
        (t (if (interactive-p) (self-insert-command (prefix-numeric-value arg))
             ;; used from dont-indent-code-unnecessarily-lp-1048778-test
             (insert ":"))
           (unless (py-in-string-or-comment-p)
             (let ((orig (copy-marker (point)))
                   (indent (py-compute-indentation)))
               (unless (or (eq (current-indentation) indent)
                           (and (not py-electric-colon-greedy-p)
                                (eq (current-indentation)(save-excursion (py-beginning-of-block)(current-indentation)))) 
                           (and (py-top-level-form-p)(< (current-indentation) indent)))
                 (beginning-of-line)
                 (delete-horizontal-space)
                 (indent-to indent))
               (goto-char orig))
             (when py-electric-colon-newline-and-indent-p
               (py-newline-and-indent))))))

(defun py-electric-comment (arg)
  "Insert a comment. If starting a comment, indent accordingly.

If a numeric argument ARG is provided, that many \"#\" are inserted
non-electrically.
With \\[universal-argument] \"#\" electric behavior is inhibited inside a string or comment."
  (interactive "*P")
  (if (and py-indent-comments py-electric-comment-p)
      (if (ignore-errors (eq 4 (car-safe arg)))
          (insert "#")
        (when (and (eq last-command 'py-electric-comment) (looking-back " "))
          (forward-char -1))
        (if (interactive-p) (self-insert-command (prefix-numeric-value arg))
          (insert "#"))
        (let ((orig (copy-marker (point)))
              (indent (py-compute-indentation)))
          (unless
              ;; (or
               (eq (current-indentation) indent)
            ;; (looking-back "#[ \t]*"))
            (goto-char orig)
            (beginning-of-line)
            (delete-horizontal-space)
            (indent-to indent)
            (goto-char orig))
          (when py-electric-comment-add-space-p
            (unless (looking-at "[ \t]")
              (insert " "))))
        (setq last-command this-command))
    (self-insert-command (prefix-numeric-value arg))))

;; Electric deletion
(defun py-electric-backspace (&optional arg)
  "Delete preceding character or level of indentation.

With ARG do that ARG times.
Returns column reached. "
  (interactive "*p")
  (let ((arg (or arg 1))
        erg)
    (dotimes (i arg)
      (if (looking-back "^[ \t]+")
          (let* ((remains (% (current-column) py-indent-offset)))
            (if (< 0 remains)
                (delete-char (- remains))
              (indent-line-to (- (current-indentation) py-indent-offset))))
        (delete-char (- 1))))
    (setq erg (current-column))
    (when (and (interactive-p) py-verbose-p) (message "%s" erg))
    erg))

(defun py-electric-delete (&optional arg)
  "Delete following character or levels of whitespace.

With ARG do that ARG times. "
  (interactive "*p")
  (let ((arg (or arg 1)))
    (dotimes (i arg)
      (if (and (or (bolp)(looking-back "^[ \t]+")) (looking-at "[ \t]+"))
          (let* ((remains (% (+ (current-column) (- (match-end 0)(match-beginning 0))) py-indent-offset)))
            (if (< 0 remains)
                (delete-char remains)
              (delete-char py-indent-offset)))
        (delete-char 1)))))

;; (defun py-electric-delete (arg)
;;   "Delete preceding or following character or levels of whitespace.
;;
;; The behavior of this function depends on the variable
;; `delete-key-deletes-forward'.  If this variable is nil (or does not
;; exist, as in older Emacsen and non-XEmacs versions), then this
;; function behaves identically to \\[c-electric-backspace].
;;
;; If `delete-key-deletes-forward' is non-nil and is supported in your
;; Emacs, then deletion occurs in the forward direction, by calling the
;; function in `py-delete-function'.
;;
;; \\[universal-argument] (programmatically, argument ARG) specifies the
;; number of characters to delete (default is 1)."
;;   (interactive "*p")
;;   (if (or (and (fboundp 'delete-forward-p) ;XEmacs 21
;;                (delete-forward-p))
;;           (and (boundp 'delete-key-deletes-forward) ;XEmacs 20
;;                delete-key-deletes-forward))
;;       (funcall py-delete-function arg)
;;     (py-electric-backspace arg)))

;; required for pending-del and delsel modes
(put 'py-electric-colon 'delete-selection t) ;delsel
(put 'py-electric-colon 'pending-delete t) ;pending-del
(put 'py-electric-backspace 'delete-selection 'supersede) ;delsel
(put 'py-electric-backspace 'pending-delete 'supersede) ;pending-del
(put 'py-electric-delete 'delete-selection 'supersede) ;delsel
(put 'py-electric-delete 'pending-delete 'supersede) ;pending-del

(provide 'python-components-electric)
;;; python-components-electric.el ends here
