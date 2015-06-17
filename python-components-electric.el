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
        ((and py-electric-colon-bobl-only (save-excursion (py-beginning-of-statement) (not (py--beginning-of-block-p))))
         (self-insert-command (prefix-numeric-value arg)))
        ((eq 4 (prefix-numeric-value arg))
         (self-insert-command 1))
        (t (insert ":")
           (unless (py-in-string-or-comment-p)
             (let ((orig (copy-marker (point)))
                   (indent (py-compute-indentation)))
               (unless (or (eq (current-indentation) indent)
                           (and py-electric-colon-greedy-p
                                (eq indent (save-excursion (py-beginning-of-statement)(current-indentation))))
                           (and (py--top-level-form-p)(< (current-indentation) indent)))
                 (beginning-of-line)
                 (delete-horizontal-space)
                 (indent-to indent))
               (goto-char orig))
             (when py-electric-colon-newline-and-indent-p
               (py-newline-and-indent))))))

(defun py-electric-space (arg)
  "Close completion buffer when it's sure, it's no longer needed, i.e. when inserting a space.

Works around a bug in `choose-completion'. "

  (interactive "*P")
  (cond ((not py-electric-space-active-p)
         (self-insert-command (prefix-numeric-value arg)))
        ((eq 4 (prefix-numeric-value arg))
         (self-insert-command 1))
        (t (if (interactive-p) (self-insert-command (prefix-numeric-value arg))
             ;; used from dont-indent-code-unnecessarily-lp-1048778-test
             (insert " ")))))

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

(defun py-empty-out-list-backward ()
  "Deletes all elements from list before point. "
  (interactive "*")
  (and (member (char-before) (list ?\) ?\] ?\}))
       (let ((orig (point))
             (thischar (char-before))
             pps cn)
         (forward-char -1)
         (setq pps (syntax-ppss))
         (if (and (not (nth 8 pps)) (nth 1 pps))
             (progn
               (goto-char (nth 1 pps))
               (forward-char 1))
           (cond ((or (eq thischar 41)(eq thischar ?\)))
                  (setq cn "("))
                 ((or (eq thischar 125) (eq thischar ?\}))
                  (setq cn "{"))
                 ((or (eq thischar 93)(eq thischar ?\]))
                  (setq cn "[")))
           (skip-chars-backward (concat "^" cn)))
         (delete-region (point) orig)
         (insert-char thischar 1)
         (forward-char -1))))

(defun py-electric-backspace (&optional arg)
  "Delete preceding character or level of indentation.

When `delete-active-region' and (region-active-p), delete region.

Unless at indentation:
  With `py-electric-kill-backward-p' delete whitespace before point.
  With `py-electric-kill-backward-p' at end of a list, empty that list.

Returns column reached. "
  (interactive "p*")
  (or arg (setq arg 1))
  (let (erg)
    (cond ((and (region-active-p)
		;; Emacs23 doesn't know that var
		(boundp 'delete-active-region) delete-active-region)
	   (backward-delete-char-untabify arg))
	  ;; (delete-region (region-beginning) (region-end)))
	  ((looking-back "^[ \t]+")
	   (let* ((remains (% (current-column) py-indent-offset)))
	     (if (< 0 remains)
		 (delete-char (- remains))
	       (indent-line-to (- (current-indentation) py-indent-offset)))))
	  ((and py-electric-kill-backward-p (member (char-before) (list ?\) ?\] ?\})))
	   (py-empty-out-list-backward))
	  ((and py-electric-kill-backward-p  (< 0 (setq erg (abs (skip-chars-backward " \t\r\n\f")))))
	   (delete-region (point) (+ erg (point))))
	  (t (delete-char (- 1))))
    (setq erg (current-column))
    (when (and (interactive-p) py-verbose-p) (message "%s" erg))
    erg))

(defun py-electric-delete (&optional arg)
  "Delete following character or levels of whitespace.

When `delete-active-region' and (region-active-p), delete region "
  (interactive "*p")
  (let ((orig (point)))
    (cond ((and (region-active-p)
		;; Emacs23 doesn't know that var
		(boundp 'delete-active-region) delete-active-region)
	   (delete-region (region-beginning) (region-end)))
	  ((save-excursion (and (< (current-column)(current-indentation)) (<= py-indent-offset (skip-chars-forward " \t"))))
	   (goto-char orig)
	   (delete-char py-indent-offset))
	  ((< 0 (skip-chars-forward " \t"))
	   (delete-region orig (point)))
	  (t (delete-char (or arg 1))))))

(defun py-electric-yank (&optional arg)
  "Perform command `yank' followed by an `indent-according-to-mode' "
  (interactive "P")
  (cond (py-electric-yank-active-p
         (yank arg)
         ;; (py-indent-line)
         )
        (t (yank arg))))

;; required for pending-del and delsel modes
(put 'py-electric-colon 'delete-selection t) ;delsel
(put 'py-electric-colon 'pending-delete t) ;pending-del
(put 'py-electric-backspace 'delete-selection 'supersede) ;delsel
(put 'py-electric-backspace 'pending-delete 'supersede) ;pending-del
(put 'py-electric-delete 'delete-selection 'supersede) ;delsel
(put 'py-electric-delete 'pending-delete 'supersede) ;pending-del

(provide 'python-components-electric)
;;; python-components-electric.el ends here
