;;; python-components-comment.el -- Comment/uncomment python constructs at point

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages, convenience

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

;;; Code:

(defun py-backward-comment (&optional last orig)
  "Leave commented section upwards,  include empty lines.

Return position reached, if successful"
  (interactive)
  (unless (bobp)
    (let ((pps (parse-partial-sexp (point-min) (point)))
	  (last (or last (point)))
	  (orig (or orig (point))))
      (if (nth 4 pps)
	  (progn
	    (goto-char (nth 8 pps))
	    (setq last (point))
	    (skip-chars-backward " \t\r\n\f")
	    (py-backward-comment last orig))
	(when last (goto-char last)))
      (when (< (point) orig) (point)))))

(defun py-beginning-of-comment ()
  "Go to the beginning of current line's comment, if any.

Returns position if succesful. "
  (interactive)
  (save-restriction
    (widen)
    (let ((pps (parse-partial-sexp (point-min) (point))))
      (when (nth 4 pps)
        (goto-char
         (nth 8 pps))))))

;; (defun py-beginning-of-comment ()
;;   "Go to beginning of comment at point.
;;
;; Returns position, nil if not in comment."
;;   (interactive)
;;   (let ((orig (point))
;;         last erg done)
;;     (while (and (not done)(not (bobp)) (or (looking-at (concat "[ \t]*" comment-start))(nth 4 (parse-partial-sexp (point-min) (point)))(empty-line-p)))
;;       (when (nth 4 (parse-partial-sexp (point-min) (point)))
;;         (goto-char (nth 8 (parse-partial-sexp (point-min) (point))))
;;         (setq last (point)))
;;       (when (and last (< (point) last))
;;         (unless (empty-line-p)
;;           (setq last (point))))
;;       (if (looking-back "^[ \t]*")
;;           (forward-line -1)
;;         ;; inline comment
;;         (when (and (looking-at (concat "[ \t]*" comment-start)) (< (point) orig))(setq last (point)))
;;         (setq done t)))
;;     last))

(defun py-forward-comment ()
    "Go to the end of comment at point.

Returns position, nil if not in comment."

  (interactive)
  (let ((orig (point))
        last)
    (while (and (not (eobp)) (or (looking-at (concat "[ \t]*" comment-start))(nth 4 (parse-partial-sexp (point-min) (point)))(empty-line-p)))
      (unless (empty-line-p)
        (setq last (point)))
      (forward-line 1))
    (if last
        (progn
          (goto-char last)
          (end-of-line)
          (skip-chars-backward " \t\r\n\f")
          (point))
      last)))

(defun py--uncomment-intern (beg end)
  (uncomment-region beg end)
  (when py-uncomment-indents-p
    (py-indent-region beg end)))

(defun py-uncomment (&optional beg end)
  "Uncomment commented lines at point.

If region is active, restrict uncommenting at region "
  (interactive "*")
  (save-excursion
    (save-restriction
      (when (use-region-p)
        (narrow-to-region (region-beginning) (region-end)))
      (let* (last
             (beg (or beg (save-excursion
                            (while (and (py-beginning-of-comment) (setq last (point))(prog1 (forward-line -1)(end-of-line))))
                            last))))
        (and (py-end-of-comment))
        (py--uncomment-intern beg (point))))))

(defun py-comment-region (beg end &optional arg)
  "Like `comment-region' but uses double hash (`#') comment starter."
  (interactive "r\nP")
  (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start)))
    (comment-region beg end arg)))

(defun py-comment-block (&optional beg end arg)
  "Comments block at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-block-position)))
          (end (or end (py--end-of-block-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-minor-block (&optional beg end arg)
  "Comments a block started by a `for', `if', `try' or `with'.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-minor-block-position)))
          (end (or end (py--end-of-minor-block-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-top-level (&optional beg end arg)
  "Comments top-level form at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-top-level-position)))
          (end (or end (py--end-of-top-level-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-clause (&optional beg end arg)
  "Comments clause at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-clause-position)))
          (end (or end (py--end-of-clause-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-block-or-clause (&optional beg end arg)
  "Comments block-or-clause at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-block-or-clause-position)))
          (end (or end (py--end-of-block-or-clause-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-def (&optional beg end arg)
  "Comments def at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-def-position)))
          (end (or end (py--end-of-def-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-class (&optional beg end arg)
  "Comments class at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-class-position)))
          (end (or end (py--end-of-class-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-def-or-class (&optional beg end arg)
  "Comments def-or-class at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-def-or-class-position)))
          (end (or end (py--end-of-def-or-class-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-statement (&optional beg end arg)
  "Comments statement at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-statement-position)))
          (end (or end (py--end-of-statement-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(provide 'python-components-comment)
;;; python-components-comment ends here
