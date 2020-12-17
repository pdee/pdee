;;; python-components-hide-show.el --- Provide hs-minor-mode forms -*- lexical-binding: t; -*-


;; URL: https://gitlab.com/python-mode-devs
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

;; This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

;;; Code:


;; (setq hs-block-start-regexp 'py-extended-block-or-clause-re)
;; (setq hs-forward-sexp-func 'py-forward-block)

(defun py-hide-base (form &optional beg end)
  "Hide visibility of existing form at point."
  (hs-minor-mode 1)
  (save-excursion
    (let* ((form (prin1-to-string form))
           (beg (or beg (or (funcall (intern-soft (concat "py--beginning-of-" form "-p")))
                            (funcall (intern-soft (concat "py-backward-" form))))))
           (end (or end (funcall (intern-soft (concat "py-forward-" form)))))
           (modified (buffer-modified-p))
           (inhibit-read-only t))
      (if (and beg end)
          (progn
            (hs-make-overlay beg end 'code)
            (set-buffer-modified-p modified))
        (error (concat "No " (format "%s" form) " at point"))))))

(defun py-hide-show (&optional form beg end)
  "Toggle visibility of existing forms at point."
  (interactive)
  (save-excursion
    (let* ((form (prin1-to-string form))
           (beg (or beg (or (funcall (intern-soft (concat "py--beginning-of-" form "-p")))
                            (funcall (intern-soft (concat "py-backward-" form))))))
           (end (or end (funcall (intern-soft (concat "py-forward-" form)))))
           (modified (buffer-modified-p))
           (inhibit-read-only t))
      (if (and beg end)
          (if (overlays-in beg end)
              (hs-discard-overlays beg end)
            (hs-make-overlay beg end 'code))
        (error (concat "No " (format "%s" form) " at point")))
      (set-buffer-modified-p modified))))

(defun py-show ()
  "Remove invisibility of existing form at point."
  (interactive)
  (with-silent-modifications
    (save-excursion
      (back-to-indentation)
      (let ((end (next-overlay-change (point))))
	(hs-discard-overlays (point) end)))))

(defun py-show-all ()
  "Remove invisibility of hidden forms in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (end)
      (while (and (not (eobp))  (setq end (next-overlay-change (point))))
	(hs-discard-overlays (point) end)
	(goto-char end)))))

(defun py-hide-region (beg end)
  "Hide active region."
  (interactive
   (list
    (and (use-region-p) (region-beginning))(and (use-region-p) (region-end))))
  (py-hide-base 'region beg end))

(defun py-show-region (beg end)
  "Un-hide active region."
  (interactive
   (list
    (and (use-region-p) (region-beginning))(and (use-region-p) (region-end))))
  (py-show-base 'region beg end))

(defun py-hide-block ()
  "Hide block at point."
  (interactive)
  (py-hide-base 'block))

(defun py-hide-block-or-clause ()
  "Hide block-or-clause at point."
  (interactive)
  (py-hide-base 'block-or-clause))

(defun py-hide-class ()
  "Hide class at point."
  (interactive)
  (py-hide-base 'class))

(defun py-hide-clause ()
  "Hide clause at point."
  (interactive)
  (py-hide-base 'clause))

(defun py-hide-comment ()
  "Hide comment at point."
  (interactive)
  (py-hide-base 'comment))

(defun py-hide-def ()
  "Hide def at point."
  (interactive)
  (py-hide-base 'def))

(defun py-hide-def-or-class ()
  "Hide def-or-class at point."
  (interactive)
  (py-hide-base 'def-or-class))

(defun py-hide-elif-block ()
  "Hide elif-block at point."
  (interactive)
  (py-hide-base 'elif-block))

(defun py-hide-else-block ()
  "Hide else-block at point."
  (interactive)
  (py-hide-base 'else-block))

(defun py-hide-except-block ()
  "Hide except-block at point."
  (interactive)
  (py-hide-base 'except-block))

(defun py-hide-expression ()
  "Hide expression at point."
  (interactive)
  (py-hide-base 'expression))

(defun py-hide-for-block ()
  "Hide for-block at point."
  (interactive)
  (py-hide-base 'for-block))

(defun py-hide-if-block ()
  "Hide if-block at point."
  (interactive)
  (py-hide-base 'if-block))

(defun py-hide-indent ()
  "Hide indent at point."
  (interactive)
  (py-hide-base 'indent))

(defun py-hide-line ()
  "Hide line at point."
  (interactive)
  (py-hide-base 'line))

(defun py-hide-minor-block ()
  "Hide minor-block at point."
  (interactive)
  (py-hide-base 'minor-block))

(defun py-hide-paragraph ()
  "Hide paragraph at point."
  (interactive)
  (py-hide-base 'paragraph))

(defun py-hide-partial-expression ()
  "Hide partial-expression at point."
  (interactive)
  (py-hide-base 'partial-expression))

(defun py-hide-section ()
  "Hide section at point."
  (interactive)
  (py-hide-base 'section))

(defun py-hide-statement ()
  "Hide statement at point."
  (interactive)
  (py-hide-base 'statement))

(defun py-hide-top-level ()
  "Hide top-level at point."
  (interactive)
  (py-hide-base 'top-level))

(defun py-dynamically-hide-indent ()
  (interactive)
  (py-show)
  (py-hide-indent))

(defun py-dynamically-hide-further-indent (&optional arg) 
  (interactive "P")
  (if (eq 4  (prefix-numeric-value arg))
      (py-show)
  (py-show)
  (py-forward-indent)
  (py-hide-indent)))

;; python-components-hide-show.el ends here
(provide 'python-components-hide-show)
