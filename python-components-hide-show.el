;;; python-components-hide-show.el --- Provide hs-minor-mode forms

;; Copyright (C) 2011-2014  Andreas Roehler
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

;; (setq hs-block-start-regexp 'py-extended-block-or-clause-re)
;; (setq hs-forward-sexp-func 'py-end-of-block)

(defun py-hide-base (form &optional beg end)
  "Hide visibility of existing form at point. "
  (hs-minor-mode 1)
  (save-excursion
    (let* ((form (prin1-to-string form))
	   (beg (or beg (or (funcall (intern-soft (concat "py--beginning-of-" form "-p")))
			    (funcall (intern-soft (concat "py-beginning-of-" form))))))
	   (end (or end (funcall (intern-soft (concat "py-end-of-" form)))))
	   (modified (buffer-modified-p))
	   (inhibit-read-only t))
      (if (and beg end)
	  (progn
	    (hs-make-overlay beg end 'code)
	    (set-buffer-modified-p modified))
	(error (concat "No " (format "%s" form) " at point!"))))))

(defun py-show-base (form &optional beg end)
  "Remove invisibility of existing form at point. "
  (save-excursion
    (let* ((form (prin1-to-string form))
	   (beg (or beg (or (funcall (intern-soft (concat "py--beginning-of-" form "-p")))
			    (funcall (intern-soft (concat "py-beginning-of-" form))))))
	   (end (or end (funcall (intern-soft (concat "py-end-of-" form)))))
	   (modified (buffer-modified-p))
	   (inhibit-read-only t))
      (if (and beg end)
	  (progn
	    (hs-discard-overlays beg end)
	    (set-buffer-modified-p modified))
	(error (concat "No " (format "%s" form) " at point!"))))))

(defun py-hide-show (&optional form beg end)
  "Toggle visibility of existing forms at point. "
  (interactive)
  (save-excursion
    (let* ((form (prin1-to-string form))
	   (beg (or beg (or (funcall (intern-soft (concat "py--beginning-of-" form "-p")))
			    (funcall (intern-soft (concat "py-beginning-of-" form))))))
	   (end (or end (funcall (intern-soft (concat "py-end-of-" form)))))
	   (modified (buffer-modified-p))
	   (inhibit-read-only t))
      (if (and beg end)
	  (if (overlays-in beg end)
	      (hs-discard-overlays beg end)
	    (hs-make-overlay beg end 'code))
	(error (concat "No " (format "%s" form) " at point!")))
      (set-buffer-modified-p modified))))

(defun py-hide-region (beg end)
  "Hide active region. "
  (interactive
   (list
    (and (use-region-p) (region-beginning))(and (use-region-p) (region-end))))
  (py-hide-base 'region beg end))

(defun py-show-region (beg end)
  "Un-hide active region. "
  (interactive
   (list
    (and (use-region-p) (region-beginning))(and (use-region-p) (region-end))))
  (py-show-base 'region beg end))

(defun py-hide-statement ()
  "Hide statement at point. "
  (interactive)
  (py-hide-base 'statement))

(defun py-show-statement ()
  "Show statement at point. "
  (interactive)
  (py-show-base 'statement))

(defun py-hide-block ()
  "Hide block at point. "
  (interactive)
  (py-hide-base 'block))

(defun py-show-block ()
  "Show block at point. "
  (interactive)
  (py-show-base 'block))

(defun py-hide-clause ()
  "Hide clause at point. "
  (interactive)
  (py-hide-base 'clause))

(defun py-show-clause ()
  "Show clause at point. "
  (interactive)
  (py-show-base 'clause))

(defun py-hide-block-or-clause ()
  "Hide block-or-clause at point. "
  (interactive)
  (py-hide-base 'block-or-clause))

(defun py-show-block-or-clause ()
  "Show block-or-clause at point. "
  (interactive)
  (py-show-base 'block-or-clause))

(defun py-hide-def ()
  "Hide def at point. "
  (interactive)
  (py-hide-base 'def))

(defun py-show-def ()
  "Show def at point. "
  (interactive)
  (py-show-base 'def))

(defun py-hide-class ()
  "Hide class at point. "
  (interactive)
  (py-hide-base 'class))

(defun py-show-class ()
  "Show class at point. "
  (interactive)
  (py-show-base 'class))

(defun py-hide-expression ()
  "Hide expression at point. "
  (interactive)
  (py-hide-base 'expression))

(defun py-show-expression ()
  "Show expression at point. "
  (interactive)
  (py-show-base 'expression))

(defun py-hide-partial-expression ()
  "Hide partial-expression at point. "
  (interactive)
  (py-hide-base 'partial-expression))

(defun py-show-partial-expression ()
  "Show partial-expression at point. "
  (interactive)
  (py-show-base 'partial-expression))

(defun py-hide-line ()
  "Hide line at point. "
  (interactive)
  (py-hide-base 'line))

(defun py-show-line ()
  "Show line at point. "
  (interactive)
  (py-show-base 'line))

(defun py-hide-top-level ()
  "Hide top-level at point. "
  (interactive)
  (py-hide-base 'top-level))

(defun py-show-top-level ()
  "Show top-level at point. "
  (interactive)
  (py-show-base 'top-level))

(provide 'python-components-hide-show)
;;; python-components-hide-show.el ends here
