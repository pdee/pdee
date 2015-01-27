;;; thingatpt-highlight.el --- more th-at-point edit functions

;; Copyright (C) 2010-2014 Andreas Roehler, unless
;; indicated otherwise

;; Author: Andreas Roehler <andreas.roehler@easy-emacs.de>, unless
;; indicated otherwise

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

;;; Code:

;; Basicely inspired by `highlight-current-line.el'. Please
;; visit
;; http://www.emacswiki.org/emacs-en/HighlightCurrentLine
;; to get the credits.

(defgroup highlight-thing nil
  "Highlight thing where the cursor is in."
  :load 'highlight-thing
  :group 'faces)

(defface highlight-thing-face
  '((t (:background "grey50")))
  "Face used to highlight current line."
  :group 'highlight-thing)

(defvar highlight-thing-minor-mode nil
  "Non-nil if using highlight-thing mode as a minor mode.")
(make-variable-buffer-local 'highlight-thing-minor-mode)

(defvar highlight-thing-overlay
  (make-overlay 1 1)
  "Overlay for highlighting.")

(overlay-put highlight-thing-overlay
	     'face 'highlight-thing-face)

(defvar highlight-thing-beg-function nil)

(defvar highlight-thing-end-function nil)
;;  (setq highlight-thing-beg-function 'ar-word-beginning-position-atpt)
;;  (setq highlight-thing-end-function 'ar-word-end-position-atpt)

(defun ar-th-highlight (thing &optional arg iact)
  (setq highlight-thing-beg-function (intern-soft (concat "ar-" (format "%s" thing) "-beginning-position-atpt")))
  (setq highlight-thing-end-function (intern-soft (concat "ar-" (format "%s" thing) "-end-position-atpt")))
  (highlight-thing-minor-mode))

(defun highlight-thing-hook ()
  (let ((beg (funcall highlight-thing-beg-function))
        (end (funcall highlight-thing-end-function)))
    (highlight-thing-hook-intern beg end nil)))

(defun highlight-thing-hook-intern (beg end &optional arg)
  "Post-Command-Hook for highlighting."
  (condition-case ()
      (if highlight-thing-minor-mode
          (let ((arg (or arg 0)))
            (move-overlay highlight-thing-overlay
                          beg end (current-buffer))))
    (error nil)))

(defconst highlight-thing-no-color (if (boundp 'xemacs-logo)
				       '[]
				     nil)
  "'color' value that represents \"no color\".")

;;  Compatibility code - Set highlight-foregroundcolor.

(defun highlight-thing-set-fg-color (color)
  "Set foregroundcolor for highlighting cursor-word to COLOR.
Key: \\[highlight-thing-set-fg-color]"
  (interactive "sForeground color (\"none\" means no color): ")
  (if (equal "none" color)
      (setq color highlight-thing-no-color))
  (set-face-foreground 'highlight-thing-face color))

;;  Compatibility code - Set highlight-backgroundcolor.

(defun highlight-thing-set-bg-color (color)
  "Set backgroundcolor for highlighting cursor-word to COLOR.
Key: \\[highlight-thing-set-bg-color]"
  (interactive "sBackground color (\"none\" means no color): ")
  (if (equal "none" color)
      (setq color highlight-thing-no-color))
  (set-face-background 'highlight-thing-face color))

;;  Enable/Disable Highlighting

(defun highlight-thing (&optional on-off local)
  "Switch highlighting of cursor-word ON-OFF
If LOCAL is non-nil, do so locally for the current buffer only."
  (cond
   (on-off
    (when
        (featurep 'xemacs)
      (make-local-hook 'post-command-hook))
    (add-hook 'post-command-hook 'highlight-thing-hook nil local))
   (t
    (remove-hook 'post-command-hook 'highlight-thing-hook t)
    (delete-overlay highlight-thing-overlay))))

(defun highlight-thing-minor-mode (&optional arg)
  "Toggle highlight-thing minor mode.
With ARG, turn minor mode on if ARG is positive, off otherwise.
You can customize the face of the highlighted line and whether the entire
line is hightlighted by customizing the group highlight-thing."
  (setq highlight-thing-minor-mode
        (if (null arg)
            (not highlight-thing-minor-mode)
          (> (prefix-numeric-value arg) 0)))
  (if highlight-thing-minor-mode
      (highlight-thing t t)
    (highlight-thing nil t)))

(provide 'thingatpt-highlight)
;;;  thingatpt-highlight.el ends here
