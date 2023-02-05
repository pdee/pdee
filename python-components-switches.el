;;; python-components-switches.el --- Toggle minor modes -*- lexical-binding: t; -*-


;; URL: https://gitlab.com/python-mode-devs

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

;;  Smart indentation
(defun py-toggle-smart-indentation (&optional arg)
  "Toggle ‘py-smart-indentation’ - on with positiv ARG.

Returns value of ‘py-smart-indentation’ switched to."
  (interactive)
  (let ((arg (or arg (if py-smart-indentation -1 1))))
    (if (< 0 arg)
        (progn
          (setq py-smart-indentation t)
          (py-guess-indent-offset))
      (setq py-smart-indentation nil)
      (setq py-indent-offset (default-value 'py-indent-offset)))
    (when (called-interactively-p 'any) (message "py-smart-indentation: %s" py-smart-indentation))
    py-smart-indentation))

(defun py-smart-indentation-on (&optional arg)
  "Toggle‘py-smart-indentation’ - on with positive ARG.

Returns value of ‘py-smart-indentation’."
  (interactive "p")
  (let ((arg (or arg 1)))
    (py-toggle-smart-indentation arg))
  (when (called-interactively-p 'any) (message "py-smart-indentation: %s" py-smart-indentation))
  py-smart-indentation)

(defun py-smart-indentation-off (&optional arg)
  "Toggle ‘py-smart-indentation’ according to ARG.

Returns value of ‘py-smart-indentation’."
  (interactive "p")
  (let ((arg (if arg (- arg) -1)))
    (py-toggle-smart-indentation arg))
  (when (called-interactively-p 'any) (message "py-smart-indentation: %s" py-smart-indentation))
  py-smart-indentation)

(defun py-toggle-sexp-function ()
  "Opens customization."
  (interactive)
  (customize-variable 'py-sexp-function))

;; Autopair mode
;; py-autopair-mode forms
(declare-function autopair-mode "autopair" ())
(defun py-toggle-autopair-mode ()
  "If ‘py-autopair-mode’ should be on or off.

  Returns value of ‘py-autopair-mode’ switched to."
  (interactive)
  (and (py-autopair-check)
       (declare-function autopair-mode "autopair-mode" ())
       (setq py-autopair-mode (autopair-mode (if autopair-mode 0 1)))))

(defun py-autopair-mode-on ()
  "Make sure, py-autopair-mode' is on.

Returns value of ‘py-autopair-mode’."
  (interactive)
  (and (py-autopair-check)
       (setq py-autopair-mode (autopair-mode 1))))

(defun py-autopair-mode-off ()
  "Make sure, py-autopair-mode' is off.

Returns value of ‘py-autopair-mode’."
  (interactive)
  (setq py-autopair-mode (autopair-mode -1)))

;;  py-switch-buffers-on-execute-p forms
(defun py-toggle-switch-buffers-on-execute-p (&optional arg)
  "Toggle ‘py-switch-buffers-on-execute-p’ according to ARG.

  Returns value of ‘py-switch-buffers-on-execute-p’ switched to."
  (interactive)
  (let ((arg (or arg (if py-switch-buffers-on-execute-p -1 1))))
    (if (< 0 arg)
        (setq py-switch-buffers-on-execute-p t)
      (setq py-switch-buffers-on-execute-p nil))
    (when (or py-verbose-p (called-interactively-p 'any)) (message "py-switch-buffers-on-execute-p: %s" py-switch-buffers-on-execute-p))
    py-switch-buffers-on-execute-p))

(defun py-switch-buffers-on-execute-p-on (&optional arg)
  "Toggle ‘py-switch-buffers-on-execute-p’ according to ARG.

Returns value of ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((arg (or arg 1)))
    (py-toggle-switch-buffers-on-execute-p arg))
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-switch-buffers-on-execute-p: %s" py-switch-buffers-on-execute-p))
  py-switch-buffers-on-execute-p)

(defun py-switch-buffers-on-execute-p-off ()
  "Make sure, ‘py-switch-buffers-on-execute-p’ is off.

Returns value of ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (py-toggle-switch-buffers-on-execute-p -1)
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-switch-buffers-on-execute-p: %s" py-switch-buffers-on-execute-p))
  py-switch-buffers-on-execute-p)

;;  py-split-window-on-execute forms
(defun py-toggle-split-window-on-execute (&optional arg)
  "Toggle ‘py-split-window-on-execute’ according to ARG.

  Returns value of ‘py-split-window-on-execute’ switched to."
  (interactive)
  (let ((arg (or arg (if py-split-window-on-execute -1 1))))
    (if (< 0 arg)
        (setq py-split-window-on-execute t)
      (setq py-split-window-on-execute nil))
    (when (or py-verbose-p (called-interactively-p 'any)) (message "py-split-window-on-execute: %s" py-split-window-on-execute))
    py-split-window-on-execute))

(defun py-split-window-on-execute-on (&optional arg)
  "Toggle ‘py-split-window-on-execute’ according to ARG.

Returns value of ‘py-split-window-on-execute’."
  (interactive)
  (let ((arg (or arg 1)))
    (py-toggle-split-window-on-execute arg))
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  py-split-window-on-execute)

(defun py-split-window-on-execute-off ()
  "Make sure, ‘py-split-window-on-execute’ is off.

Returns value of ‘py-split-window-on-execute’."
  (interactive)
  (py-toggle-split-window-on-execute -1)
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  py-split-window-on-execute)

;;  py-fontify-shell-buffer-p forms
(defun py-toggle-fontify-shell-buffer-p (&optional arg)
  "Toggle ‘py-fontify-shell-buffer-p’ according to ARG.

  Returns value of ‘py-fontify-shell-buffer-p’ switched to."
  (interactive)
  (let ((arg (or arg (if py-fontify-shell-buffer-p -1 1))))
    (if (< 0 arg)
        (progn
          (setq py-fontify-shell-buffer-p t)
          (set (make-local-variable 'font-lock-defaults)
             '(python-font-lock-keywords nil nil nil nil
                                         (font-lock-syntactic-keywords
                                          . py-font-lock-syntactic-keywords)))
          (unless (looking-at comint-prompt-regexp)
            (when (re-search-backward comint-prompt-regexp nil t 1)
              (font-lock-fontify-region (line-beginning-position) (point-max)))))
      (setq py-fontify-shell-buffer-p nil))
    (when (or py-verbose-p (called-interactively-p 'any)) (message "py-fontify-shell-buffer-p: %s" py-fontify-shell-buffer-p))
    py-fontify-shell-buffer-p))

(defun py-fontify-shell-buffer-p-on (&optional arg)
  "Toggle ‘py-fontify-shell-buffer-p’ according to ARG.

Returns value of ‘py-fontify-shell-buffer-p’."
  (interactive)
  (let ((arg (or arg 1)))
    (py-toggle-fontify-shell-buffer-p arg))
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-fontify-shell-buffer-p: %s" py-fontify-shell-buffer-p))
  py-fontify-shell-buffer-p)

(defun py-fontify-shell-buffer-p-off ()
  "Make sure, ‘py-fontify-shell-buffer-p’ is off.

Returns value of ‘py-fontify-shell-buffer-p’."
  (interactive)
  (py-toggle-fontify-shell-buffer-p -1)
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-fontify-shell-buffer-p: %s" py-fontify-shell-buffer-p))
  py-fontify-shell-buffer-p)

;;  python-mode-v5-behavior-p forms
(defun py-toggle-python-mode-v5-behavior-p (&optional arg)
  "Toggle `python-mode-v5-behavior-p' according to ARG.

  Returns value of `python-mode-v5-behavior-p' switched to."
  (interactive)
  (let ((arg (or arg (if python-mode-v5-behavior-p -1 1))))
    (if (< 0 arg)
        (setq python-mode-v5-behavior-p t)
      (setq python-mode-v5-behavior-p nil))
    (when (or py-verbose-p (called-interactively-p 'any)) (message "python-mode-v5-behavior-p: %s" python-mode-v5-behavior-p))
    python-mode-v5-behavior-p))

(defun py-python-mode-v5-behavior-p-on (&optional arg)
  "To `python-mode-v5-behavior-p' according to ARG.

Returns value of `python-mode-v5-behavior-p'."
  (interactive)
  (let ((arg (or arg 1)))
    (py-toggle-python-mode-v5-behavior-p arg))
  (when (or py-verbose-p (called-interactively-p 'any)) (message "python-mode-v5-behavior-p: %s" python-mode-v5-behavior-p))
  python-mode-v5-behavior-p)

(defun py-python-mode-v5-behavior-p-off ()
  "Make sure, `python-mode-v5-behavior-p' is off.

Returns value of `python-mode-v5-behavior-p'."
  (interactive)
  (py-toggle-python-mode-v5-behavior-p -1)
  (when (or py-verbose-p (called-interactively-p 'any)) (message "python-mode-v5-behavior-p: %s" python-mode-v5-behavior-p))
  python-mode-v5-behavior-p)

;;  py-jump-on-exception forms
(defun py-toggle-jump-on-exception (&optional arg)
  "Toggle ‘py-jump-on-exception’ according to ARG.

  Returns value of ‘py-jump-on-exception’ switched to."
  (interactive)
  (let ((arg (or arg (if py-jump-on-exception -1 1))))
    (if (< 0 arg)
        (setq py-jump-on-exception t)
      (setq py-jump-on-exception nil))
    (when (or py-verbose-p (called-interactively-p 'any)) (message "py-jump-on-exception: %s" py-jump-on-exception))
    py-jump-on-exception))

(defun py-jump-on-exception-on (&optional arg)
  "Toggle py-jump-on-exception' according to ARG.

Returns value of ‘py-jump-on-exception’."
  (interactive)
  (let ((arg (or arg 1)))
    (py-toggle-jump-on-exception arg))
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-jump-on-exception: %s" py-jump-on-exception))
  py-jump-on-exception)

(defun py-jump-on-exception-off ()
  "Make sure, ‘py-jump-on-exception’ is off.

Returns value of ‘py-jump-on-exception’."
  (interactive)
  (py-toggle-jump-on-exception -1)
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-jump-on-exception: %s" py-jump-on-exception))
  py-jump-on-exception)

;;  py-use-current-dir-when-execute-p forms
(defun py-toggle-use-current-dir-when-execute-p (&optional arg)
  "Toggle ‘py-use-current-dir-when-execute-p’ according to ARG.

  Returns value of ‘py-use-current-dir-when-execute-p’ switched to."
  (interactive)
  (let ((arg (or arg (if py-use-current-dir-when-execute-p -1 1))))
    (if (< 0 arg)
        (setq py-use-current-dir-when-execute-p t)
      (setq py-use-current-dir-when-execute-p nil))
    (when (or py-verbose-p (called-interactively-p 'any)) (message "py-use-current-dir-when-execute-p: %s" py-use-current-dir-when-execute-p))
    py-use-current-dir-when-execute-p))

(defun py-use-current-dir-when-execute-p-on (&optional arg)
  "Toggle py-use-current-dir-when-execute-p' according to ARG.

Returns value of ‘py-use-current-dir-when-execute-p’."
  (interactive)
  (let ((arg (or arg 1)))
    (py-toggle-use-current-dir-when-execute-p arg))
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-use-current-dir-when-execute-p: %s" py-use-current-dir-when-execute-p))
  py-use-current-dir-when-execute-p)

(defun py-use-current-dir-when-execute-p-off ()
  "Make sure, ‘py-use-current-dir-when-execute-p’ is off.

Returns value of ‘py-use-current-dir-when-execute-p’."
  (interactive)
  (py-toggle-use-current-dir-when-execute-p -1)
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-use-current-dir-when-execute-p: %s" py-use-current-dir-when-execute-p))
  py-use-current-dir-when-execute-p)

;;  py-electric-comment-p forms
(defun py-toggle-electric-comment-p (&optional arg)
  "Toggle ‘py-electric-comment-p’ according to ARG.

  Returns value of ‘py-electric-comment-p’ switched to."
  (interactive)
  (let ((arg (or arg (if py-electric-comment-p -1 1))))
    (if (< 0 arg)
        (setq py-electric-comment-p t)
      (setq py-electric-comment-p nil))
    (when (or py-verbose-p (called-interactively-p 'any)) (message "py-electric-comment-p: %s" py-electric-comment-p))
    py-electric-comment-p))

(defun py-electric-comment-p-on (&optional arg)
  "Toggle py-electric-comment-p' according to ARG.

Returns value of ‘py-electric-comment-p’."
  (interactive)
  (let ((arg (or arg 1)))
    (py-toggle-electric-comment-p arg))
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-electric-comment-p: %s" py-electric-comment-p))
  py-electric-comment-p)

(defun py-electric-comment-p-off ()
  "Make sure, ‘py-electric-comment-p’ is off.

Returns value of ‘py-electric-comment-p’."
  (interactive)
  (py-toggle-electric-comment-p -1)
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-electric-comment-p: %s" py-electric-comment-p))
  py-electric-comment-p)

;;  py-underscore-word-syntax-p forms
(defun py-toggle-underscore-word-syntax-p (&optional arg)
  "Toggle ‘py-underscore-word-syntax-p’ according to ARG.

  Returns value of ‘py-underscore-word-syntax-p’ switched to."
  (interactive)
  (let ((arg (or arg (if py-underscore-word-syntax-p -1 1))))
    (if (< 0 arg)
        (progn
          (setq py-underscore-word-syntax-p t)
          (modify-syntax-entry ?\_ "w" python-mode-syntax-table))
      (setq py-underscore-word-syntax-p nil)
      (modify-syntax-entry ?\_ "_" python-mode-syntax-table))
    (when (or py-verbose-p (called-interactively-p 'any)) (message "py-underscore-word-syntax-p: %s" py-underscore-word-syntax-p))
    py-underscore-word-syntax-p))

(defun py-underscore-word-syntax-p-on (&optional arg)
  "Toggle py-underscore-word-syntax-p' according to ARG.

Returns value of ‘py-underscore-word-syntax-p’."
  (interactive)
  (let ((arg (or arg 1)))
    (py-toggle-underscore-word-syntax-p arg))
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-underscore-word-syntax-p: %s" py-underscore-word-syntax-p))
  py-underscore-word-syntax-p)

(defun py-underscore-word-syntax-p-off ()
  "Make sure, ‘py-underscore-word-syntax-p’ is off.

Returns value of ‘py-underscore-word-syntax-p’."
  (interactive)
  (py-toggle-underscore-word-syntax-p -1)
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-underscore-word-syntax-p: %s" py-underscore-word-syntax-p))
  py-underscore-word-syntax-p)

;; py-toggle-underscore-word-syntax-p must be known already
;; circular: py-toggle-underscore-word-syntax-p sets and calls it
(defcustom py-underscore-word-syntax-p t
  "If underscore chars should be of ‘syntax-class’ word.

I.e. not of ‘symbol’.

Underscores in word-class like ‘forward-word’ travel the indentifiers.
Default is t.

See bug report at launchpad, lp:940812"
  :type 'boolean
  :tag "py-underscore-word-syntax-p"
  :group 'python-mode
  :set (lambda (symbol value)
         (set-default symbol value)
         (py-toggle-underscore-word-syntax-p (if value 1 0))))

(provide 'python-components-switches)
;;;  python-components-switches.el ends here
