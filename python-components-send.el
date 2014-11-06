;;; ppython-components-send.el --- functions sending strings to Python

;; Maintainer: Andreas Roehler <andreas.roehler@online.de>

;; Keywords: languages, processes

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

(defun py-output-buffer-filter (&optional beg end)
  "Clear output buffer from py-shell-input prompt etc. "
  (interactive "*")
  (let ((beg (cond (beg)
                   ((region-active-p)
                    (region-beginning))
                   (t (point-min))))
        (end (cond (end (copy-marker end))
                   ((region-active-p)
                    (copy-marker (region-end)))
                   (t (copy-marker (point-max))))))
    (goto-char beg)
    (while (re-search-forward (concat "\\(" py-shell-input-prompt-1-regexp "\\|" py-shell-input-prompt-2-regexp "\\|" "^In \\[[0-9]+\\]: *" "\\)") nil (quote move) 1)
      (replace-match ""))
    (goto-char beg)))

(defun py-output-filter (string)
  "Clear output buffer from py-shell-input prompt etc. "
  (interactive "*")
  (let (erg)
    (while
	(not (equal erg (setq erg (replace-regexp-in-string
				   (concat "\\(\n\\|" py-shell-input-prompt-1-regexp "\\|"
					   py-shell-input-prompt-2-regexp "\\|" "^In \\[[0-9]+\\]: *" "\\)") "" string))))
      (sit-for 0.1 t))
    erg))

(defun py-send-string (string &optional process)
  "Evaluate STRING in Python process."
  (interactive "sPython command: ")
  (let* ((proc (or process (get-buffer-process (py-shell))))
	 (buffer (process-buffer proc)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (comint-send-string proc string)
      (goto-char (point-max))
      (unless (string-match "\n\\'" string)
	;; Make sure the text is properly LF-terminated.
	(comint-send-string proc "\n"))
      (when py-debug-p (message "%s" (current-buffer)))
      (goto-char (point-max)))))

(provide 'python-components-send)
;;; python-components-send.el ends here
