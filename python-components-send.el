;;; ppython-components-send.el --- functions sending strings to Python -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>

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
(defun py-output-buffer-filter (&optional beg end)
  "Clear output buffer from py-shell-input prompt etc.

Optional BEG END"
  (interactive "*")
  (let ((beg (cond (beg)
                   ((use-region-p)
                    (region-beginning))
                   (t (point-min))))
        (end (cond (end (copy-marker end))
                   ((use-region-p)
                    (copy-marker (region-end)))
                   (t (copy-marker (point-max))))))
    (goto-char beg)
    (while (re-search-forward (concat "\\(" py-shell-input-prompt-1-regexp "\\|" py-shell-input-prompt-2-regexp "\\|" "^In \\[[0-9]+\\]: *" "\\)") end (quote move) 1)
      (replace-match ""))
    (goto-char beg)))

(defun py-output-filter (strg)
  "Clear STRG from py-shell-input prompt."
  (interactive "*")
  (let (erg)
    (while
	(not (equal erg (setq erg (replace-regexp-in-string
				   (concat "\\(\n\\|" py-shell-input-prompt-1-regexp "\\|"
					   py-shell-input-prompt-2-regexp "\\|" "^In \\[[0-9]+\\]: *" "\\)") "" strg))))
      (sit-for 0.1 t))
    erg))

(defun py-send-string (strg &optional process)
  "Evaluate STRG in Python PROCESS."
  (interactive "sPython command: ")
  (let* ((proc (or process
		   (prog1
		       (get-buffer-process (py-shell))
		     (comint-send-string (get-buffer-process (py-shell)) "\n"))))
	 (buffer (process-buffer proc)))
    ;; (with-current-buffer buffer
    ;; (goto-char (point-max))
    ;; (switch-to-buffer (current-buffer))
    (unless (string-match "\\`" strg)
      (setq strg (concat strg "\n")))
    (process-send-string proc strg)
     ;; (comint-send-string proc strg)
    ;; (goto-char (point-max))
    ;; (unless (string-match "\n\\'" strg)
    ;; Make sure the text is properly LF-terminated.
    ;; (comint-send-string proc "\n"))
    ;; (when py-debug-p (message "%s" (current-buffer)))
    ;; (goto-char (point-max))
    ;; (switch-to-buffer (current-buffer))
    ))

(provide 'python-components-send)
;;; python-components-send.el ends here
