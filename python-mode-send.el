;;; python-mode-execute.el --- functions sending strings to Python

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

(defun py-output-buffer-filter (&optional beg end)
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
                         
(defun py-send-string (string)
  "Evaluate STRING in inferior Python process."
  (interactive "sPython command: ")
  (comint-send-string (python-proc) string)
  (unless (string-match "\n\\'" string)
    ;; Make sure the text is properly LF-terminated.
    (comint-send-string (python-proc) "\n"))
  (when (string-match "\n[ \t].*\n?\\'" string)
    ;; If the string contains a final indented line, add a second newline so
    ;; as to make sure we terminate the multiline instruction.
    (comint-send-string (python-proc) "\n")))

(provide 'python-mode-send)
;;; python-mode-execute.el ends here
