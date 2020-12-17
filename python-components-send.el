1;;; ppython-components-send.el --- functions sending strings to Python -*- lexical-binding: t; -*-


;; URL: https://gitlab.com/python-mode-devs

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


(provide 'python-components-send)
;;; python-components-send.el ends here
