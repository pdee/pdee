;;; python-components-pdb.el --- pdb help functions -*- lexical-binding: t; -*-

;; Author: https://gitlab.com/groups/python-mode-devs

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
;; pdbtrack support contributed by Ken Manheimer, April 2001.
;;

;;; Code:

(defun py-execute-statement-pdb ()
  "Execute statement running pdb."
  (interactive)
  (let ((py-python-command-args "-i -m pdb"))
    (py-execute-statement)))

(defun py-execute-region-pdb (beg end)
  "Takes region between BEG END."
  (interactive "r")
  (let ((py-python-command-args "-i -m pdb")))
    (py-execute-region beg end))

(defun py-pdb-execute-statement ()
  "Execute statement running pdb."
  (interactive)
  (let ((stm (progn (py-statement) (car kill-ring))))
    (py-execute-string (concat "import pdb;pdb.run('" stm "')"))))

(defun py-pdb-help ()
  "Print generic pdb.help() message."
  (interactive)
  (py-execute-string "import pdb;pdb.help()"))

;; https://stackoverflow.com/questions/6980749/simpler-way-to-put-pdb-breakpoints-in-python-code
;; breakpoint at line 3

;; python -m pdb -c "b 3" -c c your_script.py

(defun py-pdb-break-at-current-line ()
  "Set breakpoint at current line.

Optional LINE FILE CONDITION"
  (interactive)
  (py-execute-string (concat "import pdb;pdb.break('" (py-count-lines)  "')")))

(defun py--pdb-versioned ()
  "Guess existing pdb version from ‘py-shell-name’.

Return \"pdb[VERSION]\" if executable found, just \"pdb\" otherwise"
  (interactive)
  (let ((erg (when (string-match "[23]" py-shell-name)
	       ;; versions-part
	       (substring py-shell-name (string-match "[23]" py-shell-name)))))
    (if erg
      (cond ((executable-find (concat "pdb" erg))
	     (concat "pdb" erg))
	    ((and (string-match "\\." erg)
		  (executable-find (concat "pdb" (substring erg 0 (string-match "\\." erg)))))
	     (concat "pdb" (substring erg 0 (string-match "\\." erg)))))
      "pdb")))

(defun py-pdb (command-line)
  "Run pdb on program FILE in buffer `*gud-FILE*'.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

At GNU Linux required pdb version should be detected by `py--pdb-version'
at Windows configure `py-python-ms-pdb-command'

lp:963253
Argument COMMAND-LINE TBD."
  (interactive
   (progn
     (require 'gud)
     (list (gud-query-cmdline
	    (if (or (eq system-type 'ms-dos)(eq system-type 'windows-nt))
		(car (read-from-string py-python-ms-pdb-command))
	      ;; sys.version_info[0]
	      ;; (car (read-from-string (py--pdb-version)))
	      'pdb)
	    (py--buffer-filename-remote-maybe)))))
  (pdb command-line))

(defun py--pdb-current-executable ()
  "When ‘py-pdb-executable’ is set, return it.

Otherwise return resuslt from `executable-find'"
  (or py-pdb-executable
      (executable-find "pdb")))

(defun py-update-gud-pdb-history ()
  "If pdb is called at a Python buffer, put it's file name at the head of `gud-pdb-history'."
  (interactive)
  (let* (;; PATH/TO/pdb
	 (first (cond ((and gud-pdb-history (ignore-errors (car gud-pdb-history)))
		       (replace-regexp-in-string "^\\([^ ]+\\) +.+$" "\\1" (car gud-pdb-history)))
		      (py-pdb-executable
		       py-pdb-executable)
		      ((or (eq system-type 'ms-dos)(eq system-type 'windows-nt))
		       ;; lp:963253
		       "c:/python27/python\ -i\ c:/python27/Lib/pdb.py")
		      (t
		       (py--pdb-current-executable))))
	 ;; file to debug
         (second (cond ((not (ignore-errors
			       (py--buffer-filename-remote-maybe)))
			(error "%s" "Buffer must be saved first."))
		       ((py--buffer-filename-remote-maybe))
		       (t (and gud-pdb-history (stringp (car gud-pdb-history)) (replace-regexp-in-string "^\\([^ ]+\\) +\\(.+\\)$" "\\2" (car gud-pdb-history))))))
         (erg (and first second (concat first " " second))))
    (when erg
      (push erg gud-pdb-history))))

(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline py-pdb-path
                            ;; (file-name-nondirectory buffer-file-name)
			    (file-name-nondirectory (py--buffer-filename-remote-maybe)) ))))

;; tbreak [ ([filename:]lineno | function) [, condition] ]
;;         Same arguments as break, but sets a temporary breakpoint: it
;;         is automatically deleted when first hit.

;; python -m pdb -c "b 3" -c c your_script.py

(defun py-pdb-tbreak ()
  "Insert a temporary break."
  (interactive)
  (let (
	(py-python-command-args '("-i -c \"b 30\" -c c \"eyp.py\""))
	(py-python3-command-args '("-i -c \"b 30\" -c c \"eyp.py\""))
	)
    (py-execute-buffer)))


(provide 'python-components-pdb)
;;; python-components-pdb.el ends here
