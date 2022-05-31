;;; python-components-extra.el --- completion and path update  -*- lexical-binding: t; -*-

;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary: Stuff merged/adapted from python.el

;;

;;; Code:

(defun py-util-comint-last-prompt ()
  "Return comint last prompt overlay start and end.
This is for compatibility with Emacs < 24.4."
  (cond ((bound-and-true-p comint-last-prompt-overlay)
         (cons (overlay-start comint-last-prompt-overlay)
               (overlay-end comint-last-prompt-overlay)))
        ((bound-and-true-p comint-last-prompt)
         comint-last-prompt)
        (t nil)))

(defun py-shell-accept-process-output (process &optional timeout regexp)
  "Accept PROCESS output with TIMEOUT until REGEXP is found.
Optional argument TIMEOUT is the timeout argument to
`accept-process-output' calls.  Optional argument REGEXP
overrides the regexp to match the end of output, defaults to
`comint-prompt-regexp'.  Returns non-nil when output was
properly captured.

This utility is useful in situations where the output may be
received in chunks, since `accept-process-output' gives no
guarantees they will be grabbed in a single call.  An example use
case for this would be the CPython shell start-up, where the
banner and the initial prompt are received separately."
  (let ((regexp (or regexp comint-prompt-regexp)))
    (catch 'found
      (while t
        (when (not (accept-process-output process timeout))
          (throw 'found nil))
        (when (looking-back
               regexp (car (py-util-comint-last-prompt)))
          (throw 'found t))))))

(defun py-shell-completion-get-completions (process import input)
  "Do completion at point using PROCESS for IMPORT or INPUT.
When IMPORT is non-nil takes precedence over INPUT for
completion."
  (setq input (or import input))
  (with-current-buffer (process-buffer process)
    (let ((completions
           (ignore-errors
	     (py--string-trim
	      (py-send-string-no-output
	       (format
		(concat py-completion-setup-code
			"\nprint (" py-shell-completion-string-code ")")
		input) process (buffer-name (current-buffer)))))))
      (when (> (length completions) 2)
        (split-string completions
                      "^'\\|^\"\\|;\\|'$\\|\"$" t)))))

(defun py-shell-completion-at-point (&optional process)
  "Function for `completion-at-point-functions' in `py-shell-mode'.
Optional argument PROCESS forces completions to be retrieved
using that one instead of current buffer's process."
  ;; (setq process (or process (get-buffer-process (current-buffer))))
  (let*
      ((process (or process (get-buffer-process (current-buffer))))
       (line-start (if (derived-mode-p 'py-shell-mode)
		       ;; Working on a shell buffer: use prompt end.
		       (or (cdr (py-util-comint-last-prompt))
			   (line-beginning-position))
		     (line-beginning-position)))
       (import-statement
	(when (string-match-p
	       (rx (* space) word-start (or "from" "import") word-end space)
	       (buffer-substring-no-properties line-start (point)))
	  (buffer-substring-no-properties line-start (point))))
       (start
	(save-excursion
	  (if (not (re-search-backward
		    ;; (py-rx
		    ;;  (or whitespace open-paren close-paren string-delimiter simple-operator))
		    "[[:space:]]\\|[([{]\\|[])}]\\|\\(?:[^\"'\\]\\|\\=\\|\\(?:[^\\]\\|\\=\\)\\\\\\(?:\\\\\\\\\\)*[\"']\\)\\(?:\\\\\\\\\\)*\\(\\(?:\"\"\"\\|'''\\|[\"']\\)\\)\\|[%&*+/<->^|~-]"
		    line-start
		    t 1))
	      line-start
	    (forward-char (length (match-string-no-properties 0)))
	    (point))))
       (end (point))
              (completion-fn
	(with-current-buffer (process-buffer process)
	  #'py-shell-completion-get-completions)))
    (list start end
          (completion-table-dynamic
           (apply-partially
            completion-fn
            process import-statement)))))

(defun py-comint-watch-for-first-prompt-output-filter (output)
  "Run `py-shell-first-prompt-hook' when first prompt is found in OUTPUT."
  (when (not py-shell--first-prompt-received)
    (set (make-local-variable 'py-shell--first-prompt-received-output-buffer)
         (concat py-shell--first-prompt-received-output-buffer
                 (ansi-color-filter-apply output)))
    (when (py-shell-comint-end-of-output-p
           py-shell--first-prompt-received-output-buffer)
      (if (string-match-p
           (concat py-shell-prompt-pdb-regexp (rx eos))
           (or py-shell--first-prompt-received-output-buffer ""))
          ;; Skip pdb prompts and reset the buffer.
          (setq py-shell--first-prompt-received-output-buffer nil)
        (set (make-local-variable 'py-shell--first-prompt-received) t)
        (setq py-shell--first-prompt-received-output-buffer nil)
        (with-current-buffer (current-buffer)
          (let ((inhibit-quit nil))
            (run-hooks 'py-shell-first-prompt-hook))))))
  output)

(defun py-shell-font-lock-get-or-create-buffer ()
  "Get or create a font-lock buffer for current inferior process."
  (with-current-buffer (current-buffer)
    (if py-shell--font-lock-buffer
        py-shell--font-lock-buffer
      (let ((process-name
             (process-name (get-buffer-process (current-buffer)))))
        (generate-new-buffer
         (format " *%s-font-lock*" process-name))))))

(defun py-font-lock-kill-buffer ()
  "Kill the font-lock buffer safely."
  (when (and py-shell--font-lock-buffer
             (buffer-live-p py-shell--font-lock-buffer))
    (kill-buffer py-shell--font-lock-buffer)
    (when (derived-mode-p 'py-shell-mode)
      (setq py-shell--font-lock-buffer nil))))

(defmacro py-shell-font-lock-with-font-lock-buffer (&rest body)
  "Execute the forms in BODY in the font-lock buffer.
The value returned is the value of the last form in BODY.  See
also `with-current-buffer'."
  (declare (indent 0) (debug t))
  `(save-current-buffer
     (when (not (and py-shell--font-lock-buffer
		     (get-buffer py-shell--font-lock-buffer)))
       (setq py-shell--font-lock-buffer
	     (py-shell-font-lock-get-or-create-buffer)))
     (set-buffer py-shell--font-lock-buffer)
     (when (not font-lock-mode)
       (font-lock-mode 1))
     (set (make-local-variable 'delay-mode-hooks) t)
     (let (py-smart-indentation)
       (when (not (derived-mode-p 'python-mode))
	 (python-mode))
       ,@body)))

(defun py-shell-font-lock-cleanup-buffer ()
  "Cleanup the font-lock buffer.
Provided as a command because this might be handy if something
goes wrong and syntax highlighting in the shell gets messed up."
  (interactive)
  (with-current-buffer (current-buffer)
    (py-shell-font-lock-with-font-lock-buffer
      (erase-buffer))))

(defun py-shell-font-lock-comint-output-filter-function (output)
  "Clean up the font-lock buffer after any OUTPUT."
  (if (and (not (string= "" output))
           ;; Is end of output and is not just a prompt.
           (not (member
                 (py-shell-comint-end-of-output-p
                  (ansi-color-filter-apply output))
                 '(nil 0))))
      ;; If output is other than an input prompt then "real" output has
      ;; been received and the font-lock buffer must be cleaned up.
      (py-shell-font-lock-cleanup-buffer)
    ;; Otherwise just add a newline.
    (py-shell-font-lock-with-font-lock-buffer
      (goto-char (point-max))
      (newline 1)))
  output)

(defun py-font-lock-post-command-hook ()
  "Fontifies current line in shell buffer."
  (let ((prompt-end
	 (or (cdr (py-util-comint-last-prompt))
	     (progn (sit-for 0.1)
		    (cdr (py-util-comint-last-prompt))))))
    (when (and prompt-end (> (point) prompt-end)
               (process-live-p (get-buffer-process (current-buffer))))
      (let* ((input (buffer-substring-no-properties
                     prompt-end (point-max)))
             (deactivate-mark nil)
             (start-pos prompt-end)
             (buffer-undo-list t)
             (font-lock-buffer-pos nil)
             (replacement
              (py-shell-font-lock-with-font-lock-buffer
                (delete-region (line-beginning-position)
                               (point-max))
                (setq font-lock-buffer-pos (point))
                (insert input)
                ;; Ensure buffer is fontified, keeping it
                ;; compatible with Emacs < 24.4.
		(when py-shell-fontify-p
		    (if (fboundp 'font-lock-ensure)
			(funcall 'font-lock-ensure)
		      (font-lock-default-fontify-buffer)))
                (buffer-substring font-lock-buffer-pos
                                  (point-max))))
             (replacement-length (length replacement))
             (i 0))
        ;; Inject text properties to get input fontified.
        (while (not (= i replacement-length))
          (let* ((plist (text-properties-at i replacement))
                 (next-change (or (next-property-change i replacement)
                                  replacement-length))
                 (plist (let ((face (plist-get plist 'face)))
                          (if (not face)
                              plist
                            ;; Replace FACE text properties with
                            ;; FONT-LOCK-FACE so input is fontified.
                            (plist-put plist 'face nil)
                            (plist-put plist 'font-lock-face face)))))
            (set-text-properties
             (+ start-pos i) (+ start-pos next-change) plist)
            (setq i next-change)))))))

(defun py-shell-font-lock-turn-on (&optional msg)
  "Turn on shell font-lock.
With argument MSG show activation message."
  (interactive "p")
  (save-current-buffer
    (py-font-lock-kill-buffer)
    (set (make-local-variable 'py-shell--font-lock-buffer) nil)
    (add-hook 'post-command-hook
	      #'py-font-lock-post-command-hook nil 'local)
    (add-hook 'kill-buffer-hook
              #'py-font-lock-kill-buffer nil 'local)
    (add-hook 'comint-output-filter-functions
              #'py-shell-font-lock-comint-output-filter-function
              'append 'local)
    (when msg
      (message "Shell font-lock is enabled"))))

(defun py-shell-font-lock-turn-off (&optional msg)
  "Turn off shell font-lock.
With argument MSG show deactivation message."
  (interactive "p")
  (with-current-buffer (current-buffer)
    (py-font-lock-kill-buffer)
    (when (py-util-comint-last-prompt)
      ;; Cleanup current fontification
      (remove-text-properties
       (cdr (py-util-comint-last-prompt))
       (line-end-position)
       '(face nil font-lock-face nil)))
    (set (make-local-variable 'py-shell--font-lock-buffer) nil)
    (remove-hook 'post-command-hook
                 #'py-font-lock-post-command-hook 'local)
    (remove-hook 'kill-buffer-hook
                 #'py-font-lock-kill-buffer 'local)
    (remove-hook 'comint-output-filter-functions
                 #'py-shell-font-lock-comint-output-filter-function
                 'local)
    (when msg
      (message "Shell font-lock is disabled"))))

(defun py-shell-font-lock-toggle (&optional msg)
  "Toggle font-lock for shell.
With argument MSG show activation/deactivation message."
  (interactive "p")
  (with-current-buffer (current-buffer)
    (set (make-local-variable 'py-shell-fontify-p)
         (not py-shell-fontify-p))
    (if py-shell-fontify-p
        (py-shell-font-lock-turn-on msg)
      (py-shell-font-lock-turn-off msg))
    py-shell-fontify-p))

(defun comint-mime-setup-py-shell ()
  "Enable `comint-mime'.

Setup code specific to `py-shell-mode'."
  (interactive)
  ;; (if (not py-shell--first-prompt-received)
  ;; (add-hook 'py-shell-first-prompt-hook #'comint-mime-setup-py-shell nil t)
  (setq py-python-command "ipython3"
	py-ipython-command "ipython3"
	py-ipython-command-args '("--pylab" "--matplotlib=inline" "--automagic" "--simple-prompt")
	py-python-command-args '("--pylab" "--matplotlib=inline" "--automagic" "--simple-prompt"))
  (py-send-string-no-output
   (format "%s\n__COMINT_MIME_setup('''%s''')"
           (with-temp-buffer
	     (switch-to-buffer (current-buffer)) 
	     (insert-file-contents
	      (expand-file-name "comint-mime.py"
                                comint-mime-setup-script-dir))
	     (buffer-string))
           (if (listp comint-mime-enabled-types)
	       (string-join comint-mime-enabled-types ";")
	     comint-mime-enabled-types))))
;; )

(when (featurep 'comint-mime)
  (add-hook 'py-shell-mode-hook 'comint-mime-setup-py-shell)
  (push '(py-shell-mode . comint-mime-setup-py-shell)
	comint-mime-setup-function-alist)
  ;; (setq py-python-command "ipython3"
  ;; 	py-ipython-command "ipython3"
  ;; 	py-python-command-args '("--pylab" "--matplotlib=inline" "--automagic" "--simple-prompt")
  ;; 	;; "-i" doesn't work with `isympy3'
  ;; 	py-ipython-command-args '("--pylab" "--matplotlib=inline" "--automagic" "--simple-prompt"))
  )

(provide 'python-components-extra)
;;; python-components-extra.el ends here
