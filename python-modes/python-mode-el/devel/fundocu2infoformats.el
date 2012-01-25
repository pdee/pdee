(defun python-components-mode-list-actives (&optional directory)
  "Return a list, which components will be loaded. "
  (interactive)
  (let ((directory (or directory default-directory))
        componentslist)
    (find-file (concat directory "python-components-mode.el"))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*(require '\\([^)]+\\))" nil (quote move) 1)
        (when (save-match-data
                (string-match "python-components" (match-string-no-properties 0)))
          (add-to-list 'componentslist (prin1-to-string (match-string-no-properties 1)))))
      (dolist (ele componentslist)
        (find-file (concat directory ele)
                   (finds (concat ele ".el")))))))

(defun finds-from-programm ()
  (let ((directory-in default-directory)
        (directory-out (concat default-directory "doc"))
        (erg (concat default-directory "python-mode.el"))
        (buffer (current-buffer)))
    (message "Lade %s" erg)
    (find-file erg)
    (finds buffer directory-in directory-out)))

(defun finds (&optional buffer directory-in directory-out)
  "List all commands in file alongside with their documentation. "
  (interactive)
  (let* ((oldbuf (buffer-name (or buffer (current-buffer))))
         (file (buffer-file-name))
         (orgname (concat (substring oldbuf 0 (string-match "\\." oldbuf)) ".org"))
         (reSTname (concat (substring oldbuf 0 (string-match "\\." oldbuf)) ".rst"))
         (directory-in (or directory-in default-directory))
         (directory-out-prepare
          (if directory-out
              (if
                  (string-match ".+/$" directory-out)
                  directory-out
                (concat directory-out "/"))
            default-directory))
         (directory-out (unless (and directory-out (string-match ".*/doc" directory-out))  (concat directory-out-prepare "doc/"))))
    (finds-base oldbuf file orgname reSTname directory-in directory-out)))

(defun finds-base (oldbuf file orgname reSTname directory-in directory-out)
  (save-restriction
    (let (commandslist)
      (widen)
      (goto-char (point-min))
      (eval-buffer)
      (while (re-search-forward "^[ \t]*(defun +\\(\\w+\\|\\s_+\\)+" nil t)
        (goto-char (match-beginning 1))
        (when (commandp (symbol-at-point))
          (lexical-let* ((name (symbol-at-point))
                         (docu (documentation name)))
            (unless docu (message "don't see docu string for %s" (prin1-to-string name)))
            (add-to-list 'commandslist (cons (prin1-to-string name) docu))
            ;; (message "%s" (car commandslist))
            (end-of-defun))))
      (setq commandslist (nreverse commandslist))
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (insert (concat ";; a list of " oldbuf " commands
\(setq " oldbuf "-commands (quote "))
        (insert (prin1-to-string commandslist))
        (insert "))")
        (eval-buffer)
        (write-file (concat directory-out "commands-" (concat (substring oldbuf 0 (string-match "\\." oldbuf)) ".el"))))
      (with-temp-buffer
        ;; org
        (insert (concat (capitalize (substring oldbuf 0 (string-match "\." oldbuf))) " commands" "\n\n"))
        (dolist (ele commandslist)
          (insert (concat "* "(car ele) "\n"))
          (insert (concat "   " (cdr ele) "\n")))
        (write-file (concat directory-out "commands-" orgname))
        (find-file (concat directory-out "commands-" orgname)))
      (with-temp-buffer
        ;; reST
        (insert (concat (capitalize (substring oldbuf 0 (string-match "\." oldbuf))) " commands" "\n"))
        (insert (concat (make-string (length (concat (substring oldbuf 0 (string-match "\." oldbuf)) " commands")) ?\=) "\n\n"))
        (dolist (ele commandslist)
          (insert (concat (car ele) "\n"))
          (insert (concat (make-string (length (car ele)) ?\-) "\n"))
          (insert (concat (cdr ele) "\n\n")))
        (write-file (concat directory-out "commands-" reSTname))
        (find-file (concat directory-out "commands-" reSTname))))))
;; (dolist (ele commandslist)))
