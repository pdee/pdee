(ert-deftest py-ert-fill-paragraph-lp-1291493 ()
  (py-test-with-temp-buffer-point-min
      "if True:
    if True:
        if True:
            if True:
                pass

def foo():
    \"\"\"Foo\"\"\"
"
    (font-lock-fontify-buffer)
    (sit-for 0.1 t)
    (when py-debug-p (switch-to-buffer (current-buffer)))
    (search-forward "\"\"\"")
    (fill-paragraph)
    (sit-for 0.1 t)
    (should (eq 7 (current-column)))))

(ert-deftest py-ert-imports-in-interactive-shell-lp-1290709 ()
  ""
  (ignore-errors (kill-buffer-unconditional (get-buffer "*Python*")))
  (ignore-errors (kill-buffer-unconditional (get-buffer "*Python3*")))
  (let ((buffer (py-shell nil nil "python")))
    (set-buffer buffer)
    (delete-other-windows)
    (let ((full-height (window-height)))
      (py-send-string "import os" (get-buffer-process (current-buffer)))
      (sit-for 0.1)
      (insert "print(os.get")
      (call-interactively 'py-shell-complete)
      (sit-for 0.1 t)
      (should (< (window-height) full-height)))))

(ert-deftest py-ert-execute-region-lp-1294796 ()
  (py-test-with-temp-buffer-point-min
      "print(1)
"
    (let ((py-shell-name "ipython")
	  py-split-windows-on-execute-p
	  py-switch-buffers-on-execute-p)
      (py-execute-buffer)
      (set-buffer "*IPython*")
      (goto-char (point-max))
      (when py-debug-p (switch-to-buffer (current-buffer)))
      (sit-for 0.1 t)
      (should (search-backward "1")))))

(ert-deftest py-ert-wrong-python-test ()
  "Python3 incompatible code should return error."
  (py-test-with-temp-buffer
      "print 123"
    (let ((py-shell-name "python3"))
      (py-execute-statement)
      (message "%s" (prin1-to-string py-error))
      (should py-error))))

(ert-deftest py-ipython-shell-test ()
  ""
  (let ((erg (ipython)))
    (sit-for 1)
    (should (bufferp (get-buffer erg)))
    (should (get-buffer-process erg))))

(ert-deftest py-ert-execute-expression-test ()
  (py-test-with-temp-buffer-point-min
      "print(\"I'm the py-execute-expression-test\")"
    (let ((py-shell-name "python"))
      (when py-debug-p (switch-to-buffer (current-buffer))) 
      (py-execute-expression)
      (sit-for 0.1 t) 
      (set-buffer ert-test-default-buffer)
      ;; (switch-to-buffer (current-buffer))
      (sit-for 0.1 t)
      (and (should
	    (or
	     (search-backward "py-execute-expression-test" nil t 1)
	     (search-forward "py-execute-expression-test" nil t 1)))
	   (py-kill-buffer-unconditional (current-buffer))))))
