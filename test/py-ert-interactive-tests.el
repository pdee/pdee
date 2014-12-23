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

(ert-deftest fill-paragraph-causes-wrong-indent-lp-1397936-test ()
  (py-test-with-temp-buffer
      "def foo():
    \"\"\"abc\"\"\"
"
    (when py-debug-p (switch-to-buffer (current-buffer))
	  (font-lock-fontify-buffer))
    (goto-char 20)
    (call-interactively 'fill-paragraph)
    (should (eq 4 (current-indentation)))))

(ert-deftest py-ert-imports-in-interactive-shell-lp-1290709 ()
  ""
  (when (buffer-live-p (get-buffer "*Python*")) (kill-buffer-unconditional (get-buffer "*Python*")))
  (when (buffer-live-p (get-buffer "*Python3*")) (kill-buffer-unconditional (get-buffer "*Python3*")))
  (let ((buffer (py-shell nil nil "python")))
    (set-buffer buffer)
    (delete-other-windows)
    (let ((full-height (window-height)))
      (when py-debug-p (switch-to-buffer (current-buffer)))
      (py-send-string "import os" (get-buffer-process (current-buffer)))
      (sit-for 0.1)
      (goto-char (point-max))
      ;; (sit-for 0.1 t)
      (insert "print(os.get")
      (py-indent-or-complete)
      (sit-for 0.1 t)
      (should (< (window-height) full-height)))))

(ert-deftest py-ert-execute-region-lp-1294796 ()
  (py-test-with-temp-buffer-point-min
      "print(1)
"
    (let ((py-shell-name "ipython")
	  py-split-window-on-execute
	  py-switch-buffers-on-execute-p)
      (py-execute-buffer)
      (set-buffer "*IPython*")
      (goto-char (point-max))
      (when py-debug-p (switch-to-buffer (current-buffer)))
      (sit-for 0.1 t)
      (should (search-backward "1")))))

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

(ert-deftest py-ert-execute-line-test ()
  (py-test-with-temp-buffer-point-min
      "print(\"I'm the py-execute-line-test\")"
    (let ((py-shell-name "python"))
      (when py-debug-p (switch-to-buffer (current-buffer))
	    (font-lock-fontify-buffer))
      (sit-for 0.1 t)
      (py-execute-line)
      (sit-for 0.1 t)
      (set-buffer ert-test-default-buffer)
      (sit-for 0.1 t)
      (when py-debug-p (switch-to-buffer (current-buffer)) )
      (and (should
	    (or
	     (search-backward "py-execute-line-test" nil t 1)
	     (search-forward "py-execute-line-test" nil t 1)))
	   (py-kill-buffer-unconditional (current-buffer))))))

(ert-deftest py-ert-execute-statement-python2-test ()
  (py-test-with-temp-buffer-point-min
      "print(\"I'm the py-execute-statement-python2-test\")"
    (when py-debug-p (switch-to-buffer (current-buffer)))
    (py-execute-statement-python2)
    (set-buffer "*Python2*")
    (goto-char (point-max))
    (sit-for 0.2 t)
    (when py-debug-p (switch-to-buffer (current-buffer)))
    (and (should (search-backward "py-execute-statement-python2-test" nil t 1))
	 (py-kill-buffer-unconditional (current-buffer)))))
