(ert-deftest py-ert-execute-block-fast-2 ()
  (py-test-point-min
      "try:
    a
except NameError:
    a=1
finally:
    a+=1
    print(a)"
    'python-mode
    'py-debug-p 
    (let ((py-fast-process-p t)
	  (py-return-result-p t)
	  (py-debug-p t)
	  py-result)
      (py-execute-block)
      (when py-debug-p (message "py-ert-execute-block-fast, py-result: %s" py-result))
      (should (and py-result (numberp (string-to-number (car (split-string py-result)))))))))

(ert-deftest py-ert-moves-up-execute-statement-python3-dedicated-test ()
  (py-test-with-temp-buffer-point-min
      "print(\"I'm the py-execute-statement-python3-dedicated-test\")"
    (let ((py-debug-p t)
	  py-store-result-p
	  erg)
      (call-interactively 'py-execute-statement-python3-dedicated)
      ;; (sit-for 0.1 t)
      (set-buffer py-buffer-name)
      (goto-char (point-min))
      (should (search-forward "py-execute-statement-python3-dedicated-test" nil t 1)))))

(ert-deftest py-ert-execute-block-fast-3 ()
  (py-test-with-temp-buffer-point-min
      "if True:
    a = 1
    print(a)"
    (let ((py-fast-process-p t)
	  (py-return-result-p t)
	  (py-debug-p t)
	  py-result)
      (py-execute-block)
      (when py-debug-p (message "py-ert-execute-block-fast, py-result: %s" py-result))
      (sit-for 0.1 t)
      (should (string= "1" py-result)))))

(ert-deftest py-ert-exception-name-face-lp-1294742 ()
  (py-test-with-temp-buffer
      "ArithmeticError"
    (forward-char -1)
    (font-lock-fontify-region (point-min) (point-max))
    (should (eq 'py-exception-name-face (get-char-property (point) 'face)))))

(ert-deftest py-ert-execute-block-jython-test ()
  (let ((buffer (py--choose-buffer-name "jython")))
    (py-test-with-temp-buffer
        "if True:
    print(\"one\")
    print(\"two\")"
      (py-execute-block-jython)
      (sit-for 0.1)
      (set-buffer buffer)
      (goto-char (point-max))
      (or
       (should (search-backward "two"))
       (should (search-forward "two"))))))

(ert-deftest py-shell-complete-in-dedicated-shell ()
  ;; (py-test-with-temp-buffer
  (with-current-buffer (python-dedicated)
    (goto-char (point-max))
    ;; (when py-debug-p (switch-to-buffer (current-buffer)))
    ;; (switch-to-buffer (current-buffer))
    (insert "pri")
    (sit-for 1 t)
    (call-interactively 'py-indent-or-complete)
    (sit-for 0.1 t)
    (should (or (looking-back "print.?" (line-beginning-position))))))

(ert-deftest py-ert-execute-statement-fast-1 ()
  (py-test-point-min
      "print(1)"
    'python-mode
    'py-debug-p 
    (let ((py-fast-process-p t)
	  (py-return-result-p t)
	  py-result py-store-result-p)
      (py-execute-statement)
      (should (string= "1" py-result)))))

(ert-deftest py-ert-execute-statement-fast-2 ()
  (py-test-point-min
      "print(2)"
    'python-mode
    'py-debug-p 
    (let ((py-fast-process-p t)
	  (py-return-result-p t)
	  py-result py-store-result-p)
      (py-execute-statement-fast)
      (should (string= "2" py-result)))))
