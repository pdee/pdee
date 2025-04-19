;;; py-ert-interactive-tests.el --- test interactively -*- lexical-binding: t; -*-

;; URL: https://gitlab.com/python-mode-devs
;; Keywords: languages

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

;;; Commentary: These tests fail in batch-mode

;;

;;; Code:

(require 'py-setup-ert-tests)

(ert-deftest py-ert-fill-paragraph-lp-1291493-JPuJd3 ()
  (py-test-point-min
   "if True:
    if True:
        if True:
            if True:
                pass
def foo():
    \"\"\"Foo\"\"\"
"
   'python-mode
   'py-verbose-p
   (sit-for 0.1 t)
   (search-forward "\"\"\"")
   (fill-paragraph)
   (sit-for 0.1 t)
   (should (eq 7 (current-column)))))

(ert-deftest py-ert-imports-in-interactive-shell-lp-1290709-lZvVlc ()
  ""
  'python-mode
  'py-verbose-p
  (when (buffer-live-p (get-buffer "*Python*"))
    (py-kill-buffer-unconditional (get-buffer "*Python*")))
  (when (buffer-live-p (get-buffer "*Python3*")) (py-kill-buffer-unconditional (get-buffer "*Python3*")))
  (let ((buffer (py-shell nil nil "python")))
    (set-buffer buffer)
    (delete-other-windows)
    (let ((full-height (window-height)))
      (py-execute-string "import os" (get-buffer-process (current-buffer)))
      (sit-for 0.1)
      (goto-char (point-max))
      ;; (sit-for 0.1 t)
      (insert "print(os.get")
      (py-indent-or-complete)
      (sit-for 0.1 t)
      (should (< (window-height) full-height)))))

(ert-deftest py-ert-execute-region-ipython-lp-1294796-HePARg ()
  (py-test-point-min
   "print(1)
"
   'python-mode
   'py-verbose-p
   (let ((py-shell-name "ipython")
	 py-split-window-on-execute
	 py-switch-buffers-on-execute-p)
     (if (executable-find "ipython")
	 (progn
	   (py-execute-buffer)
	   (sit-for 0.5 t)
	   (set-buffer "*IPython*")
	   (goto-char (point-max))
	   (should (search-backward "1")))
       (message "%s" "ipython does not exist on your system.")))))

(ert-deftest py-ert-execute-region-ipython3-lp-1294796-HePARg ()
  (py-test-point-min
   "print(1)
"
   'python-mode
   'py-verbose-p
   (let ((py-shell-name "ipython3")
	 py-split-window-on-execute
	 py-switch-buffers-on-execute-p)
     (if (executable-find "ipython3")
	 (progn
	   (py-execute-buffer)
	   (sit-for 0.5 t)
	   (set-buffer "*IPython3*")
	   (goto-char (point-max))
	   (should (search-backward "1")))
       (message "%s" "ipython3 does not exist on your system.")))))

(ert-deftest py-ert-execute-expression-test-bogDOp ()
  (py-test-point-min
   "print(\"I'm the py-execute-expression-test\")"
   'python-mode
   'py-verbose-p
   (let ((py-shell-name "python"))
     (py-execute-expression)
     (sit-for 0.1 t)
     ;; (switch-to-buffer (current-buffer))
     (sit-for 0.1 t)
     (and (should
	   (or
	    (search-backward "py-execute-expression-test" nil t 1)
	    (search-forward "py-execute-expression-test" nil t 1)))
	  (py-kill-buffer-unconditional (current-buffer))))))

(ert-deftest py-ert-execute-line-test-jU3Xgu ()
  (py-test-point-min
   "print(\"I'm the py-execute-line-test\")"
   'python-mode
   'py-verbose-p
   (py-execute-line)
   (set-buffer py-output-buffer)
   ;; (sit-for 0.1 t)
   (goto-char (point-max))
   (should (search-backward "py-execute-line-test"))))

(ert-deftest py-ert-always-reuse-lp-1361531-test-dJBO5C ()
  (py-test
   "#! /usr/bin/env python3
# -*- coding: utf-8 -*-
print(\"I'm the py-always-reuse-lp-1361531-test\")
from datetime import datetime; datetime.now()"
   'python-mode
   'py-verbose-p
   ;; (delete-other-windows)
   (let* ((py-split-window-on-execute 'always)
	  py-switch-buffers-on-execute-p
	  py-dedicated-process-p
          (oldbuf (current-buffer)))
     (py-execute-statement-python3)
     ;; (save-excursion (py-execute-statement-python))
     ;; (set-buffer oldbuf)
     ;; (when py-debug-p (switch-to-buffer (current-buffer)))
     (py-execute-statement-python)
     ;; (message "(window-list): %s" (window-list))
     ;; (sit-for 0.1 t)
     (should (eq 3 (count-windows)))
     ;; (py-restore-window-configuration)
     )))

(ert-deftest py-ert-just-two-split-dedicated-lp-1361531-ipython-test-zGlzYP ()
  (py-test
   "#! /usr/bin/env ipython
# -*- coding: utf-8 -*-
print(\"I'm the py-just-two-split-dedicated-lp-1361531-ipython-test\")"
   'python-mode
   'py-verbose-p
   (if (executable-find "ipython")
       (progn
	 (delete-other-windows)
	 (let* ((py-split-window-on-execute 'just-two)
		(erg1 (progn (py-execute-statement-ipython-dedicated) py-output-buffer))
		(erg2 (progn (py-execute-statement-ipython-dedicated) py-output-buffer)))
	   ;; (sit-for 0.1 t)
	   (when py-debug-p (message "(count-windows) %s" (count-windows)))
	   (should (eq 2 (count-windows)))
	   (py-kill-buffer-unconditional erg1)
	   (py-kill-buffer-unconditional erg2)
	   (py-restore-window-configuration)))
     (message "%s" "ipython does not exist on your system."))))

(ert-deftest py-ert-just-two-split-dedicated-lp-1361531-ipython3-test-zGlzYP ()
  (py-test
   "#! /usr/bin/env ipython
# -*- coding: utf-8 -*-
print(\"I'm the py-just-two-split-dedicated-lp-1361531-ipython-test\")"
   'python-mode
   'py-verbose-p
   (if (executable-find "ipython3")
       (progn
	 ;; (delete-other-windows)
	 (let* ((py-split-window-on-execute 'just-two)
		(erg1 (progn (py-execute-statement-ipython3-dedicated) py-output-buffer))
		(erg2 (progn (py-execute-statement-ipython3-dedicated) py-output-buffer)))
	   ;; (sit-for 0.1 t)
	   (when py-debug-p (message "(count-windows) %s" (count-windows)))
	   (should (eq 2 (count-windows)))
	   (py-kill-buffer-unconditional erg1)
	   (py-kill-buffer-unconditional erg2)
	   (py-restore-window-configuration)))
     (message "%s" "ipython3 does not exist on your system."))))

(ert-deftest py-ert-just-two-split-dedicated-lp-1361531-jython-test-Nh6zdU ()
  (py-test
   "#! /usr/bin/env jython
# -*- coding: utf-8 -*-
print(\"I'm the py-just-two-split-dedicated-lp-1361531-jython-test\")"
   'python-mode
   'py-verbose-p
   (delete-other-windows)
   (let* ((py-split-window-on-execute 'just-two)
	  (erg1 (progn (py-execute-statement-jython-dedicated) py-output-buffer))
	  (erg2 (progn (py-execute-statement-jython-dedicated) py-output-buffer)))
     ;; (sit-for 0.1 t)
     (when py-debug-p (message "(count-windows) %s" (count-windows)))
     (should (eq 2 (count-windows)))
     (py-kill-buffer-unconditional erg1)
     (py-kill-buffer-unconditional erg2)
     (py-restore-window-configuration))))

;; (ert-deftest py-flycheck-mode-5Yz7A2 ()
;;   (py-test
;;    ""
;;    (py-flycheck-mode -1)
;;    (should-not flycheck-mode)
;;    (py-flycheck-mode 1)
;;    (should flycheck-mode)
;;    (py-flycheck-mode -1)
;;    (should-not flycheck-mode)))

(ert-deftest py-face-lp-1454858-python3-1-test-3lRWI6 ()
  (py-test
   "#! /usr/bin/env python3
file.close()"
   'python-mode
   'py-verbose-p
   (let ((py-python-edit-version ""))
     (goto-char (point-max))
     (beginning-of-line)
     (should-not (face-at-point)))))

(ert-deftest py-face-lp-1454858-python3-2-test-R3JIC9 ()
  (py-test
   "#! /usr/bin/env python3
file.close()"
   'python-mode
   'py-verbose-p
   (let ((py-python-edit-version "python3"))
     (goto-char (point-max))
     (beginning-of-line)
     (should-not (face-at-point)))))

(ert-deftest py-face-lp-1454858-python3-4-test-dHIVmf ()
  (py-test
   "#! /usr/bin/env python3
print()"
   'python-mode
   'py-verbose-p
   (let ((py-python-edit-version ""))
     (goto-char (point-max))
     (search-backward "print")
     (sit-for 0.1)
     (should (eq (face-at-point) 'py-builtins-face)))))

(ert-deftest py-ert-execute-statement-split-rGDJdi ()
  (py-test-point-min
   "print(123)"
   'python-mode
   'py-verbose-p
   (let ((py-split-window-on-execute t))
     (delete-other-windows)
     (py-execute-statement)
     (sit-for 0.1 t)
     (should (not (one-window-p))))))

(ert-deftest py-ert-py-execute-section-test-bsHl0k ()
  (py-test
   "# {{
print(3+3)
# }}"
   'python-mode
   'py-verbose-p
   (let ((py-store-result-p t))
     (search-backward "print")
     (py-execute-section)
     (sleep-for 0.1)
     (should (string= py-result "6")))))

(ert-deftest py-ert-match-paren-test-3-xtdxLn ()
  (py-test
   "if __name__ == \"__main__\":
    main()
"
   'python-mode
   'py-verbose-p
   (skip-chars-backward " \t\r\n\f")
   (back-to-indentation)
   (py-match-paren)
   (should (eq 4 (current-column)))))

(ert-deftest py-ert-match-paren-test-6-p1JAuq ()
  (py-test
   "class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')
    # zeit = time.strftime('%Y-%m-%d--%H-%M-%S')
    spiel = []
    gruen = [0]
    rot = [1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36]
    def pylauf(self):
        \"\"\"Eine Doku fuer pylauf\"\"\"
        ausgabe = [\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]
        ausgabe[0] = treffer
        fertig = ''
#        print \"treffer, schwarz, gruen, rot, pair, impair, passe, manque, spiel\"
        if treffer in gruen:
            # print \"0, Gruen\"
            ausgabe[1] = treffer
            ausgabe[2] = treffer
        elif treffer in schwarz:
            # print \"%i, Schwarz\" % (treffer)
            ausgabe[1] = treffer
if __name__ == \"__main__\":
    main()
"
   'python-mode
   'py-verbose-p
   (search-backward "(treffer)")
   (skip-chars-backward "^\"")
   (forward-char -1)
   (py-match-paren)
   (should (eq (char-after) ?#))
   (py-match-paren)
   (should (eq (char-before) ?\)))
   (should (eolp))))

(ert-deftest py-ert-moves-up-fill-paragraph-pep-257-nn-2-rq3mat ()
  (py-test-point-min
   "class MyClass(object):
    def my_method(self):
        \"\"\"Some long line with more than 70 characters in the docstring. Some more text.\"\"\"
"
   'python-mode
   'py-verbose-p
   (let ((py-docstring-style 'pep-257-nn))
     (goto-char (point-min))
     (search-forward "\"\"\"")
     (fill-paragraph)
     (search-forward "\"\"\"")
     (should (eq 8 (current-indentation))))))

(ert-deftest py-ert-split-window-on-execute-1361535-test-fK4Nqy ()
  (py-test-point-min
   "print(\"%(language)s has %(number)03d quote types.\" %
       {'language': \"Python\", \"number\": 2})"
   'python-mode
   'py-verbose-p
   (let ((oldbuf (current-buffer))
	 (py-split-window-on-execute t)
	 (py-split-window-on-execute-threshold 3))
     (py-shell)
     (set-buffer oldbuf)
     (switch-to-buffer (current-buffer))
     (delete-other-windows)
     (split-window-vertically)
     (dired "~")
     (set-buffer oldbuf)
     (switch-to-buffer (current-buffer))
     (split-window-horizontally)
     (py-execute-statement)
     (should (eq 3 (length (window-list)))))))

(ert-deftest py-backward-toplevel-test-Rfa4ZA ()
  (py-test
   "''' asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf
'''
a, b, c = (1, 2, 3)"
   'python-mode
   'py-verbose-p
   (beginning-of-line)
   (py-backward-top-level)
   (should (bobp))
   ;; (should (eq (point) 1))
   ))

(ert-deftest py-pdbtrack-input-prompt-45-test-xhbEyD ()
  (py-test
   "def exercise():
  import pdb\\; pdb.set_trace()
  x = \"hello\"
  y = \"darkness\"
  print(x)
exercise()"
   'python-mode
   'py-verbose-p
   (py-execute-buffer)
   (set-buffer py-output-buffer)
   (switch-to-buffer (current-buffer))
   (message "prompt-45: %s" (buffer-name (current-buffer)))
   (message "Nach Prompt: %s" (buffer-substring-no-properties (1- (line-beginning-position)) (point)))
   (sit-for 1)
   (should (looking-back py-pdbtrack-input-prompt))))

(ert-deftest py-pdbtrack-input-prompt-45-test-7V1h5F ()
  (py-test
   "def exercise():
  import pdb\\; pdb.set_trace()
  x = \"hello\"
  y = \"darkness\"
  print(x)
exercise()"
   'python-mode
   'py-verbose-p
   (py-execute-buffer)
   (set-buffer py-output-buffer)
   (switch-to-buffer (current-buffer))
   (message "prompt-45: %s" (buffer-name (current-buffer)))
   (message "Nach Prompt: %s" (buffer-substring-no-properties (1- (line-beginning-position)) (point)))
   (sit-for 1)
   (should (looking-back py-pdbtrack-input-prompt (line-beginning-position)))))

(ert-deftest py-pdbtrack-is-tracking-45-test-N1CTvI ()
  (py-test
   "def exercise():
  import pdb\\; pdb.set_trace()
  x = \"hello\"
  y = \"darkness\"
  print(x)
exercise()"
   'python-mode
   'py-verbose-p
   (py-execute-buffer)
   (switch-to-buffer py-output-buffer)
   (should py-pdbtrack-is-tracking-p)))

(ert-deftest py-pdbtrack-is-tracking-45-test-ra9WRA ()
  (py-test
   "def exercise():
  import pdb\\; pdb.set_trace()
  x = \"hello\"
  y = \"darkness\"
  print(x)
exercise()"
   'python-mode
   py-verbose-p))

(ert-deftest py-ert-moves-up-fill-paragraph-lp-1286318 ()
  (py-test-point-min
   "# r1416
def baz():
    \"\"\"Hello there.
    This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.
    \"\"\"
    return 7
# The last line of the docstring is longer than fill-column (set to
# 78 = for me). Put point on the 'T' in 'This' and hit M-q= . Nothing
# happens.
#
# Another example:
#
def baz():
    \"\"\"Hello there.
    This is a multiline
    function definition.
    Don't worry, be happy.
    Be very very happy.
    Very. happy.
    \"\"\"
    return 7
# All of those lines are shorter than fill-column. Put point anywhere
# = in that paragraph and hit M-q. Nothing happens.
#
# In both cases I would expect to end up with:
#
def baz():
    \"\"\"Hello there.
    This is a multiline function definition. Don= 't worry, be happy. Be very
    very happy. Very. happy.
    \"\"\"
    return 7
"
   'python-mode
   'py-verbose-p
   (goto-char 49)
   ;; (sit-for 0.1 t)
   (fill-paragraph)
   (end-of-line)
   (should (<= (current-column) 72))
   (goto-char 409)
   (fill-paragraph)
   (end-of-line)
   (should (<= (current-column) 72))
   (goto-char 731)
   (fill-paragraph)
   (end-of-line)
   (should (<= (current-column) 72))
   (search-forward "\"\"\"")
   (forward-line -1)
   ;; (sit-for 0.1 t)
   (should (not (py-empty-line-p)))))

(ert-deftest py-ert-if-name-main-permission-lp-326620-test-CZefpG ()
  (py-test-point-min
   "#! /usr/bin/env python2
# -*- coding: utf-8 -*-
def py_if_name_main_permission_test():
    if __name__ == \"__main__\" :
        print(\"__name__ == '__main__' run\")
        return True
    else:
        print(\"__name__ == '__main__' supressed\")
        return False
py_if_name_main_permission_test()
"
   'python-mode
   'py-verbose-p
   (goto-char (point-min))
   (let ((py-if-name-main-permission-p t))
     (py-execute-buffer-python2)
     (set-buffer "*Python2*")
     (goto-char (point-max))
     (sit-for 0.1)
     (should (search-backward "run" nil t)))))

(ert-deftest py-in-list-indent-test-LEON2Q ()
  (py-test
   "def foo():
print(rest)"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (search-backward "rest")
   (py-indent-or-complete)
   ;; (switch-to-buffer (current-buffer))
   ;; (message "py-in-list-indent-test-LEON2Q (current-buffer):  %s" (current-buffer))
   ;; (sit-for 1)
   (should (eq 4 (current-indentation)))))

(ert-deftest py-indent-inconsistent-test-Zh2hP0 ()
  (py-test
   "def lcs (first):
    for i in range(len(first)):
        print(first[i])
        print(i)
"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (search-backward "print" nil t 2)
   (py-indent-line)
   (should (eq 8 (current-indentation)))
   (forward-line 1)
   (back-to-indentation)
   (py-indent-line)
   (should (eq 8 (current-indentation)))))

(ert-deftest py-indentation-lp-1375122-test-yx67am ()
  (py-test
   "def foo():
    if True:
pass
"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (forward-line -1)
   (py-indent-or-complete)
   (sit-for 0.1 t)
   (should (eq 8 (current-column)))
   (beginning-of-line)
   (delete-horizontal-space)
   (indent-to 4)
   (py-indent-or-complete)
   (sit-for 0.1 t)
   (should (eq 8 (current-column)))))

(ert-deftest py-ert-just-two-split-dedicated-lp-1361531-python3-test ()
  (py-test
   "#! /usr/bin/env python3
# -*- coding: utf-8 -*-
print(\"I'm the py-just-two-split-dedicated-lp-1361531-python3-test\")"
   'python-mode
   'py-verbose-p
   (delete-other-windows)
   (let* ((py-split-window-on-execute 'just-two)
	  (erg1 (progn (py-execute-statement-python3-dedicated) py-output-buffer))
	  (erg2 (progn (py-execute-statement-python3-dedicated) py-output-buffer)))
     ;; (sit-for 0.1 t)
     (when py-debug-p (message "(count-windows) %s" (count-windows)))
     (should (eq 2 (count-windows)))
     (py-kill-buffer-unconditional erg1)
     (py-kill-buffer-unconditional erg2)
     (py-restore-window-configuration))))

(ert-deftest py-shell-dedicated-buffer-test-t3Sizn ()
  (let ((buffer (py-shell nil nil t)))
  (should (buffer-live-p buffer))))

(ert-deftest py-python3-shell-test-YW7ToN ()
  ""
  'python-mode
  'py-verbose-p
  (let ((erg (python3))
        erg)
    (should (bufferp (get-buffer erg)))
    (should (get-buffer-process erg))))

(ert-deftest py-python2-shell-test-8Ostfe ()
  ""
  'python-mode
  'py-verbose-p
  (let ((erg (python2)))
    (sit-for 0.1)
    (should (bufferp (get-buffer erg)))
    (should (get-buffer-process erg))))

(ert-deftest py-keep-windows-configuration-test-Hh2GD6 ()
  (py-test
   "print('py-keep-windows-configuration-test-string')"
   'python-mode
   'py-verbose-p
   (delete-other-windows)
   (let ((py-keep-windows-configuration t)
         (py-split-window-on-execute t)
         (full-height (window-height)))
     (py-execute-statement)
     (should (eq (window-height) full-height)))))

(ert-deftest py-shell-test-t3Sizn ()
  (py-test
      ""
    'python-mode
    'py-verbose-p
    (let ((buffer (py-shell nil nil t)))
      (sit-for 0.1)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "def")
        (should (looking-back "def" (line-beginning-position)))))))

(ert-deftest py-shell-test-3uMnzx ()
  (py-test
      ""
    'python-mode
    'py-verbose-p
    (with-current-buffer (py-shell nil nil t)
      (goto-char (point-max))
      (insert "def")
      (should (looking-back "def" (line-beginning-position))))))

(ert-deftest py-execute-import-or-reload-test-ZYUvdh ()
  (py-test
   "#! /usr/bin/env python
# -*- coding: utf-8 -*-
import os"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (should (py-execute-import-or-reload))))

(ert-deftest py-master-file-not-honored-lp-794850-test-P6QZmU ()
  (py-test
   "
# -*- coding: utf-8 -*-
# Local Variables:
# py-master-file: \"/tmp/my-master.py\"
# End:
 "
   'python-mode
   'py-verbose-p
   (let ((oldbuf (current-buffer)))
     (save-excursion
       (set-buffer (get-buffer-create "test-master.py"))
       (erase-buffer)
       (insert "#! /usr/bin/env python
 # -*- coding: utf-8 -*-
print(\"Hello, I'm your master!\")
")
       (write-file "/tmp/my-master.py"))
     (set-buffer oldbuf)
     (unwind-protect
         (py-execute-buffer)
       (when (file-readable-p "/tmp/my-master.py") (delete-file "/tmp/my-master.py"))))))

(ert-deftest py-ert-moves-up-fill-paragraph-symmetric-i6vspv ()
  (let ((py-docstring-style 'symmetric))
    (py-test-point-min
     "# r1416
def baz():
    \"\"\"Hello there. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.
    This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.
    \"\"\"
    return 7
"
     'python-mode
     'py-verbose-p
     (goto-char (point-min))
     (font-lock-fontify-region (point-min)(point-max))
     (goto-char 49)
     (fill-paragraph)
     (search-backward "\"\"\"")
     (goto-char (match-end 0))
     (eolp)
     (forward-line 1)
     (end-of-line)
     (should (<= (current-column) 72))
     (search-forward "\"\"\"")
     (forward-line -1)
     (should (not (py-empty-line-p))))))

(ert-deftest py-run-python-test-QDE84k ()
  "Test built-in python.el."
  (let ((python-indend-offset 4))
    (run-python)
    (should (buffer-live-p (get-buffer "*Python*")))))

(ert-deftest py-ert-wrong-indent-inside-string-lp-1574731-test-P19RGY ()
  (py-test
   "def foo():
    print(\"\"\"
Bar
\"\"\")
"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (forward-line -3)
   (should (eq 10 (py-compute-indentation)))))

(ert-deftest py-ert-moves-up-fill-paragraph-django-76Aw4O ()
  (py-test-point-min
   "# r1416
def baz():
    \"\"\"Hello there. This is a multiline function definition. Don't wor ry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don't worry, be happy. Be very very happy. Very. happy.
    This is a multiline function definition. Don't worry, be happy. Be very very happy. Very. happy.
    Line below should not be empty, when ‘py-docstring-style’ is not ‘PEP-257’.
    \"\"\"
    return 7
"
   'python-mode
   'py-verbose-p
   (let ((py-docstring-style 'django))
     (goto-char 49)
     (fill-paragraph)
     (search-backward "\"\"\"")
     (goto-char (match-end 0))
     (should (eolp))
     (forward-line 1)
     (end-of-line)
     (when py-debug-p (message "fill-column: %s" fill-column))
     (should (<= (current-column) 72)))))

(ert-deftest py-ert-moves-up-fill-paragraph-django-w8Rbx5 ()
  (py-test-point-min
   "# r1416
def baz():
    \"\"\"Hello there. This is a multiline function definition. Don't wor ry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don't worry, be happy. Be very very happy. Very. happy.
    This is a multiline function definition. Don't worry, be happy. Be very very happy. Very. happy.
    Line below should not be empty, when ‘py-docstring-style’ is not ‘PEP-257’.
    \"\"\"
    return 7
"
   'python-mode
   'py-verbose-p
   (let ((py-docstring-style 'django))
     (goto-char 49)
     (when py-debug-p (message "fill-column: %s" fill-column))
     (fill-paragraph)
     (py-end-of-string)
     (forward-line -1)
     (should-not (py-empty-line-p)))))

(ert-deftest py-up-string-test-DVKVO2 ()
  (py-test
   "class M:
    def __init__(self):
        \"\"\"Helper function implementing the current module loader policy.1
        In Python 3.14, the end state is to require and use the module's
        __spec__.loader and ignore any __loader__ attribute on the
        module.
        * If you have a __loader__ and a __spec__.loader but they are not the
        same, in Python 3.12 we issue a DeprecationWarning and fall back to
        __loader__ for backward compatibility.  In Python 3.14, we'll flip
        this case to ignoring __loader__ entirely, without error.
        \"\"\"
        self.a = 1
        self.b = 2"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (search-backward "error")
   ;; (and py-debug-p (message "py-version: %s" py-version))
   ;; (font-lock-ensure)
   ;; (sit-for 0.1)
   (ignore-errors (call-interactively (py-up)))
   (should (eq (char-after) 34))
   (should (eq (char-before) 32))))

(ert-deftest py-backspace-test-86TyUY ()
  (py-test
   "a = b = c = 5     "
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (py-electric-backspace-mode -1)
   (execute-kbd-macro (kbd "<backspace>"))
   (should (eq (point) 18))))

(ert-deftest py-backspace-test-xyFFow ()
  (py-test
   "a = b = c = 5     "
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (py-electric-backspace-mode 1)
   (execute-kbd-macro (kbd "<backspace>"))
   (should (eq (point) 14))))

(ert-deftest py-up-string-test-NJ7sie ()
  (py-test
   "class M:
    def __init__(self):
        \"\"\"Helper function implementing the current module loader policy.1
        In Python 3.14, the end state is to require and use the module's
        __spec__.loader and ignore any __loader__ attribute on the
        module.
        * If you have a __loader__ and a __spec__.loader but they are not the
        same, in Python 3.12 we issue a DeprecationWarning and fall back to
        __loader__ for backward compatibility.  In Python 3.14, we'll flip
        this case to ignoring __loader__ entirely, without error.
        \"\"\"
        self.a = 1
        self.b = 2"
   'python-mode
   'py-verbose-p
   ;; (font-lock-ensure)
   (goto-char (point-max))
   (search-backward "\"\"\"")
   (py-up)
   (should (eq (char-after) 34))
   (should (eq (char-before) 32))))

(ert-deftest py--pdb-versioned-test-QoHSpJ ()
  (py-test
      ""
    'python-mode
    'py-verbose-p
    (require 'gud)
    (let ((py-shell-name "python3"))
      (goto-char (point-max))
      (should (string= "pdb3" (py--pdb-versioned))))))

;; (defun py-up-string-test-NJ7sie ()
;;   ""
;;   (interactive)
;;   (py-test
;;       "class M:
;;     def __init__(self):
;;         \"\"\"Helper function implementing the current module loader policy.1
;;         In Python 3.14, the end state is to require and use the module's
;;         __spec__.loader and ignore any __loader__ attribute on the
;;         module.
;;         * If you have a __loader__ and a __spec__.loader but they are not the
;;         same, in Python 3.12 we issue a DeprecationWarning and fall back to
;;         __loader__ for backward compatibility.  In Python 3.14, we'll flip
;;         this case to ignoring __loader__ entirely, without error.
;;         \"\"\"
;;         self.a = 1
;;         self.b = 2"
;;     'python-mode
;;     py-verbose-p
;;     ;; (font-lock-ensure)
;;     (goto-char (point-max))
;;     (search-backward "\"\"\"")
;;     (py-up)
;;     (should (eq (char-after) 34))
;;     (should (eq (char-before) 32))))

(provide 'py-ert-interactive-tests)
;;; py-ert-interactive-tests.el ends here
