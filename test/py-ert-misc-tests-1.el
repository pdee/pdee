;; py-ert-misc-tests-1.el --- testing python-mode.el -*- lexical-binding: t; -*-

;; Keywords: languages

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'setup-ert-tests)

;;;

;; (ert-deftest py-ert-if-name-main-permission-lp-326620-test-CZefpG ()
;;   (py-test-with-temp-buffer-point-min
;;    "#! /usr/bin/env python2
;; # -*- coding: utf-8 -*-
;; def py_if_name_main_permission_test():
;;     if __name__ == \"__main__\" :
;;         print(\"__name__ == '__main__' run\")
;;         return True

;;     else:
;;         print(\"__name__ == '__main__' supressed\")
;;         return False

;; py_if_name_main_permission_test()
;; "
;;    (goto-char (point-min))
;;    (let ((py-if-name-main-permission-p t))
;;      (py-execute-buffer-python2)
;;      (set-buffer "*Python2*")
;;      (goto-char (point-max))
;;      (sit-for 0.1)
;;      (should (search-backward "run" nil t)))))

(ert-deftest py-ert-borks-all-lp-1294820-sIKMyz ()
  (py-test-with-temp-buffer-point-min
      "# M-q within some code (not in= a docstring) completely borks all previous
# code in the file:
#
# E.g. here, if I M-q within the last function:

def foo(self):
    some_actual_code()

def bar(self):
    some_actual_code()

def baz(self):
    some_actual_code()

# def foo(self): some_actual_code() def bar(self): some_actual_code() def
# baz(self):
#     some_actual_code()
"
    (goto-char (point-min))
    (font-lock-fontify-region (point-min)(point-max))
    (search-forward "def baz(self):")
    (fill-paragraph)
    (forward-line -1)
    (should (eq (char-after) ?\n))))

(ert-deftest py-ert-deletes-too-much-lp:1300270-dMegYd ()
  (py-test-with-temp-buffer "
x = {'abc':'def',
         'ghi':'jkl'}
"
    (when py-debug-p (switch-to-buffer (current-buffer)))
    (goto-char 24)
    (py-electric-delete)
    (should (eq 5 (current-indentation)))))


(ert-deftest py-python2-shell-test-8Ostfe ()
  ""
  (if (not (executable-find "python2"))
      (message "py-python2-shell-test-8Ostfe: %s" "No python executable found!")

    (let ((erg (python2)))
      (should (bufferp (get-buffer erg)))
      (should (get-buffer-process erg)))))

(ert-deftest py-python3-shell-test-YW7ToN ()
  ""
  (let ((erg (python3)))
    (should (bufferp (get-buffer erg)))
    (should (get-buffer-process erg))))

(ert-deftest py-keep-windows-configuration-test-Hh2GD6 ()
  (py-test-with-temp-buffer
      "print('py-keep-windows-configuration-test-string')"
    (delete-other-windows)
    (let ((py-keep-windows-configuration t)
          (py-split-window-on-execute t)
          (full-height (window-height)))
      (py-execute-statement)
      (should (eq (window-height) full-height)))))



(ert-deftest py-ert-in-comment-p-test-G6FUaB ()
  (py-test-with-temp-buffer
      "# "
    (should (py--in-comment-p))))

(ert-deftest py-ert-in-sq-string-p-test-nwha1D ()
  (py-test-with-temp-buffer
      "' "
    (goto-char(point-max))
    (should (py-in-string-p))))

(ert-deftest py-ert-in-dq-string-p-test-lYrt9b ()
  (py-test-with-temp-buffer
      "\" "
    (goto-char(point-max))
    (should (py-in-string-p))))

(ert-deftest py-ert-in-sq-tqs-string-p-test-EwMSzz ()
  (py-test-with-temp-buffer
      "''' "
    (goto-char(point-max))
    (should (py-in-string-p))))

(ert-deftest py-ert-in-dq-tqs-string-p-test-jkHHQH ()
  (py-test-with-temp-buffer
      "\"\"\" "
    (goto-char(point-max))
    (should (py-in-string-p))))

(ert-deftest py-ert-electric-delete-test-HecKiw ()
  (py-test-with-temp-buffer-point-min
      "  {}"
    (goto-char (point-min))
    (py-electric-delete)
    (should (eq (char-after) ?{))))

(ert-deftest py-ert-fill-plain-string-test-OEykwr ()
  (py-test-with-temp-buffer-point-min
      "'''asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdfasdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf
'''"
    (goto-char (point-min))
    (forward-char 4)
    (fill-paragraph)
    (forward-line 1)
    (should (not (py-empty-line-p)))))

(ert-deftest py-ert-nil-docstring-style-lp-1477422-test-6wpqLB ()
  (py-test-with-temp-buffer-point-min
      "def foo():
    '''asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdfasdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf'''"
    (goto-char (point-min))
    (let (py-docstring-style)
      (search-forward "'''")
      (save-excursion
        (fill-paragraph))
      (forward-line 1)
      (should (not (py-empty-line-p))))))

(ert-deftest py-markup-region-as-section-test-KetMYL ()
  (py-test-with-temp-buffer-point-min
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
"
    (goto-char (point-min))
    (search-forward "fertig")
    (py-sectionize-region (match-beginning 0) (line-end-position))
    (py-mark-section)
    (should (eq 371 (region-beginning)))
    (should (eq 408 (region-end)))))

(ert-deftest py-test-embedded-51-test-sgaO9V ()
  (py-test-with-temp-buffer
      "from PrintEngine import *

GeomSim."
    (goto-char(point-max))
    ;; (switch-to-buffer (current-buffer))
    (ignore-errors (py-indent-or-complete))
    (sit-for 0.1)
    (should (eq (char-before) ?.))))

(ert-deftest py--pdb-versioned-test-Ft0557-OoVDWm ()
  (let ((py-shell-name "python3"))
    (py-test-with-temp-buffer
	""
      (goto-char (point-max))
      (should (string= "pdb3" (py--pdb-versioned))))))

(ert-deftest py--pdb-versioned-test-QoHSpJ-oNuvXf ()
  (let ((py-shell-name "python"))
    (py-test-with-temp-buffer
	""
      (goto-char (point-max))
      (should (string= "pdb" (py--pdb-versioned))))))

(ert-deftest py-multline-arguments-with-literal-lists-79-test-7NWa5T ()
  (py-test-with-temp-buffer
      ;; Improper indentation for multline arguments with liiteral lists (#79)
      "def foo():
    bar = dosomething([
                       x <- point"
    (goto-char (point-max))
    (search-backward "x")
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest lines-after-return-80-Ahdpe8 ()
  (py-test-with-temp-buffer
      "def empty():
    return
    yield"
    (goto-char (point-max))
    (beginning-of-line)
    (should (eq 4 (py-compute-indentation)))
    (search-backward "return")
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-ert-copy-indent-test-UbzMto ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        sdfasde
        pass"
    (goto-char (point-min) )
    (search-forward "sdfasde")
    (py-copy-indent)
    (should (string-match "sdfasde" (car kill-ring)))
    (should (not (py--beginning-of-indent-p)))
    (py-backward-statement)
    (should (py--beginning-of-indent-p))))

(ert-deftest py-ert-delete-indent-test-HhZNOr ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        sdfasde
        pass"
    (goto-char (point-min) )
    (search-forward "sdfasde")
    (py-delete-indent)
    (should (eobp))
    (should (bolp))))

(ert-deftest py-ert-kill-indent-test-ECwA5u ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        sdfasde
        pass"
    (goto-char (point-min) )
    (search-forward "sdfasde")
    (py-kill-indent)
    (should (string= (concat (make-string 8 ?\ ) "sdfasde\n" (make-string 8 ?\ ) "pass") (car kill-ring)))
    (should (eobp))
    (should (bolp))))

(ert-deftest py-ert-mark-indent-test-lJ6Hny ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        sdfasde
        pass"
    (goto-char (point-min) )
    (search-forward "sdfasde")
    (py-mark-indent)
    ;; (message "%s" (buffer-substring-no-properties (region-beginning) (region-end)))
    (should (eq 28 (length (buffer-substring-no-properties (region-beginning) (region-end)))))))

(ert-deftest py-ert-edit-docstring-write-content-back-test-mh1es0 ()
  (py-test-with-temp-buffer-point-min
      "def foo():
    \"\"\"def bar():
    pass\"\"\"
    pass
"
    (goto-char (point-min) )
    (let ((py-edit-buffer "Edit docstring"))
      (search-forward "pass" nil t 1)
      (py-edit-docstring)
      (set-buffer py-edit-buffer)
      (switch-to-buffer (current-buffer))
      (goto-char (point-min))
      (end-of-line)
      (newline)
      (insert "'''My edit-docstring ert-test'''")
      (beginning-of-line)
      (indent-according-to-mode)
      (py--write-edit)
      ;; back in orginial test buffer
      (forward-line -1)
      (should (and (nth 3 (parse-partial-sexp (point-min) (point)))
                   (nth 8 (parse-partial-sexp (point-min) (point)))))
      )))

(ert-deftest py-execute-region-no-transmm-test-1-7nmEse ()
  (py-test-with-temp-buffer
      "print(u'\\xA9')"
    (goto-char (point-max))
    (let (transient-mark-mode)
      (push-mark)
      (beginning-of-line)
      (py-shift-region-right)
      (should (eq 4 (current-indentation))))))

(ert-deftest py-execute-import-or-reload-test-ZYUvdh ()
  (py-test-with-temp-buffer
      "#! /usr/bin/env python
# -*- coding: utf-8 -*-
import os"
    (goto-char (point-max))
    (py-execute-import-or-reload)
    (should t)))

(ert-deftest py-syntax-highlighting-for-builtin-functions-55-test-qijqlm ()
  (py-test-with-temp-buffer
      "range(len(list((1, 2, 3))))"
    (goto-char (point-max))
    ;; (goto-char (point-max))
    (font-lock-fontify-region (point-min) (point-max))
    (sit-for 0.1)
    (search-backward "le")
    (should (face-equal (face-at-point) 'py-builtins-face))))

(ert-deftest py-shell-dedicated-buffer-test-t3Sizn ()
  (let ((buffer (py-shell nil nil t)))
  (should (buffer-live-p buffer))))

(ert-deftest py-shell-test-t3Sizn ()
  (let ((buffer (py-shell nil nil t)))
    (with-current-buffer buffer
      (switch-to-buffer (current-buffer))
      (goto-char (point-max))
      (insert "def")
      (backward-char)
      (should (eq (char-after) ?f)))))

(ert-deftest py-master-file-not-honored-lp-794850-test-P6QZmU ()
  (py-test-with-temp-buffer

"
# -*- coding: utf-8 -*-

# Local Variables:
# py-master-file: \"/tmp/my-master.py\"
# End:
 "

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

(ert-deftest py-named-shell-python3-794850-test-P6QZmU ()
  (py-test-mode-explizit
      "foo"
    'fundamental-mode
    py-debug-p
    ;; (switch-to-buffer (current-buffer))
    (call-interactively 'python3)
    (should (buffer-live-p (get-buffer "*Python3*")))
    (py-kill-buffer-unconditional (get-buffer "*Python3*"))))

(ert-deftest py-named-shell-ipython3-794850-test-P6QZmU ()
  (py-test-mode-explizit
      "foo"
    'fundamental-mode
    py-debug-p
    (call-interactively 'ipython3)
    (should (buffer-live-p (get-buffer "*IPython3*")))
    (py-kill-buffer-unconditional (get-buffer "*IPython3*"))))

(ert-deftest py-named-shell-ipython-794850-test-P6QZmU ()
  (py-test-mode-explizit
      "foo"
    'fundamental-mode
    py-debug-p
    (call-interactively 'ipython)
    (should (buffer-live-p (get-buffer "*IPython*")))
    (py-kill-buffer-unconditional (get-buffer "*IPython*"))))

(ert-deftest py-run-python-test-QDE84k ()
  "Test built-in python.el."
  (run-python)
  (should (buffer-live-p (get-buffer "*Python*"))))

(provide 'py-ert-misc-tests-1)
;;; py-ert-misc-tests-1.el ends here
