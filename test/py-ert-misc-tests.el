;; py-ert-misc-tests.el --- testing python-mode.el -*- lexical-binding: t; -*-

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

(require 'py-setup-ert-tests)

(ert-deftest py-ert-borks-all-lp-1294820-sIKMyz ()
  (py-test-point-min
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
    'python-mode
    'py-verbose-p
    (goto-char (point-min))
    ;; (font-lock-fontify-region (point-min)(point-max))
    (search-forward "def baz(self):")
    (fill-paragraph)
    (forward-line -1)
    (should (bolp))
    (should (looking-at "    some_"))))

(ert-deftest py-ert-in-comment-p-test-G6FUaB ()
  (py-test
      "# "
    'python-mode
    'py-verbose-p
    (should (py--in-comment-p))))

(ert-deftest py-ert-in-sq-string-p-test-nwha1D ()
  (py-test
      "' "
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (should (py-in-string-p))))

(ert-deftest py-ert-in-dq-string-p-test-lYrt9b ()
  (py-test
      "\" "
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (should (py-in-string-p))))

(ert-deftest py-ert-in-sq-tqs-string-p-test-EwMSzz ()
  (py-test
      "''' "
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (should (py-in-string-p))))

(ert-deftest py-ert-in-dq-tqs-string-p-test-jkHHQH ()
  (py-test
      "\"\"\" "
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (should (py-in-string-p))))

(ert-deftest py-ert-electric-delete-test-HecKiw ()
  (py-test-point-min
      "  {}"
    'python-mode
    'py-verbose-p
    (goto-char (point-min))
    (py-electric-delete)
    (should (eq (char-after) ?{))))

(ert-deftest py-ert-fill-plain-string-test-OEykwr ()
  (py-test-point-min
      "'''asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdfasdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf
'''"
    'python-mode
    'py-verbose-p
    (goto-char (point-min))
    (forward-char 4)
    (fill-paragraph)
    (forward-line 1)
    (should (not (py-empty-line-p)))))

(ert-deftest py-ert-nil-docstring-style-lp-1477422-test-6wpqLB ()
  (py-test-point-min
      "def foo():
    '''asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdfasdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf'''"
    'python-mode
    'py-verbose-p
    (goto-char (point-min))
    (let (py-docstring-style)
      (search-forward "'''")
      (save-excursion
        (fill-paragraph))
      (forward-line 1)
      (should (not (py-empty-line-p))))))

(ert-deftest py-markup-region-as-section-test-KetMYL ()
  (py-test-point-min
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
    'python-mode
    'py-verbose-p
    (goto-char (point-min))
    (search-forward "fertig")
    (py-sectionize-region (match-beginning 0) (line-end-position))
    (py-mark-section)
    (should (eq (region-beginning) 377 ))
    (should (eq (region-end) 414 ))))

(ert-deftest py-test-embedded-51-test-sgaO9V ()
  (py-test
      "from PrintEngine import *
GeomSim."
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (ignore-errors (py-indent-or-complete))
    ;; (sit-for 0.1)
    (should (eq (char-before) ?.))))

(ert-deftest py--pdb-versioned-test-QoHSpJ-oNuvXf ()
  (py-test
      ""
    'python-mode
    'py-verbose-p
    (let ((py-shell-name "python3"))
      (goto-char (point-max))
      (should (string= "pdb3" (py--pdb-versioned))))))

(ert-deftest py-ert-copy-indent-test-UbzMto ()
  (py-test-point-min
      "class A(object):
    def a(self):
        sdfasde
        pass"
    'python-mode
    'py-verbose-p
    (goto-char (point-min) )
    (search-forward "sdfasde")
    (py-copy-indent)
    (should (string-match "sdfasde" (car kill-ring)))
    (should (not (py--beginning-of-indent-p)))
    (py-backward-statement)
    (should (py--beginning-of-indent-p))))

(ert-deftest py-ert-delete-indent-test-HhZNOr ()
  (py-test-point-min
      "class A(object):
    def a(self):
        sdfasde
        pass"
    'python-mode
    'py-verbose-p
    (goto-char (point-min) )
    (search-forward "sdfasde")
    (py-delete-indent)
    (should (eobp))
    (should (bolp))))

(ert-deftest py-ert-kill-indent-test-ECwA5u ()
  (py-test-point-min
      "class A(object):
    def a(self):
        sdfasde
        pass"
    'python-mode
    'py-verbose-p
    (goto-char (point-min) )
    (search-forward "sdfasde")
    (py-kill-indent)
    (should (string= (concat (make-string 8 ?\ ) "sdfasde\n" (make-string 8 ?\ ) "pass") (car kill-ring)))
    (should (eobp))
    (should (bolp))))

(ert-deftest py-ert-mark-indent-test-lJ6Hny ()
  (py-test-point-min
      "class A(object):
    def a(self):
        sdfasde
        pass"
    'python-mode
    'py-verbose-p
    (goto-char (point-min) )
    (search-forward "sdfasde")
    (py-mark-indent)
    ;; (message "%s" (buffer-substring-no-properties (region-beginning) (region-end)))
    (should (eq 28 (length (buffer-substring-no-properties (region-beginning) (region-end)))))))

(ert-deftest py-ert-edit-docstring-write-content-back-test-mh1es0 ()
  (py-test-point-min
      "def foo():
    \"\"\"def bar():
    pass\"\"\"
    pass
"
    'python-mode
    'py-verbose-p
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
                   (nth 8 (parse-partial-sexp (point-min) (point))))))))

(ert-deftest py-execute-region-no-transmm-test-1-7nmEse ()
  (py-test
      "print(u'\\xA9')"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (let (transient-mark-mode)
      (push-mark)
      (beginning-of-line)
      (py-shift-region-right)
      (should (eq 4 (current-indentation))))))

;; (ert-deftest py-syntax-highlighting-for-builtin-functions-55-test-qijqlm ()
;;   (py-test
;;       "range(len(list((1, 2, 3))))"
;;     (font-lock-fontify-region (point-min) (point-max))
;;     (goto-char (point-max))
;;     (search-backward "e")
;;     (sit-for 0.1)
;;     (should (face-equal (face-at-point) 'py-builtins-face))))

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

(when (featurep  'comint-mime)
  (ert-deftest py-comint-mime-test-7JbtYW ()
    (py-test
	"__COMINT_MIME_setup"
      'python-mode
      'py-verbose-p
      (push '(inferior-python-mode . comint-mime-setup-python)
	    comint-mime-setup-function-alist)
      (push '(py-shell-mode . comint-mime-setup-py-shell)
	    comint-mime-setup-function-alist)
      (add-hook 'py-shell-mode-hook 'comint-mime-setup)
      (py-execute-buffer-ipython3)
      (message "%s" py-result)
      (should (string-match "__COMINT_MIME_setup" py-result)))))

(provide 'py-ert-misc-tests)
;;; py-ert-misc-tests.el ends here
