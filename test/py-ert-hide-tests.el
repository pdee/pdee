;;; py-ert-hide-tests.el ---  -*- lexical-binding: t; -*-

;; URL: https://gitlab.com/python-mode-devs
;; Keywords: languages, convenience

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

(require 'py-setup-ert-tests)

;; (ert-deftest py-ert-hide-partial-expression-test-Li7vPR ()

;;   (py-test-point-min "
;; class kugel(object):
;;     zeit = time.strftime('%Y%m%d--%H-%M-%S')

;;     def pylauf(self):
;;         \"\"\"Eine Doku fuer pylauf\"\"\"
;;         ausgabe = [\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]

;;         ausgabe[0] = treffer
;;         if treffer in gruen:
;;             # print \"0, Gruen\"
;;             datei.write(str(spiel[i]) + \"\\n\")
;; "
;;     (font-lock-ensure)
;;     (search-forward "+ \"")
;;     (py-hide-partial-expression)
;;     (should (string-match "overlay from " (prin1-to-string (car (overlays-at (point))))))
;;     (py-show)
;;     (should (not (string-match "overlay" (prin1-to-string (car (overlays-at (point)))))))
;;     ))

(ert-deftest py-ert-hide-expression-test ()
  (py-test-point-min "
class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')
    def pylauf(self):
        \"\"\"Eine Doku fuer pylauf\"\"\"
        ausgabe = [\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]
        ausgabe[0] = treffer
        if treffer in gruen:
            # print \"0, Gruen\"
            datei.write(str(spiel[i]) + \"\\n\")
"
                     'python-mode
                     'py-debug-p
                     (when py-debug-p (font-lock-ensure))
                     (font-lock-ensure)
                     (search-forward "+ \"")
                     (py-hide-expression)
                     (should (string-match "overlay from " (prin1-to-string (car (overlays-at (point))))))
                     (py-show)
                     (should (not (string-match "overlay" (prin1-to-string (car (overlays-at (point)))))))))

(ert-deftest py-ert-hide-clause-test-qsv8kt ()
  (py-test-point-min "
class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')
    def pylauf(self):
        \"\"\"Eine Doku fuer pylauf\"\"\"
        ausgabe = [\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]
        ausgabe[0] = treffer
        if treffer in gruen:
            # print \"0, Gruen\"
            datei.write(str(spiel[i]) + \"\\n\")
"
                     'python-mode
                     'py-debug-p
                     (when py-debug-p (font-lock-ensure))
                     (font-lock-ensure)
                     (search-forward "+ \"")
                     (py-hide-clause)
                     (should (string-match "overlay from " (prin1-to-string (car (overlays-at (point))))))
                     (py-show)
                     (should (not (string-match "overlay" (prin1-to-string (car (overlays-at (point)))))))))


(ert-deftest py-ert-hide-clause-test-rO7k4k ()
  (py-test "if 0 < treffer:
    if 18 < treffer:
        ausgabe[6] = treffer
    else:
        ausgabe[7] = treffer
"
           'python-mode
           'py-debug-p
           (when py-debug-p (font-lock-ensure))
           (font-lock-ensure)
           (goto-char (point-max))
           (search-backward "6")
           (py-hide-clause)
           ;; (should (search-forward "else"))
           (should-not (string-match (prin1-to-string (car (overlays-at (point)))) "overlay from " ))
           (should-not (string-match (prin1-to-string (car (overlays-at (point)))) "overlay from "))))

(ert-deftest py-ert-hide-block-test-5j57vC ()
  (py-test-point-min "
class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')
    def pylauf(self):
        \"\"\"Eine Doku fuer pylauf\"\"\"
        ausgabe = [\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]
        ausgabe[0] = treffer
        if treffer in gruen:
            # print \"0, Gruen\"
            datei.write(str(spiel[i]) + \"\\n\")
"
                     'python-mode
                     'py-debug-p
                     (when py-debug-p (font-lock-ensure))
                     ;; (font-lock-ensure)
                     (search-forward "+ \"")
                     (py-hide-block)
                     (should (string-match "overlay from " (prin1-to-string (car (overlays-at (point))))))
                     (py-show)
                     (should (not (string-match "overlay" (prin1-to-string (car (overlays-at (point)))))))))

(ert-deftest py-ert-hide-def-test ()
  (py-test-point-min "
class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')
    def pylauf(self):
        \"\"\"Eine Doku fuer pylauf\"\"\"
        ausgabe = [\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]
        ausgabe[0] = treffer
        if treffer in gruen:
            # print \"0, Gruen\"
            datei.write(str(spiel[i]) + \"\\n\")
"
                     'python-mode
                     'py-debug-p
                     (when py-debug-p (font-lock-ensure))
                     ;; (font-lock-ensure)
                     (search-forward "+ \"")
                     (py-hide-def)
                     (should (string-match "overlay from " (prin1-to-string (car (overlays-at (point))))))
                     (py-show)
                     (should (not (string-match "overlay" (prin1-to-string (car (overlays-at (point)))))))))

(ert-deftest py-ert-hide-class-test ()
  (py-test-point-min "
class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')
    def pylauf(self):
        \"\"\"Eine Doku fuer pylauf\"\"\"
        ausgabe = [\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]
        ausgabe[0] = treffer
        if treffer in gruen:
            # print \"0, Gruen\"
            datei.write(str(spiel[i]) + \"\\n\")
"
                     'python-mode
                     'py-debug-p
                     (when py-debug-p (font-lock-ensure))
                     (switch-to-buffer (current-buffer))
                     ;; (font-lock-ensure)
                     (search-forward "+ \"")
                     (py-hide-class)
                     (should (string-match "overlay from " (prin1-to-string (car (overlays-at (point))))))
                     (py-show)
                     (should (not (string-match "overlay" (prin1-to-string (car (overlays-at (point)))))))))

(ert-deftest py-ert-hide-indent-test-dTRpuQ ()
  (py-test-point-min "
class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')
    def pylauf(self):
        \"\"\"Eine Doku fuer pylauf\"\"\"
        ausgabe = [\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]
        ausgabe[0] = treffer
        if treffer in gruen:
            print \"0, Gruen\"
            # print \"0, Gruen\"
            datei.write(str(spiel[i]) + \"\\n\")
"
                     'python-mode
                     'py-debug-p
                     (when py-debug-p (font-lock-ensure))
                     ;; (font-lock-ensure)
                     (search-forward "+ \"")
                     (py-hide-indent)
                     (should (string-match "overlay from " (prin1-to-string (car (overlays-at (point))))))
                     (py-show)
                     (should (not (string-match "overlay" (prin1-to-string (car (overlays-at (point)))))))))

(ert-deftest py-ert-hide-block-test-dTRpuQ ()
  (py-test
   "def f(first, second):
    if first    == 1:
        return 11
    elif first  == 2:
        return 22
    if second   == 12:
        return 211
    elif second == 22:
        return 244
    else:
        return 25
"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (should (search-backward "else"))
   (py-hide-block)
   (should (string-match "overlay from " (prin1-to-string (car (overlays-at (point))))))))

(ert-deftest py-ert-show-block-test-Ik9nab ()
  (py-test-point-min
"def main():
    if len(sys.argv) == 1:
        usage()
        # sys.exit()
"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-min))
   (should (search-forward "usage"))
   (py-hide-block)
   (should (string-match "overlay from " (prin1-to-string (car (overlays-at (point))))))
   (py-hide-show)
   (should-not (car (overlays-at (point))))
   ))

(ert-deftest py-ert-show-block-test-J8V1AH ()
  (py-test-point-min
"def main():
    if len(sys.argv) == 1:
        usage()
        # sys.exit()
"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-min))
   (should (search-forward "usage"))
   (py-hide-block)
   (should (string-match "overlay from " (prin1-to-string (car (overlays-at (point))))))
   (py-show)
   (should-not (car (overlays-at (point))))
   ))


(provide 'py-ert-hide-tests)
;;; py-ert-hide-tests.el ends here
