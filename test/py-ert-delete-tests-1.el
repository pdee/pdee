;; py-ert-delete-tests.el --- Tests, some adapted from python.el -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Free Software Foundation, Inc.
;; Copyright (C) 2014-2020 Andreas Röhler, <andreas.roehler@online.de>

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

(ert-deftest py-ert-electric-kill-backward-bracket-test-COL0W9 ()
  (let ((py-electric-kill-backward-p t))
    (py-test-with-temp-buffer
      "mystring[0:1]"
      (goto-char (point-max))
      (py-electric-backspace 1)
      (should (eq ?\] (char-after))))))

(ert-deftest py-ert-electric-kill-backward-region-test-i6f2E0 ()
  (py-test-with-temp-buffer
      "mystring[0:1]     "
    (goto-char (point-max))
    (let ((py-electric-kill-backward-p t)
	  (delete-active-region t)
	  (transient-mark-mode t))
      (skip-chars-backward " \t\r\n\f")
      (set-mark (point))
      (goto-char (point-max))
      (py-electric-backspace 1)
      (should (eq ?\] (char-before))))))

(ert-deftest py-ert-electric-delete-eob-test-Az64kR ()
  (py-test-with-temp-buffer
      "mystring[0:1]     "
    (goto-char (point-max))
    (let ((py-electric-kill-backward-p t)
	  (delete-active-region t)
	  (transient-mark-mode t))
      (skip-chars-backward " \t")
      (set-mark (point))
      (skip-chars-forward " \t")
      (py-electric-delete)
      (should (eobp)))))

(ert-deftest py-ert-electric-delete-test-mWjz2H ()
  (let ((py-electric-kill-backward-p t)
	(delete-active-region t)
	(transient-mark-mode t))
    (py-test-with-temp-buffer
	"mystring[0:1]     "
      (goto-char (point-max))
      (set-mark (point))
      (skip-chars-backward " \t\r\n\f")
      (py-electric-delete)
      (should (eobp)))))

(ert-deftest py-ert-electric-kill-backward-paren-test-2H1bGy ()
  (let ((py-electric-kill-backward-p t))
    (py-test-with-temp-buffer
      "mystring(\"asdf\")"
      (goto-char (point-max))
      (py-electric-backspace 1)
      (should (eq ?\) (char-after)))
      )))

(ert-deftest py-ert-electric-kill-backward-brace-test-4Osdip ()
  (let ((py-electric-kill-backward-p t))
    (py-test-with-temp-buffer
      "mystring{0 . 1}"
      (goto-char (point-max))
      (py-electric-backspace 1)
      (should (eq ?\} (char-after))))))


(provide 'py-ert-delete-tests-1)
;;; py-ert-delete-tests-1.el ends here
