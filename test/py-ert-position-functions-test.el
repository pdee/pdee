;;; py-ert-position-functions-test.el --- test	     -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(ert-deftest py-ert--beginning-of-def-position-functionp-test ()
  (should (functionp 'py--beginning-of-def-position)))

(ert-deftest py-ert--beginning-of-class-position-functionp-test ()
  (should (functionp 'py--beginning-of-class-position)))

(ert-deftest py-ert--beginning-of-def-or-class-position-functionp-test ()
  (should (functionp 'py--beginning-of-def-or-class-position)))

(ert-deftest py-ert--beginning-of-expression-position-functionp-test ()
  (should (functionp 'py--beginning-of-expression-position)))

(ert-deftest py-ert--beginning-of-partial-expression-position-functionp-test ()
  (should (functionp 'py--beginning-of-partial-expression-position)))

(ert-deftest py-ert--beginning-of-minor-block-position-functionp-test ()
  (should (functionp 'py--beginning-of-minor-block-position)))

(ert-deftest py-ert--beginning-of-if-block-position-functionp-test ()
  (should (functionp 'py--beginning-of-if-block-position)))

(ert-deftest py-ert--beginning-of-try-block-position-functionp-test ()
  (should (functionp 'py--beginning-of-try-block-position)))

(ert-deftest py-ert--beginning-of-except-block-position-functionp-test ()
  (should (functionp 'py--beginning-of-except-block-position)))

(ert-deftest py-ert--beginning-of-statement-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-statement-position-bol)))

(ert-deftest py-ert--beginning-of-block-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-block-position-bol)))

(ert-deftest py-ert--beginning-of-clause-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-clause-position-bol)))

(ert-deftest py-ert--beginning-of-block-or-clause-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-block-or-clause-position-bol)))

(ert-deftest py-ert--beginning-of-def-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-def-position-bol)))

(ert-deftest py-ert--beginning-of-class-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-class-position-bol)))

(ert-deftest py-ert--beginning-of-def-or-class-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-def-or-class-position-bol)))

(ert-deftest py-ert--beginning-of-minor-block-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-minor-block-position-bol)))

(ert-deftest py-ert--beginning-of-if-block-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-if-block-position-bol)))

(ert-deftest py-ert--beginning-of-try-block-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-try-block-position-bol)))

(ert-deftest py-ert--beginning-of-except-block-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-except-block-position-bol)))

(ert-deftest py-ert--beginning-of-statement-position-functionp-test ()
  (should (functionp 'py--beginning-of-statement-position)))

(ert-deftest py-ert--beginning-of-block-position-functionp-test ()
  (should (functionp 'py--beginning-of-block-position)))

(ert-deftest py-ert--beginning-of-clause-position-functionp-test ()
  (should (functionp 'py--beginning-of-clause-position)))

(ert-deftest py-ert--beginning-of-block-or-clause-position-functionp-test ()
  (should (functionp 'py--beginning-of-block-or-clause-position)))

(ert-deftest py-ert--beginning-of-buffer-position-functionp-test ()
  (should (functionp 'py--beginning-of-buffer-position)))

(ert-deftest py-ert--beginning-of-comment-position-functionp-test ()
  (should (functionp 'py--beginning-of-comment-position)))

(ert-deftest py-ert--end-of-comment-position-functionp-test ()
  (should (functionp 'py--end-of-comment-position)))

(ert-deftest py-ert--beginning-of-paragraph-position-functionp-test ()
  (should (functionp 'py--beginning-of-paragraph-position)))

(ert-deftest py-ert--end-of-paragraph-position-functionp-test ()
  (should (functionp 'py--end-of-paragraph-position)))

(ert-deftest py-ert-end-of-list-position-functionp-test ()
  (should (functionp 'py-end-of-list-position)))

(ert-deftest py-ert--end-of-buffer-position-functionp-test ()
  (should (functionp 'py--end-of-buffer-position)))

(ert-deftest py-ert-list-beginning-position-functionp-test ()
  (should (functionp 'py-list-beginning-position)))

(ert-deftest py-ert--end-of-statement-position-functionp-test ()
  (should (functionp 'py--end-of-statement-position)))

(ert-deftest py-ert--end-of-block-position-functionp-test ()
  (should (functionp 'py--end-of-block-position)))

(ert-deftest py-ert--end-of-clause-position-functionp-test ()
  (should (functionp 'py--end-of-clause-position)))

(ert-deftest py-ert--end-of-block-or-clause-position-functionp-test ()
  (should (functionp 'py--end-of-block-or-clause-position)))

(ert-deftest py-ert--end-of-def-position-functionp-test ()
  (should (functionp 'py--end-of-def-position)))

(ert-deftest py-ert--end-of-class-position-functionp-test ()
  (should (functionp 'py--end-of-class-position)))

(ert-deftest py-ert--end-of-def-or-class-position-functionp-test ()
  (should (functionp 'py--end-of-def-or-class-position)))

(ert-deftest py-ert--end-of-buffer-position-functionp-test ()
  (should (functionp 'py--end-of-buffer-position)))

(ert-deftest py-ert--end-of-expression-position-functionp-test ()
  (should (functionp 'py--end-of-expression-position)))

(ert-deftest py-ert--end-of-partial-expression-position-functionp-test ()
  (should (functionp 'py--end-of-partial-expression-position)))

(ert-deftest py-ert--end-of-minor-block-position-functionp-test ()
  (should (functionp 'py--end-of-minor-block-position)))

(ert-deftest py-ert--end-of-if-block-position-functionp-test ()
  (should (functionp 'py--end-of-if-block-position)))

(ert-deftest py-ert--end-of-try-block-position-functionp-test ()
  (should (functionp 'py--end-of-try-block-position)))

(ert-deftest py-ert--end-of-except-block-position-functionp-test ()
  (should (functionp 'py--end-of-except-block-position)))

(ert-deftest py-ert--end-of-top-level-position-functionp-test ()
  (should (functionp 'py--end-of-top-level-position)))

(ert-deftest py-ert--end-of-statement-position-bol-functionp-test ()
  (should (functionp 'py--end-of-statement-position-bol)))

(ert-deftest py-ert--end-of-block-position-bol-functionp-test ()
  (should (functionp 'py--end-of-block-position-bol)))

(ert-deftest py-ert--end-of-clause-position-bol-functionp-test ()
  (should (functionp 'py--end-of-clause-position-bol)))

(ert-deftest py-ert--end-of-block-or-clause-position-bol-functionp-test ()
  (should (functionp 'py--end-of-block-or-clause-position-bol)))

(ert-deftest py-ert--end-of-def-position-bol-functionp-test ()
  (should (functionp 'py--end-of-def-position-bol)))

(ert-deftest py-ert--end-of-class-position-bol-functionp-test ()
  (should (functionp 'py--end-of-class-position-bol)))

(ert-deftest py-ert--end-of-minor-block-position-bol-functionp-test ()
  (should (functionp 'py--end-of-minor-block-position-bol)))

(ert-deftest py-ert--end-of-if-block-position-bol-functionp-test ()
  (should (functionp 'py--end-of-if-block-position-bol)))

(ert-deftest py-ert--end-of-try-block-position-bol-functionp-test ()
  (should (functionp 'py--end-of-try-block-position-bol)))

(ert-deftest py-ert-kill-block-functionp-test ()
  (should (functionp 'py-kill-block)))

(provide 'py-ert-position-functions-test)
;;; py-ert-position-functions-test.el ends here
