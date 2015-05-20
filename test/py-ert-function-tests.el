;;; py-ert-function-tests.el --- functionp ert tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Andreas Roehler

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: lisp

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

;;; Commentary:

;;

;;; Code:

(ert-deftest py-ert-virtualenv-filter-functionp-test ()
  (should (functionp 'virtualenv-filter)))

(ert-deftest py-ert-virtualenv-append-path-functionp-test ()
  (should (functionp 'virtualenv-append-path)))

(ert-deftest py-ert-virtualenv-add-to-path-functionp-test ()
  (should (functionp 'virtualenv-add-to-path)))

(ert-deftest py-ert-virtualenv-current-functionp-test ()
  (should (functionp 'virtualenv-current)))

(ert-deftest py-ert-virtualenv-deactivate-functionp-test ()
  (should (functionp 'virtualenv-deactivate)))

(ert-deftest py-ert-virtualenv-activate-functionp-test ()
  (should (functionp 'virtualenv-activate)))

(ert-deftest py-ert-virtualenv-deactivate-functionp-test ()
  (should (functionp 'virtualenv-deactivate)))

(ert-deftest py-ert-virtualenv-p-functionp-test ()
  (should (functionp 'virtualenv-p)))

(ert-deftest py-ert-virtualenv-workon-complete-functionp-test ()
  (should (functionp 'virtualenv-workon-complete)))

(ert-deftest py-ert-virtualenv-workon-functionp-test ()
  (should (functionp 'virtualenv-workon)))

(ert-deftest py-ert-py--beginning-of-block-p-functionp-test ()
  (should (functionp 'py--beginning-of-block-p)))

(ert-deftest py-ert-py--beginning-of-clause-p-functionp-test ()
  (should (functionp 'py--beginning-of-clause-p)))

(ert-deftest py-ert-py--beginning-of-block-or-clause-p-functionp-test ()
  (should (functionp 'py--beginning-of-block-or-clause-p)))

(ert-deftest py-ert-py--beginning-of-def-p-functionp-test ()
  (should (functionp 'py--beginning-of-def-p)))

(ert-deftest py-ert-py--beginning-of-class-p-functionp-test ()
  (should (functionp 'py--beginning-of-class-p)))

(ert-deftest py-ert-py--beginning-of-def-or-class-p-functionp-test ()
  (should (functionp 'py--beginning-of-def-or-class-p)))

(ert-deftest py-ert-py--beginning-of-if-block-p-functionp-test ()
  (should (functionp 'py--beginning-of-if-block-p)))

(ert-deftest py-ert-py--beginning-of-try-block-p-functionp-test ()
  (should (functionp 'py--beginning-of-try-block-p)))

(ert-deftest py-ert-py--beginning-of-minor-block-p-functionp-test ()
  (should (functionp 'py--beginning-of-minor-block-p)))

(ert-deftest py-ert-py--beginning-of-for-block-p-functionp-test ()
  (should (functionp 'py--beginning-of-for-block-p)))

(ert-deftest py-ert-py--beginning-of-top-level-p-functionp-test ()
  (should (functionp 'py--beginning-of-top-level-p)))

(ert-deftest py-ert-py--beginning-of-statement-p-functionp-test ()
  (should (functionp 'py--beginning-of-statement-p)))

(ert-deftest py-ert-py--beginning-of-expression-p-functionp-test ()
  (should (functionp 'py--beginning-of-expression-p)))

(ert-deftest py-ert-py--beginning-of-partial-expression-p-functionp-test ()
  (should (functionp 'py--beginning-of-partial-expression-p)))

(ert-deftest py-ert-py--beginning-of-block-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-block-bol-p)))

(ert-deftest py-ert-py--beginning-of-clause-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-clause-bol-p)))

(ert-deftest py-ert-py--beginning-of-block-or-clause-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-block-or-clause-bol-p)))

(ert-deftest py-ert-py--beginning-of-def-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-def-bol-p)))

(ert-deftest py-ert-py--beginning-of-class-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-class-bol-p)))

(ert-deftest py-ert-py--beginning-of-def-or-class-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-def-or-class-bol-p)))

(ert-deftest py-ert-py--beginning-of-if-block-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-if-block-bol-p)))

(ert-deftest py-ert-py--beginning-of-try-block-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-try-block-bol-p)))

(ert-deftest py-ert-py--beginning-of-minor-block-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-minor-block-bol-p)))

(ert-deftest py-ert-py--beginning-of-for-block-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-for-block-bol-p)))

(ert-deftest py-ert-py--beginning-of-statement-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-statement-bol-p)))

(ert-deftest py-ert-py-up-statement-functionp-test ()
  (should (functionp 'py-up-statement)))

(ert-deftest py-ert-py-down-statement-functionp-test ()
  (should (functionp 'py-down-statement)))

(ert-deftest py-ert-py-up-base-functionp-test ()
  (should (functionp 'py-up-base)))

(ert-deftest py-ert-py-down-base-functionp-test ()
  (should (functionp 'py-down-base)))

(ert-deftest py-ert-py-up-base-bol-functionp-test ()
  (should (functionp 'py-up-base-bol)))

(ert-deftest py-ert-py-down-base-bol-functionp-test ()
  (should (functionp 'py-down-base-bol)))

(ert-deftest py-ert-py-up-block-functionp-test ()
  (should (functionp 'py-up-block)))

(ert-deftest py-ert-py-up-minor-block-functionp-test ()
  (should (functionp 'py-up-minor-block)))

(ert-deftest py-ert-py-up-clause-functionp-test ()
  (should (functionp 'py-up-clause)))

(ert-deftest py-ert-py-up-block-or-clause-functionp-test ()
  (should (functionp 'py-up-block-or-clause)))

(ert-deftest py-ert-py-up-def-functionp-test ()
  (should (functionp 'py-up-def)))

(ert-deftest py-ert-py-up-class-functionp-test ()
  (should (functionp 'py-up-class)))

(ert-deftest py-ert-py-up-def-or-class-functionp-test ()
  (should (functionp 'py-up-def-or-class)))

(ert-deftest py-ert-py-down-block-functionp-test ()
  (should (functionp 'py-down-block)))

(ert-deftest py-ert-py-down-minor-block-functionp-test ()
  (should (functionp 'py-down-minor-block)))

(ert-deftest py-ert-py-down-clause-functionp-test ()
  (should (functionp 'py-down-clause)))

(ert-deftest py-ert-py-down-block-or-clause-functionp-test ()
  (should (functionp 'py-down-block-or-clause)))

(ert-deftest py-ert-py-down-def-functionp-test ()
  (should (functionp 'py-down-def)))

(ert-deftest py-ert-py-down-class-functionp-test ()
  (should (functionp 'py-down-class)))

(ert-deftest py-ert-py-down-def-or-class-functionp-test ()
  (should (functionp 'py-down-def-or-class)))

(ert-deftest py-ert-py-up-block-bol-functionp-test ()
  (should (functionp 'py-up-block-bol)))

(ert-deftest py-ert-py-up-minor-block-bol-functionp-test ()
  (should (functionp 'py-up-minor-block-bol)))

(ert-deftest py-ert-py-up-clause-bol-functionp-test ()
  (should (functionp 'py-up-clause-bol)))

(ert-deftest py-ert-py-up-block-or-clause-bol-functionp-test ()
  (should (functionp 'py-up-block-or-clause-bol)))

(ert-deftest py-ert-py-up-def-bol-functionp-test ()
  (should (functionp 'py-up-def-bol)))

(ert-deftest py-ert-py-up-class-bol-functionp-test ()
  (should (functionp 'py-up-class-bol)))

(ert-deftest py-ert-py-up-def-or-class-bol-functionp-test ()
  (should (functionp 'py-up-def-or-class-bol)))

(ert-deftest py-ert-py-down-block-bol-functionp-test ()
  (should (functionp 'py-down-block-bol)))

(ert-deftest py-ert-py-down-minor-block-bol-functionp-test ()
  (should (functionp 'py-down-minor-block-bol)))

(ert-deftest py-ert-py-down-clause-bol-functionp-test ()
  (should (functionp 'py-down-clause-bol)))

(ert-deftest py-ert-py-down-block-or-clause-bol-functionp-test ()
  (should (functionp 'py-down-block-or-clause-bol)))

(ert-deftest py-ert-py-down-def-bol-functionp-test ()
  (should (functionp 'py-down-def-bol)))

(ert-deftest py-ert-py-down-class-bol-functionp-test ()
  (should (functionp 'py-down-class-bol)))

(ert-deftest py-ert-py-down-def-or-class-bol-functionp-test ()
  (should (functionp 'py-down-def-or-class-bol)))

(ert-deftest py-ert-py--end-of-statement-position-functionp-test ()
  (should (functionp 'py--end-of-statement-position)))

(ert-deftest py-ert-py--end-of-block-position-functionp-test ()
  (should (functionp 'py--end-of-block-position)))

(ert-deftest py-ert-py--end-of-clause-position-functionp-test ()
  (should (functionp 'py--end-of-clause-position)))

(ert-deftest py-ert-py--end-of-block-or-clause-position-functionp-test ()
  (should (functionp 'py--end-of-block-or-clause-position)))

(ert-deftest py-ert-py--end-of-def-position-functionp-test ()
  (should (functionp 'py--end-of-def-position)))

(ert-deftest py-ert-py--end-of-class-position-functionp-test ()
  (should (functionp 'py--end-of-class-position)))

(ert-deftest py-ert-py--end-of-def-or-class-position-functionp-test ()
  (should (functionp 'py--end-of-def-or-class-position)))

(ert-deftest py-ert-py--end-of-buffer-position-functionp-test ()
  (should (functionp 'py--end-of-buffer-position)))

(ert-deftest py-ert-py--end-of-expression-position-functionp-test ()
  (should (functionp 'py--end-of-expression-position)))

(ert-deftest py-ert-py--end-of-partial-expression-position-functionp-test ()
  (should (functionp 'py--end-of-partial-expression-position)))

(ert-deftest py-ert-py--end-of-minor-block-position-functionp-test ()
  (should (functionp 'py--end-of-minor-block-position)))

(ert-deftest py-ert-py--end-of-if-block-position-functionp-test ()
  (should (functionp 'py--end-of-if-block-position)))

(ert-deftest py-ert-py--end-of-try-block-position-functionp-test ()
  (should (functionp 'py--end-of-try-block-position)))

(ert-deftest py-ert-py--end-of-except-block-position-functionp-test ()
  (should (functionp 'py--end-of-except-block-position)))

(ert-deftest py-ert-py--end-of-top-level-position-functionp-test ()
  (should (functionp 'py--end-of-top-level-position)))

(ert-deftest py-ert-py--end-of-statement-position-bol-functionp-test ()
  (should (functionp 'py--end-of-statement-position-bol)))

(ert-deftest py-ert-py--end-of-block-position-bol-functionp-test ()
  (should (functionp 'py--end-of-block-position-bol)))

(ert-deftest py-ert-py--end-of-clause-position-bol-functionp-test ()
  (should (functionp 'py--end-of-clause-position-bol)))

(ert-deftest py-ert-py--end-of-block-or-clause-position-bol-functionp-test ()
  (should (functionp 'py--end-of-block-or-clause-position-bol)))

(ert-deftest py-ert-py--end-of-def-position-bol-functionp-test ()
  (should (functionp 'py--end-of-def-position-bol)))

(ert-deftest py-ert-py--end-of-class-position-bol-functionp-test ()
  (should (functionp 'py--end-of-class-position-bol)))

(ert-deftest py-ert-py--end-of-minor-block-position-bol-functionp-test ()
  (should (functionp 'py--end-of-minor-block-position-bol)))

(ert-deftest py-ert-py--end-of-if-block-position-bol-functionp-test ()
  (should (functionp 'py--end-of-if-block-position-bol)))

(ert-deftest py-ert-py--end-of-try-block-position-bol-functionp-test ()
  (should (functionp 'py--end-of-try-block-position-bol)))

(ert-deftest py-ert-py-kill-block-functionp-test ()
  (should (functionp 'py-kill-block)))

(ert-deftest py-ert-py-kill-clause-functionp-test ()
  (should (functionp 'py-kill-clause)))

(ert-deftest py-ert-py-kill-block-or-clause-functionp-test ()
  (should (functionp 'py-kill-block-or-clause)))

(ert-deftest py-ert-py-kill-def-functionp-test ()
  (should (functionp 'py-kill-def)))

(ert-deftest py-ert-py-kill-class-functionp-test ()
  (should (functionp 'py-kill-class)))

(ert-deftest py-ert-py-kill-def-or-class-functionp-test ()
  (should (functionp 'py-kill-def-or-class)))

(ert-deftest py-ert-py-kill-if-block-functionp-test ()
  (should (functionp 'py-kill-if-block)))

(ert-deftest py-ert-py-kill-try-block-functionp-test ()
  (should (functionp 'py-kill-try-block)))

(ert-deftest py-ert-py-kill-minor-block-functionp-test ()
  (should (functionp 'py-kill-minor-block)))

(ert-deftest py-ert-py-kill-for-block-functionp-test ()
  (should (functionp 'py-kill-for-block)))

(ert-deftest py-ert-py-kill-top-level-functionp-test ()
  (should (functionp 'py-kill-top-level)))

(ert-deftest py-ert-py-kill-statement-functionp-test ()
  (should (functionp 'py-kill-statement)))

(ert-deftest py-ert-py-kill-expression-functionp-test ()
  (should (functionp 'py-kill-expression)))

(ert-deftest py-ert-py-kill-partial-expression-functionp-test ()
  (should (functionp 'py-kill-partial-expression)))

(ert-deftest py-ert-py-kill-block-bol-functionp-test ()
  (should (functionp 'py-kill-block-bol)))

(ert-deftest py-ert-py-kill-clause-bol-functionp-test ()
  (should (functionp 'py-kill-clause-bol)))

(ert-deftest py-ert-py-kill-block-or-clause-bol-functionp-test ()
  (should (functionp 'py-kill-block-or-clause-bol)))

(ert-deftest py-ert-py-kill-def-bol-functionp-test ()
  (should (functionp 'py-kill-def-bol)))

(ert-deftest py-ert-py-kill-class-bol-functionp-test ()
  (should (functionp 'py-kill-class-bol)))

(ert-deftest py-ert-py-kill-def-or-class-bol-functionp-test ()
  (should (functionp 'py-kill-def-or-class-bol)))

(ert-deftest py-ert-py-kill-if-block-bol-functionp-test ()
  (should (functionp 'py-kill-if-block-bol)))

(ert-deftest py-ert-py-kill-try-block-bol-functionp-test ()
  (should (functionp 'py-kill-try-block-bol)))

(ert-deftest py-ert-py-kill-minor-block-bol-functionp-test ()
  (should (functionp 'py-kill-minor-block-bol)))

(ert-deftest py-ert-py-kill-for-block-bol-functionp-test ()
  (should (functionp 'py-kill-for-block-bol)))

(ert-deftest py-ert-py-kill-top-level-bol-functionp-test ()
  (should (functionp 'py-kill-top-level-bol)))

(ert-deftest py-ert-py-kill-statement-bol-functionp-test ()
  (should (functionp 'py-kill-statement-bol)))

(ert-deftest py-ert-py-beginning-of-expression-functionp-test ()
  (should (functionp 'py-beginning-of-expression)))

(ert-deftest py-ert-py--beginning-of-expression-intern-functionp-test ()
  (should (functionp 'py--beginning-of-expression-intern)))

(ert-deftest py-ert-py-end-of-expression-functionp-test ()
  (should (functionp 'py-end-of-expression)))

(ert-deftest py-ert-py--end-of-expression-intern-functionp-test ()
  (should (functionp 'py--end-of-expression-intern)))

(ert-deftest py-ert-py-beginning-of-partial-expression-functionp-test ()
  (should (functionp 'py-beginning-of-partial-expression)))

(ert-deftest py-ert-py-end-of-partial-expression-functionp-test ()
  (should (functionp 'py-end-of-partial-expression)))

(ert-deftest py-ert-py-beginning-of-line-functionp-test ()
  (should (functionp 'py-beginning-of-line)))

(ert-deftest py-ert-py-end-of-line-functionp-test ()
  (should (functionp 'py-end-of-line)))

(ert-deftest py-ert-py-beginning-of-statement-functionp-test ()
  (should (functionp 'py-beginning-of-statement)))

(ert-deftest py-ert-py-beginning-of-statement-bol-functionp-test ()
  (should (functionp 'py-beginning-of-statement-bol)))

(ert-deftest py-ert-py-end-of-statement-functionp-test ()
  (should (functionp 'py-end-of-statement)))

(ert-deftest py-ert-py-end-of-statement-bol-functionp-test ()
  (should (functionp 'py-end-of-statement-bol)))

(ert-deftest py-ert-py-goto-statement-below-functionp-test ()
  (should (functionp 'py-goto-statement-below)))

(ert-deftest py-ert-py-beginning-of-decorator-functionp-test ()
  (should (functionp 'py-beginning-of-decorator)))

(ert-deftest py-ert-py-end-of-decorator-functionp-test ()
  (should (functionp 'py-end-of-decorator)))

(ert-deftest py-ert-py-forward-line-functionp-test ()
  (should (functionp 'py-forward-line)))

(ert-deftest py-ert-py-go-to-beginning-of-comment-functionp-test ()
  (should (functionp 'py-go-to-beginning-of-comment)))

(ert-deftest py-ert-py--go-to-keyword-functionp-test ()
  (should (functionp 'py--go-to-keyword)))

(ert-deftest py-ert-py--clause-lookup-keyword-functionp-test ()
  (should (functionp 'py--clause-lookup-keyword)))

(ert-deftest py-ert-py-leave-comment-or-string-backward-functionp-test ()
  (should (functionp 'py-leave-comment-or-string-backward)))

(ert-deftest py-ert-py-beginning-of-list-pps-functionp-test ()
  (should (functionp 'py-beginning-of-list-pps)))

(ert-deftest py-ert-py-forward-into-nomenclature-functionp-test ()
  (should (functionp 'py-forward-into-nomenclature)))

(ert-deftest py-ert-py-backward-into-nomenclature-functionp-test ()
  (should (functionp 'py-backward-into-nomenclature)))

(ert-deftest py-ert-match-paren-functionp-test ()
  (should (functionp 'match-paren)))

(ert-deftest py-ert-py--travel-current-indent-functionp-test ()
  (should (functionp 'py--travel-current-indent)))

(ert-deftest py-ert-py-beginning-of-block-current-column-functionp-test ()
  (should (functionp 'py-beginning-of-block-current-column)))

(ert-deftest py-ert-py--end-of-block-p-functionp-test ()
  (should (functionp 'py--end-of-block-p)))

(ert-deftest py-ert-py--end-of-clause-p-functionp-test ()
  (should (functionp 'py--end-of-clause-p)))

(ert-deftest py-ert-py--end-of-block-or-clause-p-functionp-test ()
  (should (functionp 'py--end-of-block-or-clause-p)))

(ert-deftest py-ert-py--end-of-def-p-functionp-test ()
  (should (functionp 'py--end-of-def-p)))

(ert-deftest py-ert-py--end-of-class-p-functionp-test ()
  (should (functionp 'py--end-of-class-p)))

(ert-deftest py-ert-py--end-of-def-or-class-p-functionp-test ()
  (should (functionp 'py--end-of-def-or-class-p)))

(ert-deftest py-ert-py--end-of-if-block-p-functionp-test ()
  (should (functionp 'py--end-of-if-block-p)))

(ert-deftest py-ert-py--end-of-try-block-p-functionp-test ()
  (should (functionp 'py--end-of-try-block-p)))

(ert-deftest py-ert-py--end-of-minor-block-p-functionp-test ()
  (should (functionp 'py--end-of-minor-block-p)))

(ert-deftest py-ert-py--end-of-for-block-p-functionp-test ()
  (should (functionp 'py--end-of-for-block-p)))

(ert-deftest py-ert-py--end-of-top-level-p-functionp-test ()
  (should (functionp 'py--end-of-top-level-p)))

(ert-deftest py-ert-py--end-of-statement-p-functionp-test ()
  (should (functionp 'py--end-of-statement-p)))

(ert-deftest py-ert-py--end-of-expression-p-functionp-test ()
  (should (functionp 'py--end-of-expression-p)))

(ert-deftest py-ert-py--end-of-partial-expression-p-functionp-test ()
  (should (functionp 'py--end-of-partial-expression-p)))

(ert-deftest py-ert-py--end-of-block-bol-p-functionp-test ()
  (should (functionp 'py--end-of-block-bol-p)))

(ert-deftest py-ert-py--end-of-clause-bol-p-functionp-test ()
  (should (functionp 'py--end-of-clause-bol-p)))

(ert-deftest py-ert-py--end-of-block-or-clause-bol-p-functionp-test ()
  (should (functionp 'py--end-of-block-or-clause-bol-p)))

(ert-deftest py-ert-py--end-of-def-bol-p-functionp-test ()
  (should (functionp 'py--end-of-def-bol-p)))

(ert-deftest py-ert-py--end-of-class-bol-p-functionp-test ()
  (should (functionp 'py--end-of-class-bol-p)))

(ert-deftest py-ert-py--end-of-def-or-class-bol-p-functionp-test ()
  (should (functionp 'py--end-of-def-or-class-bol-p)))

(ert-deftest py-ert-py--end-of-if-block-bol-p-functionp-test ()
  (should (functionp 'py--end-of-if-block-bol-p)))

(ert-deftest py-ert-py--end-of-try-block-bol-p-functionp-test ()
  (should (functionp 'py--end-of-try-block-bol-p)))

(ert-deftest py-ert-py--end-of-minor-block-bol-p-functionp-test ()
  (should (functionp 'py--end-of-minor-block-bol-p)))

(ert-deftest py-ert-py--end-of-for-block-bol-p-functionp-test ()
  (should (functionp 'py--end-of-for-block-bol-p)))

(ert-deftest py-ert-py--end-of-top-level-bol-p-functionp-test ()
  (should (functionp 'py--end-of-top-level-bol-p)))

(ert-deftest py-ert-py--end-of-statement-bol-p-functionp-test ()
  (should (functionp 'py--end-of-statement-bol-p)))

(ert-deftest py-ert-py--fast-completion-get-completions-functionp-test ()
  (should (functionp 'py--fast-completion-get-completions)))

(ert-deftest py-ert-py--fast--do-completion-at-point-functionp-test ()
  (should (functionp 'py--fast--do-completion-at-point)))

(ert-deftest py-ert-py--fast-complete-base-functionp-test ()
  (should (functionp 'py--fast-complete-base)))

(ert-deftest py-ert-py-fast-complete-functionp-test ()
  (should (functionp 'py-fast-complete)))

(ert-deftest py-ert-py--end-of-block-p-functionp-test ()
  (should (functionp 'py--end-of-block-p)))

(ert-deftest py-ert-py--end-of-clause-p-functionp-test ()
  (should (functionp 'py--end-of-clause-p)))

(ert-deftest py-ert-py--end-of-block-or-clause-p-functionp-test ()
  (should (functionp 'py--end-of-block-or-clause-p)))

(ert-deftest py-ert-py--end-of-def-p-functionp-test ()
  (should (functionp 'py--end-of-def-p)))

(ert-deftest py-ert-py--end-of-class-p-functionp-test ()
  (should (functionp 'py--end-of-class-p)))

(ert-deftest py-ert-py--end-of-def-or-class-p-functionp-test ()
  (should (functionp 'py--end-of-def-or-class-p)))

(ert-deftest py-ert-py--end-of-if-block-p-functionp-test ()
  (should (functionp 'py--end-of-if-block-p)))

(ert-deftest py-ert-py--end-of-try-block-p-functionp-test ()
  (should (functionp 'py--end-of-try-block-p)))

(ert-deftest py-ert-py--end-of-minor-block-p-functionp-test ()
  (should (functionp 'py--end-of-minor-block-p)))

(ert-deftest py-ert-py--end-of-for-block-p-functionp-test ()
  (should (functionp 'py--end-of-for-block-p)))

(ert-deftest py-ert-py--end-of-top-level-p-functionp-test ()
  (should (functionp 'py--end-of-top-level-p)))

(ert-deftest py-ert-py--end-of-statement-p-functionp-test ()
  (should (functionp 'py--end-of-statement-p)))

(ert-deftest py-ert-py--end-of-expression-p-functionp-test ()
  (should (functionp 'py--end-of-expression-p)))

(ert-deftest py-ert-py--end-of-partial-expression-p-functionp-test ()
  (should (functionp 'py--end-of-partial-expression-p)))

(ert-deftest py-ert-py--end-of-block-bol-p-functionp-test ()
  (should (functionp 'py--end-of-block-bol-p)))

(ert-deftest py-ert-py--end-of-clause-bol-p-functionp-test ()
  (should (functionp 'py--end-of-clause-bol-p)))

(ert-deftest py-ert-py--end-of-block-or-clause-bol-p-functionp-test ()
  (should (functionp 'py--end-of-block-or-clause-bol-p)))

(ert-deftest py-ert-py--end-of-def-bol-p-functionp-test ()
  (should (functionp 'py--end-of-def-bol-p)))

(ert-deftest py-ert-py--end-of-class-bol-p-functionp-test ()
  (should (functionp 'py--end-of-class-bol-p)))

(ert-deftest py-ert-py--end-of-def-or-class-bol-p-functionp-test ()
  (should (functionp 'py--end-of-def-or-class-bol-p)))

(ert-deftest py-ert-py--end-of-if-block-bol-p-functionp-test ()
  (should (functionp 'py--end-of-if-block-bol-p)))

(ert-deftest py-ert-py--end-of-try-block-bol-p-functionp-test ()
  (should (functionp 'py--end-of-try-block-bol-p)))

(ert-deftest py-ert-py--end-of-minor-block-bol-p-functionp-test ()
  (should (functionp 'py--end-of-minor-block-bol-p)))

(ert-deftest py-ert-py--end-of-for-block-bol-p-functionp-test ()
  (should (functionp 'py--end-of-for-block-bol-p)))

(ert-deftest py-ert-py--end-of-top-level-bol-p-functionp-test ()
  (should (functionp 'py--end-of-top-level-bol-p)))

(ert-deftest py-ert-py--end-of-statement-bol-p-functionp-test ()
  (should (functionp 'py--end-of-statement-bol-p)))

(ert-deftest py-ert-py--all-shell-mode-setting-functionp-test ()
  (should (functionp 'py--all-shell-mode-setting)))

(ert-deftest py-ert-py-fast-process-functionp-test ()
  (should (functionp 'py-fast-process)))

(ert-deftest py-ert-py--filter-result-functionp-test ()
  (should (functionp 'py--filter-result)))

(ert-deftest py-ert-py--fast-send-string-no-output-functionp-test ()
  (should (functionp 'py--fast-send-string-no-output)))

(ert-deftest py-ert-py--fast-send-string-intern-functionp-test ()
  (should (functionp 'py--fast-send-string-intern)))

(ert-deftest py-ert-py--fast-send-string-functionp-test ()
  (should (functionp 'py--fast-send-string)))

(ert-deftest py-ert-py-fast-send-string-functionp-test ()
  (should (functionp 'py-fast-send-string)))

(ert-deftest py-ert-py-process-region-fast-functionp-test ()
  (should (functionp 'py-process-region-fast)))

(ert-deftest py-ert-py-execute-statement-fast-functionp-test ()
  (should (functionp 'py-execute-statement-fast)))

(ert-deftest py-ert-py-execute-block-fast-functionp-test ()
  (should (functionp 'py-execute-block-fast)))

(ert-deftest py-ert-py-execute-block-or-clause-fast-functionp-test ()
  (should (functionp 'py-execute-block-or-clause-fast)))

(ert-deftest py-ert-py-execute-def-fast-functionp-test ()
  (should (functionp 'py-execute-def-fast)))

(ert-deftest py-ert-py-execute-class-fast-functionp-test ()
  (should (functionp 'py-execute-class-fast)))

(ert-deftest py-ert-py-execute-def-or-class-fast-functionp-test ()
  (should (functionp 'py-execute-def-or-class-fast)))

(ert-deftest py-ert-py-execute-expression-fast-functionp-test ()
  (should (functionp 'py-execute-expression-fast)))

(ert-deftest py-ert-py-execute-partial-expression-fast-functionp-test ()
  (should (functionp 'py-execute-partial-expression-fast)))

(ert-deftest py-ert-py-execute-top-level-fast-functionp-test ()
  (should (functionp 'py-execute-top-level-fast)))

(ert-deftest py-ert-py-execute-clause-fast-functionp-test ()
  (should (functionp 'py-execute-clause-fast)))

(ert-deftest py-ert-py-restore-window-configuration-functionp-test ()
  (should (functionp 'py-restore-window-configuration)))

(ert-deftest py-ert-py-shell-execute-string-now-functionp-test ()
  (should (functionp 'py-shell-execute-string-now)))

(ert-deftest py-ert-py-switch-to-python-functionp-test ()
  (should (functionp 'py-switch-to-python)))

(ert-deftest py-ert-py-send-file-functionp-test ()
  (should (functionp 'py-send-file)))

(ert-deftest py-ert-toggle-force-local-shell-functionp-test ()
  (should (functionp 'toggle-force-local-shell)))

(ert-deftest py-ert-py-force-local-shell-on-functionp-test ()
  (should (functionp 'py-force-local-shell-on)))

(ert-deftest py-ert-py-force-local-shell-off-functionp-test ()
  (should (functionp 'py-force-local-shell-off)))

(ert-deftest py-ert-toggle-force-py-shell-name-p-functionp-test ()
  (should (functionp 'toggle-force-py-shell-name-p)))

(ert-deftest py-ert-force-py-shell-name-p-on-functionp-test ()
  (should (functionp 'force-py-shell-name-p-on)))

(ert-deftest py-ert-force-py-shell-name-p-off-functionp-test ()
  (should (functionp 'force-py-shell-name-p-off)))

(ert-deftest py-ert-py-toggle-split-windows-on-execute-functionp-test ()
  (should (functionp 'py-toggle-split-windows-on-execute)))

(ert-deftest py-ert-py-split-windows-on-execute-on-functionp-test ()
  (should (functionp 'py-split-windows-on-execute-on)))

(ert-deftest py-ert-py-split-windows-on-execute-off-functionp-test ()
  (should (functionp 'py-split-windows-on-execute-off)))

(ert-deftest py-ert-py-toggle-shell-switch-buffers-on-execute-functionp-test ()
  (should (functionp 'py-toggle-shell-switch-buffers-on-execute)))

(ert-deftest py-ert-py-shell-switch-buffers-on-execute-on-functionp-test ()
  (should (functionp 'py-shell-switch-buffers-on-execute-on)))

(ert-deftest py-ert-py-shell-switch-buffers-on-execute-off-functionp-test ()
  (should (functionp 'py-shell-switch-buffers-on-execute-off)))

(ert-deftest py-ert-py-guess-default-python-functionp-test ()
  (should (functionp 'py-guess-default-python)))

(ert-deftest py-ert-py-dirstack-hook-functionp-test ()
  (should (functionp 'py-dirstack-hook)))

(ert-deftest py-ert-py-shell-dedicated-functionp-test ()
  (should (functionp 'py-shell-dedicated)))

(ert-deftest py-ert-py-set-ipython-completion-command-string-functionp-test ()
  (should (functionp 'py-set-ipython-completion-command-string)))

(ert-deftest py-ert-py-ipython--module-completion-import-functionp-test ()
  (should (functionp 'py-ipython--module-completion-import)))

(ert-deftest py-ert-py--compose-buffer-name-initials-functionp-test ()
  (should (functionp 'py--compose-buffer-name-initials)))

(ert-deftest py-ert-py--remove-home-directory-from-list-functionp-test ()
  (should (functionp 'py--remove-home-directory-from-list)))

(ert-deftest py-ert-py--choose-buffer-name-functionp-test ()
  (should (functionp 'py--choose-buffer-name)))

(ert-deftest py-ert-py--jump-to-exception-intern-functionp-test ()
  (should (functionp 'py--jump-to-exception-intern)))

(ert-deftest py-ert-py--jump-to-exception-functionp-test ()
  (should (functionp 'py--jump-to-exception)))

(ert-deftest py-ert-py-toggle-split-window-function-functionp-test ()
  (should (functionp 'py-toggle-split-window-function)))

(ert-deftest py-ert-py--manage-windows-set-and-switch-functionp-test ()
  (should (functionp 'py--manage-windows-set-and-switch)))

(ert-deftest py-ert-py--alternative-split-windows-on-execute-function-functionp-test ()
  (should (functionp 'py--alternative-split-windows-on-execute-function)))

(ert-deftest py-ert-py--get-splittable-window-functionp-test ()
  (should (functionp 'py--get-splittable-window)))

(ert-deftest py-ert-py--manage-windows-split-functionp-test ()
  (should (functionp 'py--manage-windows-split)))

(ert-deftest py-ert-py--display-windows-functionp-test ()
  (should (functionp 'py--display-windows)))

(ert-deftest py-ert-py--shell-manage-windows-functionp-test ()
  (should (functionp 'py--shell-manage-windows)))

(ert-deftest py-ert-py-kill-shell-unconditional-functionp-test ()
  (should (functionp 'py-kill-shell-unconditional)))

(ert-deftest py-ert-py-kill-default-shell-unconditional-functionp-test ()
  (should (functionp 'py-kill-default-shell-unconditional)))

(ert-deftest py-ert-py--report-executable-functionp-test ()
  (should (functionp 'py--report-executable)))

(ert-deftest py-ert-py--shell-make-comint-functionp-test ()
  (should (functionp 'py--shell-make-comint)))

(ert-deftest py-ert-py--guess-buffer-name-functionp-test ()
  (should (functionp 'py--guess-buffer-name)))

(ert-deftest py-ert-py--configured-shell-functionp-test ()
  (should (functionp 'py--configured-shell)))

(ert-deftest py-ert-py--grab-prompt-ps1-functionp-test ()
  (should (functionp 'py--grab-prompt-ps1)))

(ert-deftest py-ert-py--start-fast-process-functionp-test ()
  (should (functionp 'py--start-fast-process)))

(ert-deftest py-ert-py-shell-functionp-test ()
  (should (functionp 'py-shell)))

(ert-deftest py-ert-py-shell-get-process-functionp-test ()
  (should (functionp 'py-shell-get-process)))

(ert-deftest py-ert-py-switch-to-shell-functionp-test ()
  (should (functionp 'py-switch-to-shell)))

(ert-deftest py-ert-py-which-execute-file-command-functionp-test ()
  (should (functionp 'py-which-execute-file-command)))

(ert-deftest py-ert-py--store-result-maybe-functionp-test ()
  (should (functionp 'py--store-result-maybe)))

(ert-deftest py-ert-py--close-execution-functionp-test ()
  (should (functionp 'py--close-execution)))

(ert-deftest py-ert-py--execute-base-functionp-test ()
  (should (functionp 'py--execute-base)))

(ert-deftest py-ert-py--send-to-fast-process-functionp-test ()
  (should (functionp 'py--send-to-fast-process)))

(ert-deftest py-ert-py--execute-base-intern-functionp-test ()
  (should (functionp 'py--execute-base-intern)))

(ert-deftest py-ert-py--execute-buffer-finally-functionp-test ()
  (should (functionp 'py--execute-buffer-finally)))

(ert-deftest py-ert-py--fetch-error-functionp-test ()
  (should (functionp 'py--fetch-error)))

(ert-deftest py-ert-py--fetch-result-functionp-test ()
  (should (functionp 'py--fetch-result)))

(ert-deftest py-ert-py--postprocess-comint-functionp-test ()
  (should (functionp 'py--postprocess-comint)))

(ert-deftest py-ert-py--execute-ge24.3-functionp-test ()
  (should (functionp 'py--execute-ge24.3)))

(ert-deftest py-ert-py-delete-temporary-functionp-test ()
  (should (functionp 'py-delete-temporary)))

(ert-deftest py-ert-py-execute-python-mode-v5-functionp-test ()
  (should (functionp 'py-execute-python-mode-v5)))

(ert-deftest py-ert-py--insert-offset-lines-functionp-test ()
  (should (functionp 'py--insert-offset-lines)))

(ert-deftest py-ert-py--execute-file-base-functionp-test ()
  (should (functionp 'py--execute-file-base)))

(ert-deftest py-ert-py-execute-file-functionp-test ()
  (should (functionp 'py-execute-file)))

(ert-deftest py-ert-py--current-working-directory-functionp-test ()
  (should (functionp 'py--current-working-directory)))

(ert-deftest py-ert-py--update-execute-directory-intern-functionp-test ()
  (should (functionp 'py--update-execute-directory-intern)))

(ert-deftest py-ert-py--update-execute-directory-functionp-test ()
  (should (functionp 'py--update-execute-directory)))

(ert-deftest py-ert-py-execute-string-functionp-test ()
  (should (functionp 'py-execute-string)))

(ert-deftest py-ert-py-execute-string-dedicated-functionp-test ()
  (should (functionp 'py-execute-string-dedicated)))

(ert-deftest py-ert-py--insert-execute-directory-functionp-test ()
  (should (functionp 'py--insert-execute-directory)))

(ert-deftest py-ert-py--fix-if-name-main-permission-functionp-test ()
  (should (functionp 'py--fix-if-name-main-permission)))

(ert-deftest py-ert-py--fix-start-functionp-test ()
  (should (functionp 'py--fix-start)))

(ert-deftest py-ert-py-fetch-py-master-file-functionp-test ()
  (should (functionp 'py-fetch-py-master-file)))

(ert-deftest py-ert-py-execute-import-or-reload-functionp-test ()
  (should (functionp 'py-execute-import-or-reload)))

(ert-deftest py-ert-py--qualified-module-name-functionp-test ()
  (should (functionp 'py--qualified-module-name)))

(ert-deftest py-ert-py-execute-buffer-functionp-test ()
  (should (functionp 'py-execute-buffer)))

(ert-deftest py-ert-py--execute-buffer-base-functionp-test ()
  (should (functionp 'py--execute-buffer-base)))

(ert-deftest py-ert-py-execute-buffer-dedicated-functionp-test ()
  (should (functionp 'py-execute-buffer-dedicated)))

(ert-deftest py-ert-py-execute-buffer-switch-functionp-test ()
  (should (functionp 'py-execute-buffer-switch)))

(ert-deftest py-ert-py-execute-buffer-no-switch-functionp-test ()
  (should (functionp 'py-execute-buffer-no-switch)))

(ert-deftest py-ert-py-execute-buffer-dedicated-switch-functionp-test ()
  (should (functionp 'py-execute-buffer-dedicated-switch)))

(ert-deftest py-ert-py-execute-region-python-functionp-test ()
  (should (functionp 'py-execute-region-python)))

(ert-deftest py-ert-py-execute-region-python-switch-functionp-test ()
  (should (functionp 'py-execute-region-python-switch)))

(ert-deftest py-ert-py-execute-region-python-no-switch-functionp-test ()
  (should (functionp 'py-execute-region-python-no-switch)))

(ert-deftest py-ert-py-execute-region-python2-functionp-test ()
  (should (functionp 'py-execute-region-python2)))

(ert-deftest py-ert-py-execute-region-python2-switch-functionp-test ()
  (should (functionp 'py-execute-region-python2-switch)))

(ert-deftest py-ert-py-execute-region-python2-no-switch-functionp-test ()
  (should (functionp 'py-execute-region-python2-no-switch)))

(ert-deftest py-ert-py-execute-region-python3-functionp-test ()
  (should (functionp 'py-execute-region-python3)))

(ert-deftest py-ert-py-execute-region-python3-switch-functionp-test ()
  (should (functionp 'py-execute-region-python3-switch)))

(ert-deftest py-ert-py-execute-region-python3-no-switch-functionp-test ()
  (should (functionp 'py-execute-region-python3-no-switch)))

(ert-deftest py-ert-py-execute-region-ipython-functionp-test ()
  (should (functionp 'py-execute-region-ipython)))

(ert-deftest py-ert-py-execute-region-ipython-switch-functionp-test ()
  (should (functionp 'py-execute-region-ipython-switch)))

(ert-deftest py-ert-py-execute-region-ipython-no-switch-functionp-test ()
  (should (functionp 'py-execute-region-ipython-no-switch)))

(ert-deftest py-ert-py-execute-region-jython-functionp-test ()
  (should (functionp 'py-execute-region-jython)))

(ert-deftest py-ert-py-execute-region-jython-switch-functionp-test ()
  (should (functionp 'py-execute-region-jython-switch)))

(ert-deftest py-ert-py-execute-region-jython-no-switch-functionp-test ()
  (should (functionp 'py-execute-region-jython-no-switch)))

(ert-deftest py-ert-py-execute-defun-functionp-test ()
  (should (functionp 'py-execute-defun)))

(ert-deftest py-ert-py-process-file-functionp-test ()
  (should (functionp 'py-process-file)))

(ert-deftest py-ert-py-execute-line-functionp-test ()
  (should (functionp 'py-execute-line)))

(ert-deftest py-ert-py-remove-overlays-at-point-functionp-test ()
  (should (functionp 'py-remove-overlays-at-point)))

(ert-deftest py-ert-py-mouseto-exception-functionp-test ()
  (should (functionp 'py-mouseto-exception)))

(ert-deftest py-ert-py-goto-exception-functionp-test ()
  (should (functionp 'py-goto-exception)))

(ert-deftest py-ert-py--find-next-exception-functionp-test ()
  (should (functionp 'py--find-next-exception)))

(ert-deftest py-ert-py-down-exception-functionp-test ()
  (should (functionp 'py-down-exception)))

(ert-deftest py-ert-py-up-exception-functionp-test ()
  (should (functionp 'py-up-exception)))

(ert-deftest py-ert-py--postprocess-intern-functionp-test ()
  (should (functionp 'py--postprocess-intern)))

(ert-deftest py-ert-py--find-next-exception-prepare-functionp-test ()
  (should (functionp 'py--find-next-exception-prepare)))

(ert-deftest py-ert-python-functionp-test ()
  (should (functionp 'python)))

(ert-deftest py-ert-ipython-functionp-test ()
  (should (functionp 'ipython)))

(ert-deftest py-ert-ipython-functionp-test ()
  (should (functionp 'ipython3)))

(ert-deftest py-ert-python2-functionp-test ()
  (should (functionp 'python2)))

(ert-deftest py-ert-jython-functionp-test ()
  (should (functionp 'jython)))

(ert-deftest py-ert-python3-functionp-test ()
  (should (functionp 'python3)))

(ert-deftest py-ert-python-dedicated-functionp-test ()
  (should (functionp 'python-dedicated)))

(ert-deftest py-ert-ipython-dedicated-functionp-test ()
  (should (functionp 'ipython-dedicated)))

(ert-deftest py-ert-ipython-dedicated-functionp-test ()
  (should (functionp 'ipython3-dedicated)))

(ert-deftest py-ert-python2-dedicated-functionp-test ()
  (should (functionp 'python2-dedicated)))

(ert-deftest py-ert-jython-dedicated-functionp-test ()
  (should (functionp 'jython-dedicated)))

(ert-deftest py-ert-python3-dedicated-functionp-test ()
  (should (functionp 'python3-dedicated)))

(ert-deftest py-ert-python-switch-functionp-test ()
  (should (functionp 'python-switch)))

(ert-deftest py-ert-ipython-switch-functionp-test ()
  (should (functionp 'ipython-switch)))

(ert-deftest py-ert-python2-switch-functionp-test ()
  (should (functionp 'python2-switch)))

(ert-deftest py-ert-jython-switch-functionp-test ()
  (should (functionp 'jython-switch)))

(ert-deftest py-ert-python3-switch-functionp-test ()
  (should (functionp 'python3-switch)))

(ert-deftest py-ert-python-no-switch-functionp-test ()
  (should (functionp 'python-no-switch)))

(ert-deftest py-ert-ipython-no-switch-functionp-test ()
  (should (functionp 'ipython-no-switch)))

(ert-deftest py-ert-python2-no-switch-functionp-test ()
  (should (functionp 'python2-no-switch)))

(ert-deftest py-ert-jython-no-switch-functionp-test ()
  (should (functionp 'jython-no-switch)))

(ert-deftest py-ert-python3-no-switch-functionp-test ()
  (should (functionp 'python3-no-switch)))

(ert-deftest py-ert-python-switch-dedicated-functionp-test ()
  (should (functionp 'python-switch-dedicated)))

(ert-deftest py-ert-ipython-switch-dedicated-functionp-test ()
  (should (functionp 'ipython-switch-dedicated)))

(ert-deftest py-ert-python2-switch-dedicated-functionp-test ()
  (should (functionp 'python2-switch-dedicated)))

(ert-deftest py-ert-jython-switch-dedicated-functionp-test ()
  (should (functionp 'jython-switch-dedicated)))

(ert-deftest py-ert-python3-switch-dedicated-functionp-test ()
  (should (functionp 'python3-switch-dedicated)))

(ert-deftest py-ert-py-hide-base-functionp-test ()
  (should (functionp 'py-hide-base)))

(ert-deftest py-ert-py-show-base-functionp-test ()
  (should (functionp 'py-show-base)))

(ert-deftest py-ert-py-hide-show-functionp-test ()
  (should (functionp 'py-hide-show)))

(ert-deftest py-ert-py-hide-region-functionp-test ()
  (should (functionp 'py-hide-region)))

(ert-deftest py-ert-py-show-region-functionp-test ()
  (should (functionp 'py-show-region)))

(ert-deftest py-ert-py-hide-statement-functionp-test ()
  (should (functionp 'py-hide-statement)))

(ert-deftest py-ert-py-show-statement-functionp-test ()
  (should (functionp 'py-show-statement)))

(ert-deftest py-ert-py-hide-block-functionp-test ()
  (should (functionp 'py-hide-block)))

(ert-deftest py-ert-py-show-block-functionp-test ()
  (should (functionp 'py-show-block)))

(ert-deftest py-ert-py-hide-clause-functionp-test ()
  (should (functionp 'py-hide-clause)))

(ert-deftest py-ert-py-show-clause-functionp-test ()
  (should (functionp 'py-show-clause)))

(ert-deftest py-ert-py-hide-block-or-clause-functionp-test ()
  (should (functionp 'py-hide-block-or-clause)))

(ert-deftest py-ert-py-show-block-or-clause-functionp-test ()
  (should (functionp 'py-show-block-or-clause)))

(ert-deftest py-ert-py-hide-def-functionp-test ()
  (should (functionp 'py-hide-def)))

(ert-deftest py-ert-py-show-def-functionp-test ()
  (should (functionp 'py-show-def)))

(ert-deftest py-ert-py-hide-class-functionp-test ()
  (should (functionp 'py-hide-class)))

(ert-deftest py-ert-py-show-class-functionp-test ()
  (should (functionp 'py-show-class)))

(ert-deftest py-ert-py-hide-expression-functionp-test ()
  (should (functionp 'py-hide-expression)))

(ert-deftest py-ert-py-show-expression-functionp-test ()
  (should (functionp 'py-show-expression)))

(ert-deftest py-ert-py-hide-partial-expression-functionp-test ()
  (should (functionp 'py-hide-partial-expression)))

(ert-deftest py-ert-py-show-partial-expression-functionp-test ()
  (should (functionp 'py-show-partial-expression)))

(ert-deftest py-ert-py-hide-line-functionp-test ()
  (should (functionp 'py-hide-line)))

(ert-deftest py-ert-py-show-line-functionp-test ()
  (should (functionp 'py-show-line)))

(ert-deftest py-ert-py-hide-top-level-functionp-test ()
  (should (functionp 'py-hide-top-level)))

(ert-deftest py-ert-py-show-top-level-functionp-test ()
  (should (functionp 'py-show-top-level)))

(ert-deftest py-ert-py-copy-statement-functionp-test ()
  (should (functionp 'py-copy-statement)))

(ert-deftest py-ert-py-copy-statement-bol-functionp-test ()
  (should (functionp 'py-copy-statement-bol)))

(ert-deftest py-ert-py-copy-top-level-functionp-test ()
  (should (functionp 'py-copy-top-level)))

(ert-deftest py-ert-py-copy-top-level-bol-functionp-test ()
  (should (functionp 'py-copy-top-level-bol)))

(ert-deftest py-ert-py-copy-block-functionp-test ()
  (should (functionp 'py-copy-block)))

(ert-deftest py-ert-py-copy-block-bol-functionp-test ()
  (should (functionp 'py-copy-block-bol)))

(ert-deftest py-ert-py-copy-clause-functionp-test ()
  (should (functionp 'py-copy-clause)))

(ert-deftest py-ert-py-copy-clause-bol-functionp-test ()
  (should (functionp 'py-copy-clause-bol)))

(ert-deftest py-ert-py-copy-block-or-clause-functionp-test ()
  (should (functionp 'py-copy-block-or-clause)))

(ert-deftest py-ert-py-copy-block-or-clause-bol-functionp-test ()
  (should (functionp 'py-copy-block-or-clause-bol)))

(ert-deftest py-ert-py-copy-def-functionp-test ()
  (should (functionp 'py-copy-def)))

(ert-deftest py-ert-py-copy-def-bol-functionp-test ()
  (should (functionp 'py-copy-def-bol)))

(ert-deftest py-ert-py-copy-class-functionp-test ()
  (should (functionp 'py-copy-class)))

(ert-deftest py-ert-py-copy-class-bol-functionp-test ()
  (should (functionp 'py-copy-class-bol)))

(ert-deftest py-ert-py-copy-def-or-class-functionp-test ()
  (should (functionp 'py-copy-def-or-class)))

(ert-deftest py-ert-py-copy-def-or-class-bol-functionp-test ()
  (should (functionp 'py-copy-def-or-class-bol)))

(ert-deftest py-ert-py-copy-expression-functionp-test ()
  (should (functionp 'py-copy-expression)))

(ert-deftest py-ert-py-copy-expression-bol-functionp-test ()
  (should (functionp 'py-copy-expression-bol)))

(ert-deftest py-ert-py-copy-partial-expression-functionp-test ()
  (should (functionp 'py-copy-partial-expression)))

(ert-deftest py-ert-py-copy-partial-expression-bol-functionp-test ()
  (should (functionp 'py-copy-partial-expression-bol)))

(ert-deftest py-ert-py-copy-minor-block-functionp-test ()
  (should (functionp 'py-copy-minor-block)))

(ert-deftest py-ert-py-copy-minor-block-bol-functionp-test ()
  (should (functionp 'py-copy-minor-block-bol)))

(ert-deftest py-ert-py-copy-statement-functionp-test ()
  (should (functionp 'py-copy-statement)))

(ert-deftest py-ert-py-copy-top-level-functionp-test ()
  (should (functionp 'py-copy-top-level)))

(ert-deftest py-ert-py-copy-block-functionp-test ()
  (should (functionp 'py-copy-block)))

(ert-deftest py-ert-py-copy-clause-functionp-test ()
  (should (functionp 'py-copy-clause)))

(ert-deftest py-ert-py-copy-block-or-clause-functionp-test ()
  (should (functionp 'py-copy-block-or-clause)))

(ert-deftest py-ert-py-copy-def-functionp-test ()
  (should (functionp 'py-copy-def)))

(ert-deftest py-ert-py-copy-class-functionp-test ()
  (should (functionp 'py-copy-class)))

(ert-deftest py-ert-py-copy-def-or-class-functionp-test ()
  (should (functionp 'py-copy-def-or-class)))

(ert-deftest py-ert-py-copy-expression-functionp-test ()
  (should (functionp 'py-copy-expression)))

(ert-deftest py-ert-py-copy-partial-expression-functionp-test ()
  (should (functionp 'py-copy-partial-expression)))

(ert-deftest py-ert-py-copy-minor-block-functionp-test ()
  (should (functionp 'py-copy-minor-block)))

(ert-deftest py-ert-py-statement-functionp-test ()
  (should (functionp 'py-statement)))

(ert-deftest py-ert-py-top-level-functionp-test ()
  (should (functionp 'py-top-level)))

(ert-deftest py-ert-py-block-functionp-test ()
  (should (functionp 'py-block)))

(ert-deftest py-ert-py-clause-functionp-test ()
  (should (functionp 'py-clause)))

(ert-deftest py-ert-py-block-or-clause-functionp-test ()
  (should (functionp 'py-block-or-clause)))

(ert-deftest py-ert-py-def-functionp-test ()
  (should (functionp 'py-def)))

(ert-deftest py-ert-py-class-functionp-test ()
  (should (functionp 'py-class)))

(ert-deftest py-ert-py-def-or-class-functionp-test ()
  (should (functionp 'py-def-or-class)))

(ert-deftest py-ert-py-expression-functionp-test ()
  (should (functionp 'py-expression)))

(ert-deftest py-ert-py-partial-expression-functionp-test ()
  (should (functionp 'py-partial-expression)))

(ert-deftest py-ert-py-minor-block-functionp-test ()
  (should (functionp 'py-minor-block)))

(ert-deftest py-ert-py-output-buffer-filter-functionp-test ()
  (should (functionp 'py-output-buffer-filter)))

(ert-deftest py-ert-py-output-filter-functionp-test ()
  (should (functionp 'py-output-filter)))

(ert-deftest py-ert-py-send-string-functionp-test ()
  (should (functionp 'py-send-string)))

(ert-deftest py-ert-py-copy-statement-functionp-test ()
  (should (functionp 'py-copy-statement)))

(ert-deftest py-ert-py-copy-statement-bol-functionp-test ()
  (should (functionp 'py-copy-statement-bol)))

(ert-deftest py-ert-py-copy-top-level-functionp-test ()
  (should (functionp 'py-copy-top-level)))

(ert-deftest py-ert-py-copy-top-level-bol-functionp-test ()
  (should (functionp 'py-copy-top-level-bol)))

(ert-deftest py-ert-py-copy-block-functionp-test ()
  (should (functionp 'py-copy-block)))

(ert-deftest py-ert-py-copy-block-bol-functionp-test ()
  (should (functionp 'py-copy-block-bol)))

(ert-deftest py-ert-py-copy-clause-functionp-test ()
  (should (functionp 'py-copy-clause)))

(ert-deftest py-ert-py-copy-clause-bol-functionp-test ()
  (should (functionp 'py-copy-clause-bol)))

(ert-deftest py-ert-py-copy-block-or-clause-functionp-test ()
  (should (functionp 'py-copy-block-or-clause)))

(ert-deftest py-ert-py-copy-block-or-clause-bol-functionp-test ()
  (should (functionp 'py-copy-block-or-clause-bol)))

(ert-deftest py-ert-py-copy-def-functionp-test ()
  (should (functionp 'py-copy-def)))

(ert-deftest py-ert-py-copy-def-bol-functionp-test ()
  (should (functionp 'py-copy-def-bol)))

(ert-deftest py-ert-py-copy-class-functionp-test ()
  (should (functionp 'py-copy-class)))

(ert-deftest py-ert-py-copy-class-bol-functionp-test ()
  (should (functionp 'py-copy-class-bol)))

(ert-deftest py-ert-py-copy-def-or-class-functionp-test ()
  (should (functionp 'py-copy-def-or-class)))

(ert-deftest py-ert-py-copy-def-or-class-bol-functionp-test ()
  (should (functionp 'py-copy-def-or-class-bol)))

(ert-deftest py-ert-py-copy-expression-functionp-test ()
  (should (functionp 'py-copy-expression)))

(ert-deftest py-ert-py-copy-expression-bol-functionp-test ()
  (should (functionp 'py-copy-expression-bol)))

(ert-deftest py-ert-py-copy-partial-expression-functionp-test ()
  (should (functionp 'py-copy-partial-expression)))

(ert-deftest py-ert-py-copy-partial-expression-bol-functionp-test ()
  (should (functionp 'py-copy-partial-expression-bol)))

(ert-deftest py-ert-py-copy-minor-block-functionp-test ()
  (should (functionp 'py-copy-minor-block)))

(ert-deftest py-ert-py-copy-minor-block-bol-functionp-test ()
  (should (functionp 'py-copy-minor-block-bol)))

(ert-deftest py-ert-py-delete-statement-functionp-test ()
  (should (functionp 'py-delete-statement)))

(ert-deftest py-ert-py-delete-top-level-functionp-test ()
  (should (functionp 'py-delete-top-level)))

(ert-deftest py-ert-py-delete-block-functionp-test ()
  (should (functionp 'py-delete-block)))

(ert-deftest py-ert-py-delete-clause-functionp-test ()
  (should (functionp 'py-delete-clause)))

(ert-deftest py-ert-py-delete-block-or-clause-functionp-test ()
  (should (functionp 'py-delete-block-or-clause)))

(ert-deftest py-ert-py-delete-def-functionp-test ()
  (should (functionp 'py-delete-def)))

(ert-deftest py-ert-py-delete-class-functionp-test ()
  (should (functionp 'py-delete-class)))

(ert-deftest py-ert-py-delete-def-or-class-functionp-test ()
  (should (functionp 'py-delete-def-or-class)))

(ert-deftest py-ert-py-delete-expression-functionp-test ()
  (should (functionp 'py-delete-expression)))

(ert-deftest py-ert-py-delete-partial-expression-functionp-test ()
  (should (functionp 'py-delete-partial-expression)))

(ert-deftest py-ert-py-delete-minor-block-functionp-test ()
  (should (functionp 'py-delete-minor-block)))

(ert-deftest py-ert-py-delete-paragraph-bol-functionp-test ()
  (should (functionp 'py-delete-paragraph-bol)))

(ert-deftest py-ert-py-delete-block-bol-functionp-test ()
  (should (functionp 'py-delete-block-bol)))

(ert-deftest py-ert-py-delete-minor-block-bol-functionp-test ()
  (should (functionp 'py-delete-minor-block-bol)))

(ert-deftest py-ert-py-delete-clause-bol-functionp-test ()
  (should (functionp 'py-delete-clause-bol)))

(ert-deftest py-ert-py-delete-block-or-clause-bol-functionp-test ()
  (should (functionp 'py-delete-block-or-clause-bol)))

(ert-deftest py-ert-py-delete-def-bol-functionp-test ()
  (should (functionp 'py-delete-def-bol)))

(ert-deftest py-ert-py-delete-class-bol-functionp-test ()
  (should (functionp 'py-delete-class-bol)))

(ert-deftest py-ert-py-delete-def-or-class-bol-functionp-test ()
  (should (functionp 'py-delete-def-or-class-bol)))

(ert-deftest py-ert-py-delete-statement-bol-functionp-test ()
  (should (functionp 'py-delete-statement-bol)))

(ert-deftest py-ert-py-delete-statement-functionp-test ()
  (should (functionp 'py-delete-statement)))

(ert-deftest py-ert-py-delete-top-level-functionp-test ()
  (should (functionp 'py-delete-top-level)))

(ert-deftest py-ert-py-delete-block-functionp-test ()
  (should (functionp 'py-delete-block)))

(ert-deftest py-ert-py-delete-block-or-clause-functionp-test ()
  (should (functionp 'py-delete-block-or-clause)))

(ert-deftest py-ert-py-delete-def-functionp-test ()
  (should (functionp 'py-delete-def)))

(ert-deftest py-ert-py-delete-class-functionp-test ()
  (should (functionp 'py-delete-class)))

(ert-deftest py-ert-py-delete-def-or-class-functionp-test ()
  (should (functionp 'py-delete-def-or-class)))

(ert-deftest py-ert-py-delete-expression-functionp-test ()
  (should (functionp 'py-delete-expression)))

(ert-deftest py-ert-py-delete-partial-expression-functionp-test ()
  (should (functionp 'py-delete-partial-expression)))

(ert-deftest py-ert-py-delete-minor-block-functionp-test ()
  (should (functionp 'py-delete-minor-block)))

(ert-deftest py-ert-py--beginning-of-statement-position-functionp-test ()
  (should (functionp 'py--beginning-of-statement-position)))

(ert-deftest py-ert-py--beginning-of-block-position-functionp-test ()
  (should (functionp 'py--beginning-of-block-position)))

(ert-deftest py-ert-py--beginning-of-clause-position-functionp-test ()
  (should (functionp 'py--beginning-of-clause-position)))

(ert-deftest py-ert-py--beginning-of-block-or-clause-position-functionp-test ()
  (should (functionp 'py--beginning-of-block-or-clause-position)))

(ert-deftest py-ert-py--beginning-of-def-position-functionp-test ()
  (should (functionp 'py--beginning-of-def-position)))

(ert-deftest py-ert-py--beginning-of-class-position-functionp-test ()
  (should (functionp 'py--beginning-of-class-position)))

(ert-deftest py-ert-py--beginning-of-def-or-class-position-functionp-test ()
  (should (functionp 'py--beginning-of-def-or-class-position)))

(ert-deftest py-ert-py--beginning-of-expression-position-functionp-test ()
  (should (functionp 'py--beginning-of-expression-position)))

(ert-deftest py-ert-py--beginning-of-partial-expression-position-functionp-test ()
  (should (functionp 'py--beginning-of-partial-expression-position)))

(ert-deftest py-ert-py--beginning-of-minor-block-position-functionp-test ()
  (should (functionp 'py--beginning-of-minor-block-position)))

(ert-deftest py-ert-py--beginning-of-if-block-position-functionp-test ()
  (should (functionp 'py--beginning-of-if-block-position)))

(ert-deftest py-ert-py--beginning-of-try-block-position-functionp-test ()
  (should (functionp 'py--beginning-of-try-block-position)))

(ert-deftest py-ert-py--beginning-of-except-block-position-functionp-test ()
  (should (functionp 'py--beginning-of-except-block-position)))

(ert-deftest py-ert-py--beginning-of-top-level-position-functionp-test ()
  (should (functionp 'py--beginning-of-top-level-position)))

(ert-deftest py-ert-py--beginning-of-statement-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-statement-position-bol)))

(ert-deftest py-ert-py--beginning-of-block-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-block-position-bol)))

(ert-deftest py-ert-py--beginning-of-clause-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-clause-position-bol)))

(ert-deftest py-ert-py--beginning-of-block-or-clause-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-block-or-clause-position-bol)))

(ert-deftest py-ert-py--beginning-of-def-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-def-position-bol)))

(ert-deftest py-ert-py--beginning-of-class-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-class-position-bol)))

(ert-deftest py-ert-py--beginning-of-def-or-class-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-def-or-class-position-bol)))

(ert-deftest py-ert-py--beginning-of-minor-block-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-minor-block-position-bol)))

(ert-deftest py-ert-py--beginning-of-if-block-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-if-block-position-bol)))

(ert-deftest py-ert-py--beginning-of-try-block-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-try-block-position-bol)))

(ert-deftest py-ert-py--beginning-of-except-block-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-except-block-position-bol)))

(ert-deftest py-ert-py--beginning-of-top-level-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-top-level-position-bol)))

(ert-deftest py-ert-py-fill-string-django-functionp-test ()
  (should (functionp 'py-fill-string-django)))

(ert-deftest py-ert-py-fill-string-onetwo-functionp-test ()
  (should (functionp 'py-fill-string-onetwo)))

(ert-deftest py-ert-py-fill-string-pep-257-functionp-test ()
  (should (functionp 'py-fill-string-pep-257)))

(ert-deftest py-ert-py-fill-string-pep-257-nn-functionp-test ()
  (should (functionp 'py-fill-string-pep-257-nn)))

(ert-deftest py-ert-py-fill-string-symmetric-functionp-test ()
  (should (functionp 'py-fill-string-symmetric)))

(ert-deftest py-ert-py-set-nil-docstring-style-functionp-test ()
  (should (functionp 'py-set-nil-docstring-style)))

(ert-deftest py-ert-py-set-pep-257-nn-docstring-style-functionp-test ()
  (should (functionp 'py-set-pep-257-nn-docstring-style)))

(ert-deftest py-ert-py-set-pep-257-docstring-style-functionp-test ()
  (should (functionp 'py-set-pep-257-docstring-style)))

(ert-deftest py-ert-py-set-django-docstring-style-functionp-test ()
  (should (functionp 'py-set-django-docstring-style)))

(ert-deftest py-ert-py-set-symmetric-docstring-style-functionp-test ()
  (should (functionp 'py-set-symmetric-docstring-style)))

(ert-deftest py-ert-py-set-onetwo-docstring-style-functionp-test ()
  (should (functionp 'py-set-onetwo-docstring-style)))

(ert-deftest py-ert-py-fill-decorator-functionp-test ()
  (should (functionp 'py-fill-decorator)))

(ert-deftest py-ert-py-fill-comment-functionp-test ()
  (should (functionp 'py-fill-comment)))

(ert-deftest py-ert-py-fill-labelled-string-functionp-test ()
  (should (functionp 'py-fill-labelled-string)))

(ert-deftest py-ert-py--in-or-behind-or-before-a-docstring-functionp-test ()
  (should (functionp 'py--in-or-behind-or-before-a-docstring)))

(ert-deftest py-ert-py--string-fence-delete-spaces-functionp-test ()
  (should (functionp 'py--string-fence-delete-spaces)))

(ert-deftest py-ert-py--fill-fix-end-functionp-test ()
  (should (functionp 'py--fill-fix-end)))

(ert-deftest py-ert-py--fill-docstring-base-functionp-test ()
  (should (functionp 'py--fill-docstring-base)))

(ert-deftest py-ert-py--fill-docstring-last-line-functionp-test ()
  (should (functionp 'py--fill-docstring-last-line)))

(ert-deftest py-ert-py--fill-docstring-first-line-functionp-test ()
  (should (functionp 'py--fill-docstring-first-line)))

(ert-deftest py-ert-py--fill-docstring-functionp-test ()
  (should (functionp 'py--fill-docstring)))

(ert-deftest py-ert-py-fill-string-functionp-test ()
  (should (functionp 'py-fill-string)))

(ert-deftest py-ert-py-fill-paragraph-functionp-test ()
  (should (functionp 'py-fill-paragraph)))

(ert-deftest py-ert-py-insert-default-shebang-functionp-test ()
  (should (functionp 'py-insert-default-shebang)))

(ert-deftest py-ert-py--top-level-form-p-functionp-test ()
  (should (functionp 'py--top-level-form-p)))

(ert-deftest py-ert-py-indent-line-outmost-functionp-test ()
  (should (functionp 'py-indent-line-outmost)))

(ert-deftest py-ert-py--indent-fix-region-intern-functionp-test ()
  (should (functionp 'py--indent-fix-region-intern)))

(ert-deftest py-ert-py--indent-line-intern-functionp-test ()
  (should (functionp 'py--indent-line-intern)))

(ert-deftest py-ert-py--indent-line-base-functionp-test ()
  (should (functionp 'py--indent-line-base)))

(ert-deftest py-ert-py--calculate-indent-backwards-functionp-test ()
  (should (functionp 'py--calculate-indent-backwards)))

(ert-deftest py-ert-py-indent-line-functionp-test ()
  (should (functionp 'py-indent-line)))

(ert-deftest py-ert-py--delete-trailing-whitespace-functionp-test ()
  (should (functionp 'py--delete-trailing-whitespace)))

(ert-deftest py-ert-py-newline-and-indent-functionp-test ()
  (should (functionp 'py-newline-and-indent)))

(ert-deftest py-ert-py-newline-and-dedent-functionp-test ()
  (should (functionp 'py-newline-and-dedent)))

(ert-deftest py-ert-py-toggle-indent-tabs-mode-functionp-test ()
  (should (functionp 'py-toggle-indent-tabs-mode)))

(ert-deftest py-ert-py-indent-tabs-mode-functionp-test ()
  (should (functionp 'py-indent-tabs-mode)))

(ert-deftest py-ert-py-indent-tabs-mode-on-functionp-test ()
  (should (functionp 'py-indent-tabs-mode-on)))

(ert-deftest py-ert-py-indent-tabs-mode-off-functionp-test ()
  (should (functionp 'py-indent-tabs-mode-off)))

(ert-deftest py-ert-py-guessed-sanity-check-functionp-test ()
  (should (functionp 'py-guessed-sanity-check)))

(ert-deftest py-ert-py--guess-indent-final-functionp-test ()
  (should (functionp 'py--guess-indent-final)))

(ert-deftest py-ert-py--guess-indent-forward-functionp-test ()
  (should (functionp 'py--guess-indent-forward)))

(ert-deftest py-ert-py--guess-indent-backward-functionp-test ()
  (should (functionp 'py--guess-indent-backward)))

(ert-deftest py-ert-py-guess-indent-offset-functionp-test ()
  (should (functionp 'py-guess-indent-offset)))

(ert-deftest py-ert-py--comment-indent-function-functionp-test ()
  (should (functionp 'py--comment-indent-function)))

(ert-deftest py-ert-py-narrow-to-defun-functionp-test ()
  (should (functionp 'py-narrow-to-defun)))

(ert-deftest py-ert-py-beginning-of-paragraph-functionp-test ()
  (should (functionp 'py-beginning-of-paragraph)))

(ert-deftest py-ert-py-end-of-paragraph-functionp-test ()
  (should (functionp 'py-end-of-paragraph)))

(ert-deftest py-ert-py-indent-and-forward-functionp-test ()
  (should (functionp 'py-indent-and-forward)))

(ert-deftest py-ert-py-indent-region-functionp-test ()
  (should (functionp 'py-indent-region)))

(ert-deftest py-ert-py--beginning-of-buffer-position-functionp-test ()
  (should (functionp 'py--beginning-of-buffer-position)))

(ert-deftest py-ert-py--end-of-buffer-position-functionp-test ()
  (should (functionp 'py--end-of-buffer-position)))

(ert-deftest py-ert-py--bounds-of-declarations-functionp-test ()
  (should (functionp 'py--bounds-of-declarations)))

(ert-deftest py-ert-py-beginning-of-declarations-functionp-test ()
  (should (functionp 'py-beginning-of-declarations)))

(ert-deftest py-ert-py-end-of-declarations-functionp-test ()
  (should (functionp 'py-end-of-declarations)))

(ert-deftest py-ert-py-declarations-functionp-test ()
  (should (functionp 'py-declarations)))

(ert-deftest py-ert-py-kill-declarations-functionp-test ()
  (should (functionp 'py-kill-declarations)))

(ert-deftest py-ert-py--bounds-of-statements-functionp-test ()
  (should (functionp 'py--bounds-of-statements)))

(ert-deftest py-ert-py-beginning-of-statements-functionp-test ()
  (should (functionp 'py-beginning-of-statements)))

(ert-deftest py-ert-py-end-of-statements-functionp-test ()
  (should (functionp 'py-end-of-statements)))

(ert-deftest py-ert-py-statements-functionp-test ()
  (should (functionp 'py-statements)))

(ert-deftest py-ert-py-kill-statements-functionp-test ()
  (should (functionp 'py-kill-statements)))

(ert-deftest py-ert-py--join-words-wrapping-functionp-test ()
  (should (functionp 'py--join-words-wrapping)))

(ert-deftest py-ert-py-insert-super-functionp-test ()
  (should (functionp 'py-insert-super)))

(ert-deftest py-ert-py-comment-region-functionp-test ()
  (should (functionp 'py-comment-region)))

(ert-deftest py-ert-py-delete-comments-in-def-or-class-functionp-test ()
  (should (functionp 'py-delete-comments-in-def-or-class)))

(ert-deftest py-ert-py-delete-comments-in-class-functionp-test ()
  (should (functionp 'py-delete-comments-in-class)))

(ert-deftest py-ert-py-delete-comments-in-block-functionp-test ()
  (should (functionp 'py-delete-comments-in-block)))

(ert-deftest py-ert-py-delete-comments-in-region-functionp-test ()
  (should (functionp 'py-delete-comments-in-region)))

(ert-deftest py-ert-py--delete-comments-intern-functionp-test ()
  (should (functionp 'py--delete-comments-intern)))

(ert-deftest py-ert-py-update-gud-pdb-history-functionp-test ()
  (should (functionp 'py-update-gud-pdb-history)))

(ert-deftest py-ert-py--pdbtrack-overlay-arrow-functionp-test ()
  (should (functionp 'py--pdbtrack-overlay-arrow)))

(ert-deftest py-ert-py--pdbtrack-track-stack-file-functionp-test ()
  (should (functionp 'py--pdbtrack-track-stack-file)))

(ert-deftest py-ert-py--pdbtrack-map-filename-functionp-test ()
  (should (functionp 'py--pdbtrack-map-filename)))

(ert-deftest py-ert-py--pdbtrack-get-source-buffer-functionp-test ()
  (should (functionp 'py--pdbtrack-get-source-buffer)))

(ert-deftest py-ert-py--pdbtrack-grub-for-buffer-functionp-test ()
  (should (functionp 'py--pdbtrack-grub-for-buffer)))

(ert-deftest py-ert-py-pdbtrack-toggle-stack-tracking-functionp-test ()
  (should (functionp 'py-pdbtrack-toggle-stack-tracking)))

(ert-deftest py-ert-turn-on-pdbtrack-functionp-test ()
  (should (functionp 'turn-on-pdbtrack)))

(ert-deftest py-ert-turn-off-pdbtrack-functionp-test ()
  (should (functionp 'turn-off-pdbtrack)))

(ert-deftest py-ert-py-execute-statement-pdb-functionp-test ()
  (should (functionp 'py-execute-statement-pdb)))

(ert-deftest py-ert-py-execute-region-pdb-functionp-test ()
  (should (functionp 'py-execute-region-pdb)))

(ert-deftest py-ert-py-pdb-execute-statement-functionp-test ()
  (should (functionp 'py-pdb-execute-statement)))

(ert-deftest py-ert-py-pdb-help-functionp-test ()
  (should (functionp 'py-pdb-help)))

(ert-deftest py-ert-py-pdb-break-functionp-test ()
  (should (functionp 'py-pdb-break)))

(ert-deftest py-ert-py-end-of-block-functionp-test ()
  (should (functionp 'py-end-of-block)))

(ert-deftest py-ert-py-end-of-block-bol-functionp-test ()
  (should (functionp 'py-end-of-block-bol)))

(ert-deftest py-ert-py-end-of-clause-functionp-test ()
  (should (functionp 'py-end-of-clause)))

(ert-deftest py-ert-py-end-of-clause-bol-functionp-test ()
  (should (functionp 'py-end-of-clause-bol)))

(ert-deftest py-ert-py-end-of-block-or-clause-functionp-test ()
  (should (functionp 'py-end-of-block-or-clause)))

(ert-deftest py-ert-py-end-of-block-or-clause-bol-functionp-test ()
  (should (functionp 'py-end-of-block-or-clause-bol)))

(ert-deftest py-ert-py-end-of-def-functionp-test ()
  (should (functionp 'py-end-of-def)))

(ert-deftest py-ert-py-end-of-def-bol-functionp-test ()
  (should (functionp 'py-end-of-def-bol)))

(ert-deftest py-ert-py-end-of-class-functionp-test ()
  (should (functionp 'py-end-of-class)))

(ert-deftest py-ert-py-end-of-class-bol-functionp-test ()
  (should (functionp 'py-end-of-class-bol)))

(ert-deftest py-ert-py-end-of-def-or-class-functionp-test ()
  (should (functionp 'py-end-of-def-or-class)))

(ert-deftest py-ert-py-end-of-def-or-class-bol-functionp-test ()
  (should (functionp 'py-end-of-def-or-class-bol)))

(ert-deftest py-ert-py-end-of-if-block-functionp-test ()
  (should (functionp 'py-end-of-if-block)))

(ert-deftest py-ert-py-end-of-if-block-bol-functionp-test ()
  (should (functionp 'py-end-of-if-block-bol)))

(ert-deftest py-ert-py-end-of-try-block-functionp-test ()
  (should (functionp 'py-end-of-try-block)))

(ert-deftest py-ert-py-end-of-try-block-bol-functionp-test ()
  (should (functionp 'py-end-of-try-block-bol)))

(ert-deftest py-ert-py-end-of-minor-block-functionp-test ()
  (should (functionp 'py-end-of-minor-block)))

(ert-deftest py-ert-py-end-of-minor-block-bol-functionp-test ()
  (should (functionp 'py-end-of-minor-block-bol)))

(ert-deftest py-ert-py-end-of-for-block-functionp-test ()
  (should (functionp 'py-end-of-for-block)))

(ert-deftest py-ert-py-end-of-for-block-bol-functionp-test ()
  (should (functionp 'py-end-of-for-block-bol)))

(ert-deftest py-ert-py-end-of-except-block-functionp-test ()
  (should (functionp 'py-end-of-except-block)))

(ert-deftest py-ert-py-end-of-except-block-bol-functionp-test ()
  (should (functionp 'py-end-of-except-block-bol)))

(ert-deftest py-ert-py-execute-statement-functionp-test ()
  (should (functionp 'py-execute-statement)))

(ert-deftest py-ert-py-execute-block-functionp-test ()
  (should (functionp 'py-execute-block)))

(ert-deftest py-ert-py-execute-block-or-clause-functionp-test ()
  (should (functionp 'py-execute-block-or-clause)))

(ert-deftest py-ert-py-execute-def-functionp-test ()
  (should (functionp 'py-execute-def)))

(ert-deftest py-ert-py-execute-class-functionp-test ()
  (should (functionp 'py-execute-class)))

(ert-deftest py-ert-py-execute-def-or-class-functionp-test ()
  (should (functionp 'py-execute-def-or-class)))

(ert-deftest py-ert-py-execute-expression-functionp-test ()
  (should (functionp 'py-execute-expression)))

(ert-deftest py-ert-py-execute-partial-expression-functionp-test ()
  (should (functionp 'py-execute-partial-expression)))

(ert-deftest py-ert-py-execute-top-level-functionp-test ()
  (should (functionp 'py-execute-top-level)))

(ert-deftest py-ert-py-execute-clause-functionp-test ()
  (should (functionp 'py-execute-clause)))

(ert-deftest py-ert-py--end-of-statement-position-functionp-test ()
  (should (functionp 'py--end-of-statement-position)))

(ert-deftest py-ert-py--end-of-block-position-functionp-test ()
  (should (functionp 'py--end-of-block-position)))

(ert-deftest py-ert-py--end-of-clause-position-functionp-test ()
  (should (functionp 'py--end-of-clause-position)))

(ert-deftest py-ert-py--end-of-block-or-clause-position-functionp-test ()
  (should (functionp 'py--end-of-block-or-clause-position)))

(ert-deftest py-ert-py--end-of-def-position-functionp-test ()
  (should (functionp 'py--end-of-def-position)))

(ert-deftest py-ert-py--end-of-class-position-functionp-test ()
  (should (functionp 'py--end-of-class-position)))

(ert-deftest py-ert-py--end-of-def-or-class-position-functionp-test ()
  (should (functionp 'py--end-of-def-or-class-position)))

(ert-deftest py-ert-py--end-of-buffer-position-functionp-test ()
  (should (functionp 'py--end-of-buffer-position)))

(ert-deftest py-ert-py--end-of-expression-position-functionp-test ()
  (should (functionp 'py--end-of-expression-position)))

(ert-deftest py-ert-py--end-of-partial-expression-position-functionp-test ()
  (should (functionp 'py--end-of-partial-expression-position)))

(ert-deftest py-ert-py--end-of-minor-block-position-functionp-test ()
  (should (functionp 'py--end-of-minor-block-position)))

(ert-deftest py-ert-py--end-of-if-block-position-functionp-test ()
  (should (functionp 'py--end-of-if-block-position)))

(ert-deftest py-ert-py--end-of-try-block-position-functionp-test ()
  (should (functionp 'py--end-of-try-block-position)))

(ert-deftest py-ert-py--end-of-except-block-position-functionp-test ()
  (should (functionp 'py--end-of-except-block-position)))

(ert-deftest py-ert-py--end-of-top-level-position-functionp-test ()
  (should (functionp 'py--end-of-top-level-position)))

(ert-deftest py-ert-py--end-of-statement-position-bol-functionp-test ()
  (should (functionp 'py--end-of-statement-position-bol)))

(ert-deftest py-ert-py--end-of-block-position-bol-functionp-test ()
  (should (functionp 'py--end-of-block-position-bol)))

(ert-deftest py-ert-py--end-of-clause-position-bol-functionp-test ()
  (should (functionp 'py--end-of-clause-position-bol)))

(ert-deftest py-ert-py--end-of-block-or-clause-position-bol-functionp-test ()
  (should (functionp 'py--end-of-block-or-clause-position-bol)))

(ert-deftest py-ert-py--end-of-def-position-bol-functionp-test ()
  (should (functionp 'py--end-of-def-position-bol)))

(ert-deftest py-ert-py--end-of-class-position-bol-functionp-test ()
  (should (functionp 'py--end-of-class-position-bol)))

(ert-deftest py-ert-py--end-of-minor-block-position-bol-functionp-test ()
  (should (functionp 'py--end-of-minor-block-position-bol)))

(ert-deftest py-ert-py--end-of-if-block-position-bol-functionp-test ()
  (should (functionp 'py--end-of-if-block-position-bol)))

(ert-deftest py-ert-py--end-of-try-block-position-bol-functionp-test ()
  (should (functionp 'py--end-of-try-block-position-bol)))

(ert-deftest py-ert-py-toggle-highlight-indentation-functionp-test ()
  (should (functionp 'py-toggle-highlight-indentation)))

(ert-deftest py-ert-py-highlight-indentation-off-functionp-test ()
  (should (functionp 'py-highlight-indentation-off)))

(ert-deftest py-ert-py-highlight-indentation-on-functionp-test ()
  (should (functionp 'py-highlight-indentation-on)))

(ert-deftest py-ert-py-toggle-smart-indentation-functionp-test ()
  (should (functionp 'py-toggle-smart-indentation)))

(ert-deftest py-ert-py-smart-indentation-on-functionp-test ()
  (should (functionp 'py-smart-indentation-on)))

(ert-deftest py-ert-py-smart-indentation-off-functionp-test ()
  (should (functionp 'py-smart-indentation-off)))

(ert-deftest py-ert-py-toggle-sexp-function-functionp-test ()
  (should (functionp 'py-toggle-sexp-function)))

(ert-deftest py-ert-py-toggle-autopair-mode-functionp-test ()
  (should (functionp 'py-toggle-autopair-mode)))

(ert-deftest py-ert-py-autopair-mode-on-functionp-test ()
  (should (functionp 'py-autopair-mode-on)))

(ert-deftest py-ert-py-autopair-mode-off-functionp-test ()
  (should (functionp 'py-autopair-mode-off)))

(ert-deftest py-ert-toggle-py-smart-operator-mode-p-functionp-test ()
  (should (functionp 'toggle-py-smart-operator-mode-p)))

(ert-deftest py-ert-py-smart-operator-mode-p-on-functionp-test ()
  (should (functionp 'py-smart-operator-mode-p-on)))

(ert-deftest py-ert-py-smart-operator-mode-p-off-functionp-test ()
  (should (functionp 'py-smart-operator-mode-p-off)))

(ert-deftest py-ert-toggle-py-switch-buffers-on-execute-p-functionp-test ()
  (should (functionp 'toggle-py-switch-buffers-on-execute-p)))

(ert-deftest py-ert-py-switch-buffers-on-execute-p-on-functionp-test ()
  (should (functionp 'py-switch-buffers-on-execute-p-on)))

(ert-deftest py-ert-py-switch-buffers-on-execute-p-off-functionp-test ()
  (should (functionp 'py-switch-buffers-on-execute-p-off)))

(ert-deftest py-ert-toggle-py-split-window-on-execute-functionp-test ()
  (should (functionp 'toggle-py-split-window-on-execute)))

(ert-deftest py-ert-py-split-window-on-execute-on-functionp-test ()
  (should (functionp 'py-split-window-on-execute-on)))

(ert-deftest py-ert-py-split-window-on-execute-off-functionp-test ()
  (should (functionp 'py-split-window-on-execute-off)))

(ert-deftest py-ert-toggle-py-fontify-shell-buffer-p-functionp-test ()
  (should (functionp 'toggle-py-fontify-shell-buffer-p)))

(ert-deftest py-ert-py-fontify-shell-buffer-p-on-functionp-test ()
  (should (functionp 'py-fontify-shell-buffer-p-on)))

(ert-deftest py-ert-py-fontify-shell-buffer-p-off-functionp-test ()
  (should (functionp 'py-fontify-shell-buffer-p-off)))

(ert-deftest py-ert-toggle-python-mode-v5-behavior-p-functionp-test ()
  (should (functionp 'toggle-python-mode-v5-behavior-p)))

(ert-deftest py-ert-python-mode-v5-behavior-p-on-functionp-test ()
  (should (functionp 'python-mode-v5-behavior-p-on)))

(ert-deftest py-ert-python-mode-v5-behavior-p-off-functionp-test ()
  (should (functionp 'python-mode-v5-behavior-p-off)))

(ert-deftest py-ert-toggle-py-jump-on-exception-functionp-test ()
  (should (functionp 'toggle-py-jump-on-exception)))

(ert-deftest py-ert-py-jump-on-exception-on-functionp-test ()
  (should (functionp 'py-jump-on-exception-on)))

(ert-deftest py-ert-py-jump-on-exception-off-functionp-test ()
  (should (functionp 'py-jump-on-exception-off)))

(ert-deftest py-ert-toggle-py-use-current-dir-when-execute-p-functionp-test ()
  (should (functionp 'toggle-py-use-current-dir-when-execute-p)))

(ert-deftest py-ert-py-use-current-dir-when-execute-p-on-functionp-test ()
  (should (functionp 'py-use-current-dir-when-execute-p-on)))

(ert-deftest py-ert-py-use-current-dir-when-execute-p-off-functionp-test ()
  (should (functionp 'py-use-current-dir-when-execute-p-off)))

(ert-deftest py-ert-toggle-py-electric-comment-p-functionp-test ()
  (should (functionp 'toggle-py-electric-comment-p)))

(ert-deftest py-ert-py-electric-comment-p-on-functionp-test ()
  (should (functionp 'py-electric-comment-p-on)))

(ert-deftest py-ert-py-electric-comment-p-off-functionp-test ()
  (should (functionp 'py-electric-comment-p-off)))

(ert-deftest py-ert-toggle-py-underscore-word-syntax-p-functionp-test ()
  (should (functionp 'toggle-py-underscore-word-syntax-p)))

(ert-deftest py-ert-py-underscore-word-syntax-p-on-functionp-test ()
  (should (functionp 'py-underscore-word-syntax-p-on)))

(ert-deftest py-ert-py-underscore-word-syntax-p-off-functionp-test ()
  (should (functionp 'py-underscore-word-syntax-p-off)))

(ert-deftest py-ert-py-beginning-of-block-functionp-test ()
  (should (functionp 'py-beginning-of-block)))

(ert-deftest py-ert-py-beginning-of-clause-functionp-test ()
  (should (functionp 'py-beginning-of-clause)))

(ert-deftest py-ert-py-beginning-of-block-or-clause-functionp-test ()
  (should (functionp 'py-beginning-of-block-or-clause)))

(ert-deftest py-ert-py-beginning-of-def-functionp-test ()
  (should (functionp 'py-beginning-of-def)))

(ert-deftest py-ert-py-beginning-of-class-functionp-test ()
  (should (functionp 'py-beginning-of-class)))

(ert-deftest py-ert-py-beginning-of-def-or-class-functionp-test ()
  (should (functionp 'py-beginning-of-def-or-class)))

(ert-deftest py-ert-py-beginning-of-if-block-functionp-test ()
  (should (functionp 'py-beginning-of-if-block)))

(ert-deftest py-ert-py-beginning-of-try-block-functionp-test ()
  (should (functionp 'py-beginning-of-try-block)))

(ert-deftest py-ert-py-beginning-of-minor-block-functionp-test ()
  (should (functionp 'py-beginning-of-minor-block)))

(ert-deftest py-ert-py-beginning-of-for-block-functionp-test ()
  (should (functionp 'py-beginning-of-for-block)))

(ert-deftest py-ert-py-beginning-of-except-block-functionp-test ()
  (should (functionp 'py-beginning-of-except-block)))

(ert-deftest py-ert-py-beginning-of-block-bol-functionp-test ()
  (should (functionp 'py-beginning-of-block-bol)))

(ert-deftest py-ert-py-beginning-of-clause-bol-functionp-test ()
  (should (functionp 'py-beginning-of-clause-bol)))

(ert-deftest py-ert-py-beginning-of-block-or-clause-bol-functionp-test ()
  (should (functionp 'py-beginning-of-block-or-clause-bol)))

(ert-deftest py-ert-py-beginning-of-def-bol-functionp-test ()
  (should (functionp 'py-beginning-of-def-bol)))

(ert-deftest py-ert-py-beginning-of-class-bol-functionp-test ()
  (should (functionp 'py-beginning-of-class-bol)))

(ert-deftest py-ert-py-beginning-of-def-or-class-bol-functionp-test ()
  (should (functionp 'py-beginning-of-def-or-class-bol)))

(ert-deftest py-ert-py-beginning-of-if-block-bol-functionp-test ()
  (should (functionp 'py-beginning-of-if-block-bol)))

(ert-deftest py-ert-py-beginning-of-try-block-bol-functionp-test ()
  (should (functionp 'py-beginning-of-try-block-bol)))

(ert-deftest py-ert-py-beginning-of-minor-block-bol-functionp-test ()
  (should (functionp 'py-beginning-of-minor-block-bol)))

(ert-deftest py-ert-py-beginning-of-for-block-bol-functionp-test ()
  (should (functionp 'py-beginning-of-for-block-bol)))

(ert-deftest py-ert-py-beginning-of-except-block-bol-functionp-test ()
  (should (functionp 'py-beginning-of-except-block-bol)))

(ert-deftest py-ert-py-comment-auto-fill-functionp-test ()
  (should (functionp 'py-comment-auto-fill)))

(ert-deftest py-ert-py-comment-auto-fill-on-functionp-test ()
  (should (functionp 'py-comment-auto-fill-on)))

(ert-deftest py-ert-py-comment-auto-fill-off-functionp-test ()
  (should (functionp 'py-comment-auto-fill-off)))

(ert-deftest py-ert-py-beginning-of-block-functionp-test ()
  (should (functionp 'py-beginning-of-block)))

(ert-deftest py-ert-py-beginning-of-clause-functionp-test ()
  (should (functionp 'py-beginning-of-clause)))

(ert-deftest py-ert-py-beginning-of-block-or-clause-functionp-test ()
  (should (functionp 'py-beginning-of-block-or-clause)))

(ert-deftest py-ert-py-beginning-of-def-functionp-test ()
  (should (functionp 'py-beginning-of-def)))

(ert-deftest py-ert-py-beginning-of-class-functionp-test ()
  (should (functionp 'py-beginning-of-class)))

(ert-deftest py-ert-py-beginning-of-def-or-class-functionp-test ()
  (should (functionp 'py-beginning-of-def-or-class)))

(ert-deftest py-ert-py-beginning-of-if-block-functionp-test ()
  (should (functionp 'py-beginning-of-if-block)))

(ert-deftest py-ert-py-beginning-of-elif-block-functionp-test ()
  (should (functionp 'py-beginning-of-elif-block)))

(ert-deftest py-ert-py-beginning-of-else-block-functionp-test ()
  (should (functionp 'py-beginning-of-else-block)))

(ert-deftest py-ert-py-beginning-of-try-block-functionp-test ()
  (should (functionp 'py-beginning-of-try-block)))

(ert-deftest py-ert-py-beginning-of-minor-block-functionp-test ()
  (should (functionp 'py-beginning-of-minor-block)))

(ert-deftest py-ert-py-beginning-of-for-block-functionp-test ()
  (should (functionp 'py-beginning-of-for-block)))

(ert-deftest py-ert-py-beginning-of-except-block-functionp-test ()
  (should (functionp 'py-beginning-of-except-block)))

(ert-deftest py-ert-py-beginning-of-block-bol-functionp-test ()
  (should (functionp 'py-beginning-of-block-bol)))

(ert-deftest py-ert-py-beginning-of-clause-bol-functionp-test ()
  (should (functionp 'py-beginning-of-clause-bol)))

(ert-deftest py-ert-py-beginning-of-block-or-clause-bol-functionp-test ()
  (should (functionp 'py-beginning-of-block-or-clause-bol)))

(ert-deftest py-ert-py-beginning-of-def-bol-functionp-test ()
  (should (functionp 'py-beginning-of-def-bol)))

(ert-deftest py-ert-py-beginning-of-class-bol-functionp-test ()
  (should (functionp 'py-beginning-of-class-bol)))

(ert-deftest py-ert-py-beginning-of-def-or-class-bol-functionp-test ()
  (should (functionp 'py-beginning-of-def-or-class-bol)))

(ert-deftest py-ert-py-beginning-of-if-block-bol-functionp-test ()
  (should (functionp 'py-beginning-of-if-block-bol)))

(ert-deftest py-ert-py-beginning-of-elif-block-bol-functionp-test ()
  (should (functionp 'py-beginning-of-elif-block-bol)))

(ert-deftest py-ert-py-beginning-of-else-block-bol-functionp-test ()
  (should (functionp 'py-beginning-of-else-block-bol)))

(ert-deftest py-ert-py-beginning-of-try-block-bol-functionp-test ()
  (should (functionp 'py-beginning-of-try-block-bol)))

(ert-deftest py-ert-py-beginning-of-minor-block-bol-functionp-test ()
  (should (functionp 'py-beginning-of-minor-block-bol)))

(ert-deftest py-ert-py-beginning-of-for-block-bol-functionp-test ()
  (should (functionp 'py-beginning-of-for-block-bol)))

(ert-deftest py-ert-py-beginning-of-except-block-bol-functionp-test ()
  (should (functionp 'py-beginning-of-except-block-bol)))

(ert-deftest py-ert-py-indent-forward-line-functionp-test ()
  (should (functionp 'py-indent-forward-line)))

(ert-deftest py-ert-py-dedent-forward-line-functionp-test ()
  (should (functionp 'py-dedent-forward-line)))

(ert-deftest py-ert-py-dedent-functionp-test ()
  (should (functionp 'py-dedent)))

(ert-deftest py-ert-py--close-intern-functionp-test ()
  (should (functionp 'py--close-intern)))

(ert-deftest py-ert-py-close-def-functionp-test ()
  (should (functionp 'py-close-def)))

(ert-deftest py-ert-py-close-class-functionp-test ()
  (should (functionp 'py-close-class)))

(ert-deftest py-ert-py-close-clause-functionp-test ()
  (should (functionp 'py-close-clause)))

(ert-deftest py-ert-py-close-block-functionp-test ()
  (should (functionp 'py-close-block)))

(ert-deftest py-ert-py-class-at-point-functionp-test ()
  (should (functionp 'py-class-at-point)))

(ert-deftest py-ert-ar-py-function-at-point-functionp-test ()
  (should (functionp 'ar-py-function-at-point)))

(ert-deftest py-ert-ar-py-beginning-of-function-functionp-test ()
  (should (functionp 'ar-py-beginning-of-function)))

(ert-deftest py-ert-ar-py-beginning-of-class-functionp-test ()
  (should (functionp 'ar-py-beginning-of-class)))

(ert-deftest py-ert-ar-py-end-of-function-functionp-test ()
  (should (functionp 'ar-py-end-of-function)))

(ert-deftest py-ert-ar-py-line-at-point-functionp-test ()
  (should (functionp 'ar-py-line-at-point)))

(ert-deftest py-ert-ar-py-looking-at-keywords-p-functionp-test ()
  (should (functionp 'ar-py-looking-at-keywords-p)))

(ert-deftest py-ert-ar-py-match-paren-mode-functionp-test ()
  (should (functionp 'ar-py-match-paren-mode)))

(ert-deftest py-ert-ar-py-match-paren-functionp-test ()
  (should (functionp 'ar-py-match-paren)))

(ert-deftest py-ert-ar-py-documentation-functionp-test ()
  (should (functionp 'ar-py-documentation)))

(ert-deftest py-ert-eva-functionp-test ()
  (should (functionp 'eva)))

(ert-deftest py-ert-pst-here-functionp-test ()
  (should (functionp 'pst-here)))

(ert-deftest py-ert-py-printform-insert-functionp-test ()
  (should (functionp 'py-printform-insert)))

(ert-deftest py-ert-py-line-to-printform-python2-functionp-test ()
  (should (functionp 'py-line-to-printform-python2)))

(ert-deftest py-ert-py-boolswitch-functionp-test ()
  (should (functionp 'py-boolswitch)))

(ert-deftest py-ert-py-end-of-block-functionp-test ()
  (should (functionp 'py-end-of-block)))

(ert-deftest py-ert-py-end-of-block-bol-functionp-test ()
  (should (functionp 'py-end-of-block-bol)))

(ert-deftest py-ert-py-end-of-clause-functionp-test ()
  (should (functionp 'py-end-of-clause)))

(ert-deftest py-ert-py-end-of-clause-bol-functionp-test ()
  (should (functionp 'py-end-of-clause-bol)))

(ert-deftest py-ert-py-end-of-block-or-clause-functionp-test ()
  (should (functionp 'py-end-of-block-or-clause)))

(ert-deftest py-ert-py-end-of-block-or-clause-bol-functionp-test ()
  (should (functionp 'py-end-of-block-or-clause-bol)))

(ert-deftest py-ert-py-end-of-def-functionp-test ()
  (should (functionp 'py-end-of-def)))

(ert-deftest py-ert-py-end-of-def-bol-functionp-test ()
  (should (functionp 'py-end-of-def-bol)))

(ert-deftest py-ert-py-end-of-class-functionp-test ()
  (should (functionp 'py-end-of-class)))

(ert-deftest py-ert-py-end-of-class-bol-functionp-test ()
  (should (functionp 'py-end-of-class-bol)))

(ert-deftest py-ert-py-end-of-def-or-class-functionp-test ()
  (should (functionp 'py-end-of-def-or-class)))

(ert-deftest py-ert-py-end-of-def-or-class-bol-functionp-test ()
  (should (functionp 'py-end-of-def-or-class-bol)))

(ert-deftest py-ert-py-end-of-if-block-functionp-test ()
  (should (functionp 'py-end-of-if-block)))

(ert-deftest py-ert-py-end-of-if-block-bol-functionp-test ()
  (should (functionp 'py-end-of-if-block-bol)))

(ert-deftest py-ert-py-end-of-elif-block-functionp-test ()
  (should (functionp 'py-end-of-elif-block)))

(ert-deftest py-ert-py-end-of-elif-block-bol-functionp-test ()
  (should (functionp 'py-end-of-elif-block-bol)))

(ert-deftest py-ert-py-end-of-else-block-functionp-test ()
  (should (functionp 'py-end-of-else-block)))

(ert-deftest py-ert-py-end-of-else-block-bol-functionp-test ()
  (should (functionp 'py-end-of-else-block-bol)))

(ert-deftest py-ert-py-end-of-try-block-functionp-test ()
  (should (functionp 'py-end-of-try-block)))

(ert-deftest py-ert-py-end-of-try-block-bol-functionp-test ()
  (should (functionp 'py-end-of-try-block-bol)))

(ert-deftest py-ert-py-end-of-minor-block-functionp-test ()
  (should (functionp 'py-end-of-minor-block)))

(ert-deftest py-ert-py-end-of-minor-block-bol-functionp-test ()
  (should (functionp 'py-end-of-minor-block-bol)))

(ert-deftest py-ert-py-end-of-for-block-functionp-test ()
  (should (functionp 'py-end-of-for-block)))

(ert-deftest py-ert-py-end-of-for-block-bol-functionp-test ()
  (should (functionp 'py-end-of-for-block-bol)))

(ert-deftest py-ert-py-end-of-except-block-functionp-test ()
  (should (functionp 'py-end-of-except-block)))

(ert-deftest py-ert-py-end-of-except-block-bol-functionp-test ()
  (should (functionp 'py-end-of-except-block-bol)))

(ert-deftest py-ert-py-mark-paragraph-functionp-test ()
  (should (functionp 'py-mark-paragraph)))

(ert-deftest py-ert-py-mark-block-functionp-test ()
  (should (functionp 'py-mark-block)))

(ert-deftest py-ert-py-mark-minor-block-functionp-test ()
  (should (functionp 'py-mark-minor-block)))

(ert-deftest py-ert-py-mark-clause-functionp-test ()
  (should (functionp 'py-mark-clause)))

(ert-deftest py-ert-py-mark-block-or-clause-functionp-test ()
  (should (functionp 'py-mark-block-or-clause)))

(ert-deftest py-ert-py-mark-def-functionp-test ()
  (should (functionp 'py-mark-def)))

(ert-deftest py-ert-py-mark-class-functionp-test ()
  (should (functionp 'py-mark-class)))

(ert-deftest py-ert-py-mark-def-or-class-functionp-test ()
  (should (functionp 'py-mark-def-or-class)))

(ert-deftest py-ert-py-mark-line-functionp-test ()
  (should (functionp 'py-mark-line)))

(ert-deftest py-ert-py-mark-statement-functionp-test ()
  (should (functionp 'py-mark-statement)))

(ert-deftest py-ert-py-mark-comment-functionp-test ()
  (should (functionp 'py-mark-comment)))

(ert-deftest py-ert-py-mark-top-level-functionp-test ()
  (should (functionp 'py-mark-top-level)))

(ert-deftest py-ert-py-mark-partial-expression-functionp-test ()
  (should (functionp 'py-mark-partial-expression)))

(ert-deftest py-ert-py-mark-expression-functionp-test ()
  (should (functionp 'py-mark-expression)))

(ert-deftest py-ert-py-mark-paragraph-bol-functionp-test ()
  (should (functionp 'py-mark-paragraph-bol)))

(ert-deftest py-ert-py-mark-block-bol-functionp-test ()
  (should (functionp 'py-mark-block-bol)))

(ert-deftest py-ert-py-mark-minor-block-bol-functionp-test ()
  (should (functionp 'py-mark-minor-block-bol)))

(ert-deftest py-ert-py-mark-clause-bol-functionp-test ()
  (should (functionp 'py-mark-clause-bol)))

(ert-deftest py-ert-py-mark-block-or-clause-bol-functionp-test ()
  (should (functionp 'py-mark-block-or-clause-bol)))

(ert-deftest py-ert-py-mark-def-bol-functionp-test ()
  (should (functionp 'py-mark-def-bol)))

(ert-deftest py-ert-py-mark-class-bol-functionp-test ()
  (should (functionp 'py-mark-class-bol)))

(ert-deftest py-ert-py-mark-def-or-class-bol-functionp-test ()
  (should (functionp 'py-mark-def-or-class-bol)))

(ert-deftest py-ert-py-mark-line-bol-functionp-test ()
  (should (functionp 'py-mark-line-bol)))

(ert-deftest py-ert-py-mark-statement-bol-functionp-test ()
  (should (functionp 'py-mark-statement-bol)))

(ert-deftest py-ert-py-mark-comment-bol-functionp-test ()
  (should (functionp 'py-mark-comment-bol)))

(ert-deftest py-ert-py-mark-top-level-bol-functionp-test ()
  (should (functionp 'py-mark-top-level-bol)))

(ert-deftest py-ert-py-mark-partial-expression-bol-functionp-test ()
  (should (functionp 'py-mark-partial-expression-bol)))

(ert-deftest py-ert-py-mark-expression-bol-functionp-test ()
  (should (functionp 'py-mark-expression-bol)))

(ert-deftest py-ert-py--kill-emacs-hook-functionp-test ()
  (should (functionp 'py--kill-emacs-hook)))

(ert-deftest py-ert-py-python-version-functionp-test ()
  (should (functionp 'py-python-version)))

(ert-deftest py-ert-py-version-functionp-test ()
  (should (functionp 'py-version)))

(ert-deftest py-ert-py-history-input-filter-functionp-test ()
  (should (functionp 'py-history-input-filter)))

(ert-deftest py-ert-py-load-file-functionp-test ()
  (should (functionp 'py-load-file)))

(ert-deftest py-ert-py-proc-functionp-test ()
  (should (functionp 'py-proc)))

(ert-deftest py-ert-py--shell-simple-send-functionp-test ()
  (should (functionp 'py--shell-simple-send)))

(ert-deftest py-ert-py-guess-pdb-path-functionp-test ()
  (should (functionp 'py-guess-pdb-path)))

(ert-deftest py-ert-py-switch-shell-functionp-test ()
  (should (functionp 'py-switch-shell)))

(ert-deftest py-ert-py-toggle-local-default-use-functionp-test ()
  (should (functionp 'py-toggle-local-default-use)))

(ert-deftest py-ert-py--input-filter-functionp-test ()
  (should (functionp 'py--input-filter)))

(ert-deftest py-ert-py--set-auto-fill-values-functionp-test ()
  (should (functionp 'py--set-auto-fill-values)))

(ert-deftest py-ert-py--run-auto-fill-timer-functionp-test ()
  (should (functionp 'py--run-auto-fill-timer)))

(ert-deftest py-ert-py--unfontify-banner-intern-functionp-test ()
  (should (functionp 'py--unfontify-banner-intern)))

(ert-deftest py-ert-py--unfontify-banner-functionp-test ()
  (should (functionp 'py--unfontify-banner)))

(ert-deftest py-ert-py--run-unfontify-timer-functionp-test ()
  (should (functionp 'py--run-unfontify-timer)))

(ert-deftest py-ert-py-complete-auto-functionp-test ()
  (should (functionp 'py-complete-auto)))

(ert-deftest py-ert-py-set-command-args-functionp-test ()
  (should (functionp 'py-set-command-args)))

(ert-deftest py-ert-py---emacs-version-greater-23-functionp-test ()
  (should (functionp 'py---emacs-version-greater-23)))

(ert-deftest py-ert-py-beginning-of-commented-section-functionp-test ()
  (should (functionp 'py-beginning-of-commented-section)))

(ert-deftest py-ert-py--empty-arglist-indent-functionp-test ()
  (should (functionp 'py--empty-arglist-indent)))

(ert-deftest py-ert-py-symbol-at-point-functionp-test ()
  (should (functionp 'py-symbol-at-point)))

(ert-deftest py-ert-py-kill-buffer-unconditional-functionp-test ()
  (should (functionp 'py-kill-buffer-unconditional)))

(ert-deftest py-ert-py--line-backward-maybe-functionp-test ()
  (should (functionp 'py--line-backward-maybe)))

(ert-deftest py-ert-py--after-empty-line-functionp-test ()
  (should (functionp 'py--after-empty-line)))

(ert-deftest py-ert-py-compute-indentation-functionp-test ()
  (should (functionp 'py-compute-indentation)))

(ert-deftest py-ert-py--fetch-previous-indent-functionp-test ()
  (should (functionp 'py--fetch-previous-indent)))

(ert-deftest py-ert-py-continuation-offset-functionp-test ()
  (should (functionp 'py-continuation-offset)))

(ert-deftest py-ert-py-indentation-of-statement-functionp-test ()
  (should (functionp 'py-indentation-of-statement)))

(ert-deftest py-ert-py-list-beginning-position-functionp-test ()
  (should (functionp 'py-list-beginning-position)))

(ert-deftest py-ert-py-end-of-list-position-functionp-test ()
  (should (functionp 'py-end-of-list-position)))

(ert-deftest py-ert-py--in-comment-p-functionp-test ()
  (should (functionp 'py--in-comment-p)))

(ert-deftest py-ert-py-in-triplequoted-string-p-functionp-test ()
  (should (functionp 'py-in-triplequoted-string-p)))

(ert-deftest py-ert-py-in-string-p-functionp-test ()
  (should (functionp 'py-in-string-p)))

(ert-deftest py-ert-py-in-statement-p-functionp-test ()
  (should (functionp 'py-in-statement-p)))

(ert-deftest py-ert-py-beginning-of-top-level-p-functionp-test ()
  (should (functionp 'py-beginning-of-top-level-p)))

(ert-deftest py-ert-py--beginning-of-line-p-functionp-test ()
  (should (functionp 'py--beginning-of-line-p)))

(ert-deftest py-ert-py--beginning-of-buffer-p-functionp-test ()
  (should (functionp 'py--beginning-of-buffer-p)))

(ert-deftest py-ert-py--beginning-of-paragraph-p-functionp-test ()
  (should (functionp 'py--beginning-of-paragraph-p)))

(ert-deftest py-ert-py--end-of-line-p-functionp-test ()
  (should (functionp 'py--end-of-line-p)))

(ert-deftest py-ert-py--end-of-paragraph-p-functionp-test ()
  (should (functionp 'py--end-of-paragraph-p)))

(ert-deftest py-ert-py--statement-opens-block-p-functionp-test ()
  (should (functionp 'py--statement-opens-block-p)))

(ert-deftest py-ert-py--statement-opens-base-functionp-test ()
  (should (functionp 'py--statement-opens-base)))

(ert-deftest py-ert-py--statement-opens-clause-p-functionp-test ()
  (should (functionp 'py--statement-opens-clause-p)))

(ert-deftest py-ert-py--statement-opens-block-or-clause-p-functionp-test ()
  (should (functionp 'py--statement-opens-block-or-clause-p)))

(ert-deftest py-ert-py--statement-opens-class-p-functionp-test ()
  (should (functionp 'py--statement-opens-class-p)))

(ert-deftest py-ert-py--statement-opens-def-p-functionp-test ()
  (should (functionp 'py--statement-opens-def-p)))

(ert-deftest py-ert-py--statement-opens-def-or-class-p-functionp-test ()
  (should (functionp 'py--statement-opens-def-or-class-p)))

(ert-deftest py-ert-py--record-list-error-functionp-test ()
  (should (functionp 'py--record-list-error)))

(ert-deftest py-ert-py--message-error-functionp-test ()
  (should (functionp 'py--message-error)))

(ert-deftest py-ert-py--end-base-functionp-test ()
  (should (functionp 'py--end-base)))

(ert-deftest py-ert-py--look-downward-for-beginning-functionp-test ()
  (should (functionp 'py--look-downward-for-beginning)))

(ert-deftest py-ert-py-look-downward-for-clause-functionp-test ()
  (should (functionp 'py-look-downward-for-clause)))

(ert-deftest py-ert-py-current-defun-functionp-test ()
  (should (functionp 'py-current-defun)))

(ert-deftest py-ert-py-sort-imports-functionp-test ()
  (should (functionp 'py-sort-imports)))

(ert-deftest py-ert-py--in-literal-functionp-test ()
  (should (functionp 'py--in-literal)))

(ert-deftest py-ert-py-count-lines-functionp-test ()
  (should (functionp 'py-count-lines)))

(ert-deftest py-ert-py-which-function-functionp-test ()
  (should (functionp 'py-which-function)))

(ert-deftest py-ert-py--point-functionp-test ()
  (should (functionp 'py--point)))

(ert-deftest py-ert-py-install-search-local-functionp-test ()
  (should (functionp 'py-install-search-local)))

(ert-deftest py-ert-py-install-local-shells-functionp-test ()
  (should (functionp 'py-install-local-shells)))

(ert-deftest py-ert-py-end-of-string-functionp-test ()
  (should (functionp 'py-end-of-string)))

(ert-deftest py-ert-py--until-found-functionp-test ()
  (should (functionp 'py--until-found)))

(ert-deftest py-ert-py--delay-process-dependent-functionp-test ()
  (should (functionp 'py--delay-process-dependent)))

(ert-deftest py-ert-py--send-string-no-output-functionp-test ()
  (should (functionp 'py--send-string-no-output)))

(ert-deftest py-ert-py--send-string-return-output-functionp-test ()
  (should (functionp 'py--send-string-return-output)))

(ert-deftest py-ert-py-which-def-or-class-functionp-test ()
  (should (functionp 'py-which-def-or-class)))

(ert-deftest py-ert-py--beginning-of-form-intern-functionp-test ()
  (should (functionp 'py--beginning-of-form-intern)))

(ert-deftest py-ert-py--beginning-of-prepare-functionp-test ()
  (should (functionp 'py--beginning-of-prepare)))

(ert-deftest py-ert-py--fetch-first-python-buffer-functionp-test ()
  (should (functionp 'py--fetch-first-python-buffer)))

(ert-deftest py-ert-py-unload-python-el-functionp-test ()
  (should (functionp 'py-unload-python-el)))

(ert-deftest py-ert-py--skip-to-semicolon-backward-functionp-test ()
  (should (functionp 'py--skip-to-semicolon-backward)))

(ert-deftest py-ert-py--eos-in-string-functionp-test ()
  (should (functionp 'py--eos-in-string)))

(ert-deftest py-ert-py--end-of-comment-intern-functionp-test ()
  (should (functionp 'py--end-of-comment-intern)))

(ert-deftest py-ert-py--skip-to-comment-or-semicolon-functionp-test ()
  (should (functionp 'py--skip-to-comment-or-semicolon)))

(ert-deftest py-ert-py-beginning-of-top-level-functionp-test ()
  (should (functionp 'py-beginning-of-top-level)))

(ert-deftest py-ert-py-end-of-top-level-functionp-test ()
  (should (functionp 'py-end-of-top-level)))

(ert-deftest py-ert-py-end-of-top-level-bol-functionp-test ()
  (should (functionp 'py-end-of-top-level-bol)))

(ert-deftest py-ert-py-up-functionp-test ()
  (should (functionp 'py-up)))

(ert-deftest py-ert-py-down-functionp-test ()
  (should (functionp 'py-down)))

(ert-deftest py-ert-py--beginning-of-line-form-functionp-test ()
  (should (functionp 'py--beginning-of-line-form)))

(ert-deftest py-ert-py--mark-base-functionp-test ()
  (should (functionp 'py--mark-base)))

(ert-deftest py-ert-py--mark-base-bol-functionp-test ()
  (should (functionp 'py--mark-base-bol)))

(ert-deftest py-ert-py-mark-base-functionp-test ()
  (should (functionp 'py-mark-base)))

(ert-deftest py-ert-py-beginning-functionp-test ()
  (should (functionp 'py-beginning)))

(ert-deftest py-ert-py-end-functionp-test ()
  (should (functionp 'py-end)))

(ert-deftest py-ert-py-beginning-of-buffer-functionp-test ()
  (should (functionp 'py-beginning-of-buffer)))

(ert-deftest py-ert-py-end-of-buffer-functionp-test ()
  (should (functionp 'py-end-of-buffer)))

(ert-deftest py-ert-py-backward-same-level-functionp-test ()
  (should (functionp 'py-backward-same-level)))

(ert-deftest py-ert-py--end-of-line-p-functionp-test ()
  (should (functionp 'py--end-of-line-p)))

(ert-deftest py-ert-py--end-of-buffer-p-functionp-test ()
  (should (functionp 'py--end-of-buffer-p)))

(ert-deftest py-ert-py--bounds-of-region-functionp-test ()
  (should (functionp 'py--bounds-of-region)))

(ert-deftest py-ert-py--beginning-of-paragraph-position-functionp-test ()
  (should (functionp 'py--beginning-of-paragraph-position)))

(ert-deftest py-ert-py--end-of-paragraph-position-functionp-test ()
  (should (functionp 'py--end-of-paragraph-position)))

(ert-deftest py-ert-py--beginning-of-comment-position-functionp-test ()
  (should (functionp 'py--beginning-of-comment-position)))

(ert-deftest py-ert-py--end-of-comment-position-functionp-test ()
  (should (functionp 'py--end-of-comment-position)))

(ert-deftest py-ert-py-info-lookup-symbol-functionp-test ()
  (should (functionp 'py-info-lookup-symbol)))

(ert-deftest py-ert-python-after-info-look-functionp-test ()
  (should (functionp 'python-after-info-look)))

(ert-deftest py-ert-py--warn-tmp-files-left-functionp-test ()
  (should (functionp 'py--warn-tmp-files-left)))

(ert-deftest py-ert-py-fetch-docu-functionp-test ()
  (should (functionp 'py-fetch-docu)))

(ert-deftest py-ert-py-info-current-defun-functionp-test ()
  (should (functionp 'py-info-current-defun)))

(ert-deftest py-ert-py-help-at-point-functionp-test ()
  (should (functionp 'py-help-at-point)))

(ert-deftest py-ert-py--dump-help-string-functionp-test ()
  (should (functionp 'py--dump-help-string)))

(ert-deftest py-ert-py-describe-mode-functionp-test ()
  (should (functionp 'py-describe-mode)))

(ert-deftest py-ert-py-find-definition-functionp-test ()
  (should (functionp 'py-find-definition)))

(ert-deftest py-ert-py-find-imports-functionp-test ()
  (should (functionp 'py-find-imports)))

(ert-deftest py-ert-py-update-imports-functionp-test ()
  (should (functionp 'py-update-imports)))

(ert-deftest py-ert-py-pep8-run-functionp-test ()
  (should (functionp 'py-pep8-run)))

(ert-deftest py-ert-py-pep8-help-functionp-test ()
  (should (functionp 'py-pep8-help)))

(ert-deftest py-ert-py-pylint-run-functionp-test ()
  (should (functionp 'py-pylint-run)))

(ert-deftest py-ert-py-pylint-help-functionp-test ()
  (should (functionp 'py-pylint-help)))

(ert-deftest py-ert-py-pylint-doku-functionp-test ()
  (should (functionp 'py-pylint-doku)))

(ert-deftest py-ert-py-pyflakes-run-functionp-test ()
  (should (functionp 'py-pyflakes-run)))

(ert-deftest py-ert-py-pyflakes-help-functionp-test ()
  (should (functionp 'py-pyflakes-help)))

(ert-deftest py-ert-py-pyflakespep8-run-functionp-test ()
  (should (functionp 'py-pyflakespep8-run)))

(ert-deftest py-ert-py-pyflakespep8-help-functionp-test ()
  (should (functionp 'py-pyflakespep8-help)))

(ert-deftest py-ert-py-pychecker-run-functionp-test ()
  (should (functionp 'py-pychecker-run)))

(ert-deftest py-ert-py-check-command-functionp-test ()
  (should (functionp 'py-check-command)))

(ert-deftest py-ert-py-flake8-run-functionp-test ()
  (should (functionp 'py-flake8-run)))

(ert-deftest py-ert-py-flake8-help-functionp-test ()
  (should (functionp 'py-flake8-help)))

(ert-deftest py-ert-py--string-strip-functionp-test ()
  (should (functionp 'py--string-strip)))

(ert-deftest py-ert-py-nesting-level-functionp-test ()
  (should (functionp 'py-nesting-level)))

(ert-deftest py-ert-py-ffap-module-path-functionp-test ()
  (should (functionp 'py-ffap-module-path)))

(ert-deftest py-ert-py-toggle-flymake-intern-functionp-test ()
  (should (functionp 'py-toggle-flymake-intern)))

(ert-deftest py-ert-pylint-flymake-mode-functionp-test ()
  (should (functionp 'pylint-flymake-mode)))

(ert-deftest py-ert-pyflakes-flymake-mode-functionp-test ()
  (should (functionp 'pyflakes-flymake-mode)))

(ert-deftest py-ert-pychecker-flymake-mode-functionp-test ()
  (should (functionp 'pychecker-flymake-mode)))

(ert-deftest py-ert-pep8-flymake-mode-functionp-test ()
  (should (functionp 'pep8-flymake-mode)))

(ert-deftest py-ert-pyflakespep8-flymake-mode-functionp-test ()
  (should (functionp 'pyflakespep8-flymake-mode)))

(ert-deftest py-ert-variables-state-functionp-test ()
  (should (functionp 'variables-state)))

(ert-deftest py-ert-variables-base-state-functionp-test ()
  (should (functionp 'variables-base-state)))

(ert-deftest py-ert-py-kill-block-functionp-test ()
  (should (functionp 'py-kill-block)))

(ert-deftest py-ert-py-kill-clause-functionp-test ()
  (should (functionp 'py-kill-clause)))

(ert-deftest py-ert-py-kill-block-or-clause-functionp-test ()
  (should (functionp 'py-kill-block-or-clause)))

(ert-deftest py-ert-py-kill-def-functionp-test ()
  (should (functionp 'py-kill-def)))

(ert-deftest py-ert-py-kill-class-functionp-test ()
  (should (functionp 'py-kill-class)))

(ert-deftest py-ert-py-kill-def-or-class-functionp-test ()
  (should (functionp 'py-kill-def-or-class)))

(ert-deftest py-ert-py-kill-if-block-functionp-test ()
  (should (functionp 'py-kill-if-block)))

(ert-deftest py-ert-py-kill-try-block-functionp-test ()
  (should (functionp 'py-kill-try-block)))

(ert-deftest py-ert-py-kill-minor-block-functionp-test ()
  (should (functionp 'py-kill-minor-block)))

(ert-deftest py-ert-py-kill-for-block-functionp-test ()
  (should (functionp 'py-kill-for-block)))

(ert-deftest py-ert-py-kill-top-level-functionp-test ()
  (should (functionp 'py-kill-top-level)))

(ert-deftest py-ert-py-kill-statement-functionp-test ()
  (should (functionp 'py-kill-statement)))

(ert-deftest py-ert-py-kill-expression-functionp-test ()
  (should (functionp 'py-kill-expression)))

(ert-deftest py-ert-py-kill-partial-expression-functionp-test ()
  (should (functionp 'py-kill-partial-expression)))

(ert-deftest py-ert-py-kill-block-bol-functionp-test ()
  (should (functionp 'py-kill-block-bol)))

(ert-deftest py-ert-py-kill-clause-bol-functionp-test ()
  (should (functionp 'py-kill-clause-bol)))

(ert-deftest py-ert-py-kill-block-or-clause-bol-functionp-test ()
  (should (functionp 'py-kill-block-or-clause-bol)))

(ert-deftest py-ert-py-kill-def-bol-functionp-test ()
  (should (functionp 'py-kill-def-bol)))

(ert-deftest py-ert-py-kill-class-bol-functionp-test ()
  (should (functionp 'py-kill-class-bol)))

(ert-deftest py-ert-py-kill-def-or-class-bol-functionp-test ()
  (should (functionp 'py-kill-def-or-class-bol)))

(ert-deftest py-ert-py-kill-if-block-bol-functionp-test ()
  (should (functionp 'py-kill-if-block-bol)))

(ert-deftest py-ert-py-kill-try-block-bol-functionp-test ()
  (should (functionp 'py-kill-try-block-bol)))

(ert-deftest py-ert-py-kill-minor-block-bol-functionp-test ()
  (should (functionp 'py-kill-minor-block-bol)))

(ert-deftest py-ert-py-kill-for-block-bol-functionp-test ()
  (should (functionp 'py-kill-for-block-bol)))

(ert-deftest py-ert-py-kill-top-level-bol-functionp-test ()
  (should (functionp 'py-kill-top-level-bol)))

(ert-deftest py-ert-py-kill-statement-bol-functionp-test ()
  (should (functionp 'py-kill-statement-bol)))

(ert-deftest py-ert-py--bounds-of-statement-functionp-test ()
  (should (functionp 'py--bounds-of-statement)))

(ert-deftest py-ert-py--bounds-of-block-functionp-test ()
  (should (functionp 'py--bounds-of-block)))

(ert-deftest py-ert-py--bounds-of-clause-functionp-test ()
  (should (functionp 'py--bounds-of-clause)))

(ert-deftest py-ert-py--bounds-of-block-or-clause-functionp-test ()
  (should (functionp 'py--bounds-of-block-or-clause)))

(ert-deftest py-ert-py--bounds-of-def-functionp-test ()
  (should (functionp 'py--bounds-of-def)))

(ert-deftest py-ert-py--bounds-of-class-functionp-test ()
  (should (functionp 'py--bounds-of-class)))

(ert-deftest py-ert-py--bounds-of-def-or-class-functionp-test ()
  (should (functionp 'py--bounds-of-def-or-class)))

(ert-deftest py-ert-py--bounds-of-buffer-functionp-test ()
  (should (functionp 'py--bounds-of-buffer)))

(ert-deftest py-ert-py--bounds-of-expression-functionp-test ()
  (should (functionp 'py--bounds-of-expression)))

(ert-deftest py-ert-py--bounds-of-partial-expression-functionp-test ()
  (should (functionp 'py--bounds-of-partial-expression)))

(ert-deftest py-ert-py--bounds-of-minor-block-functionp-test ()
  (should (functionp 'py--bounds-of-minor-block)))

(ert-deftest py-ert-py--bounds-of-if-block-functionp-test ()
  (should (functionp 'py--bounds-of-if-block)))

(ert-deftest py-ert-py--bounds-of-try-block-functionp-test ()
  (should (functionp 'py--bounds-of-try-block)))

(ert-deftest py-ert-py--bounds-of-except-block-functionp-test ()
  (should (functionp 'py--bounds-of-except-block)))

(ert-deftest py-ert-py--bounds-of-top-level-functionp-test ()
  (should (functionp 'py--bounds-of-top-level)))

(ert-deftest py-ert-py--bounds-of-statement-bol-functionp-test ()
  (should (functionp 'py--bounds-of-statement-bol)))

(ert-deftest py-ert-py--bounds-of-block-bol-functionp-test ()
  (should (functionp 'py--bounds-of-block-bol)))

(ert-deftest py-ert-py--bounds-of-clause-bol-functionp-test ()
  (should (functionp 'py--bounds-of-clause-bol)))

(ert-deftest py-ert-py--bounds-of-block-or-clause-bol-functionp-test ()
  (should (functionp 'py--bounds-of-block-or-clause-bol)))

(ert-deftest py-ert-py--bounds-of-def-bol-functionp-test ()
  (should (functionp 'py--bounds-of-def-bol)))

(ert-deftest py-ert-py--bounds-of-class-bol-functionp-test ()
  (should (functionp 'py--bounds-of-class-bol)))

(ert-deftest py-ert-py--bounds-of-minor-block-bol-functionp-test ()
  (should (functionp 'py--bounds-of-minor-block-bol)))

(ert-deftest py-ert-py--bounds-of-if-block-bol-functionp-test ()
  (should (functionp 'py--bounds-of-if-block-bol)))

(ert-deftest py-ert-py--bounds-of-try-block-bol-functionp-test ()
  (should (functionp 'py--bounds-of-try-block-bol)))

(ert-deftest py-ert-py-smart-operator-check-functionp-test ()
  (should (functionp 'py-smart-operator-check)))

(ert-deftest py-ert-py-autopair-check-functionp-test ()
  (should (functionp 'py-autopair-check)))

(ert-deftest py-ert-py--set-ffap-form-functionp-test ()
  (should (functionp 'py--set-ffap-form)))

(ert-deftest py-ert-py--quote-syntax-functionp-test ()
  (should (functionp 'py--quote-syntax)))

(ert-deftest py-ert-py--delete-all-but-first-prompt-functionp-test ()
  (should (functionp 'py--delete-all-but-first-prompt)))

(ert-deftest py-ert-py--python-send-setup-code-intern-functionp-test ()
  (should (functionp 'py--python-send-setup-code-intern)))

(ert-deftest py-ert-py--python-send-completion-setup-code-functionp-test ()
  (should (functionp 'py--python-send-completion-setup-code)))

(ert-deftest py-ert-py--python-send-ffap-setup-code-functionp-test ()
  (should (functionp 'py--python-send-ffap-setup-code)))

(ert-deftest py-ert-py--python-send-eldoc-setup-code-functionp-test ()
  (should (functionp 'py--python-send-eldoc-setup-code)))

(ert-deftest py-ert-py--ipython-import-module-completion-functionp-test ()
  (should (functionp 'py--ipython-import-module-completion)))

(ert-deftest py-ert-py--docstring-p-functionp-test ()
  (should (functionp 'py--docstring-p)))

(ert-deftest py-ert-py--font-lock-syntactic-face-function-functionp-test ()
  (should (functionp 'py--font-lock-syntactic-face-function)))

(ert-deftest py-ert-py-choose-shell-by-shebang-functionp-test ()
  (should (functionp 'py-choose-shell-by-shebang)))

(ert-deftest py-ert-py--choose-shell-by-import-functionp-test ()
  (should (functionp 'py--choose-shell-by-import)))

(ert-deftest py-ert-py-choose-shell-by-path-functionp-test ()
  (should (functionp 'py-choose-shell-by-path)))

(ert-deftest py-ert-py-which-python-functionp-test ()
  (should (functionp 'py-which-python)))

(ert-deftest py-ert-py-python-current-environment-functionp-test ()
  (should (functionp 'py-python-current-environment)))

(ert-deftest py-ert-py--cleanup-process-name-functionp-test ()
  (should (functionp 'py--cleanup-process-name)))

(ert-deftest py-ert-py-choose-shell-functionp-test ()
  (should (functionp 'py-choose-shell)))

(ert-deftest py-ert-py--normalize-directory-functionp-test ()
  (should (functionp 'py--normalize-directory)))

(ert-deftest py-ert-py-install-directory-check-functionp-test ()
  (should (functionp 'py-install-directory-check)))

(ert-deftest py-ert-py-guess-py-install-directory-functionp-test ()
  (should (functionp 'py-guess-py-install-directory)))

(ert-deftest py-ert-py-load-pymacs-functionp-test ()
  (should (functionp 'py-load-pymacs)))

(ert-deftest py-ert-py-set-load-path-functionp-test ()
  (should (functionp 'py-set-load-path)))

(ert-deftest py-ert-py-separator-char-functionp-test ()
  (should (functionp 'py-separator-char)))

(ert-deftest py-ert-pps-emacs-version-functionp-test ()
  (should (functionp 'pps-emacs-version)))

(ert-deftest py-ert-py-in-string-or-comment-p-functionp-test ()
  (should (functionp 'py-in-string-or-comment-p)))

(ert-deftest py-ert-py-electric-colon-functionp-test ()
  (should (functionp 'py-electric-colon)))

(ert-deftest py-ert-py-electric-space-functionp-test ()
  (should (functionp 'py-electric-space)))

(ert-deftest py-ert-py-electric-comment-functionp-test ()
  (should (functionp 'py-electric-comment)))

(ert-deftest py-ert-py-empty-out-list-backward-functionp-test ()
  (should (functionp 'py-empty-out-list-backward)))

(ert-deftest py-ert-py-electric-backspace-functionp-test ()
  (should (functionp 'py-electric-backspace)))

(ert-deftest py-ert-py-electric-delete-functionp-test ()
  (should (functionp 'py-electric-delete)))

(ert-deftest py-ert-py-electric-yank-functionp-test ()
  (should (functionp 'py-electric-yank)))

(ert-deftest py-ert-py--beginning-of-statement-position-functionp-test ()
  (should (functionp 'py--beginning-of-statement-position)))

(ert-deftest py-ert-py--beginning-of-block-position-functionp-test ()
  (should (functionp 'py--beginning-of-block-position)))

(ert-deftest py-ert-py--beginning-of-clause-position-functionp-test ()
  (should (functionp 'py--beginning-of-clause-position)))

(ert-deftest py-ert-py--beginning-of-block-or-clause-position-functionp-test ()
  (should (functionp 'py--beginning-of-block-or-clause-position)))

(ert-deftest py-ert-py--beginning-of-def-position-functionp-test ()
  (should (functionp 'py--beginning-of-def-position)))

(ert-deftest py-ert-py--beginning-of-class-position-functionp-test ()
  (should (functionp 'py--beginning-of-class-position)))

(ert-deftest py-ert-py--beginning-of-def-or-class-position-functionp-test ()
  (should (functionp 'py--beginning-of-def-or-class-position)))

(ert-deftest py-ert-py--beginning-of-expression-position-functionp-test ()
  (should (functionp 'py--beginning-of-expression-position)))

(ert-deftest py-ert-py--beginning-of-partial-expression-position-functionp-test ()
  (should (functionp 'py--beginning-of-partial-expression-position)))

(ert-deftest py-ert-py--beginning-of-minor-block-position-functionp-test ()
  (should (functionp 'py--beginning-of-minor-block-position)))

(ert-deftest py-ert-py--beginning-of-if-block-position-functionp-test ()
  (should (functionp 'py--beginning-of-if-block-position)))

(ert-deftest py-ert-py--beginning-of-try-block-position-functionp-test ()
  (should (functionp 'py--beginning-of-try-block-position)))

(ert-deftest py-ert-py--beginning-of-except-block-position-functionp-test ()
  (should (functionp 'py--beginning-of-except-block-position)))

(ert-deftest py-ert-py--beginning-of-top-level-position-functionp-test ()
  (should (functionp 'py--beginning-of-top-level-position)))

(ert-deftest py-ert-py--beginning-of-statement-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-statement-position-bol)))

(ert-deftest py-ert-py--beginning-of-block-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-block-position-bol)))

(ert-deftest py-ert-py--beginning-of-clause-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-clause-position-bol)))

(ert-deftest py-ert-py--beginning-of-block-or-clause-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-block-or-clause-position-bol)))

(ert-deftest py-ert-py--beginning-of-def-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-def-position-bol)))

(ert-deftest py-ert-py--beginning-of-class-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-class-position-bol)))

(ert-deftest py-ert-py--beginning-of-def-or-class-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-def-or-class-position-bol)))

(ert-deftest py-ert-py--beginning-of-minor-block-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-minor-block-position-bol)))

(ert-deftest py-ert-py--beginning-of-if-block-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-if-block-position-bol)))

(ert-deftest py-ert-py--beginning-of-try-block-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-try-block-position-bol)))

(ert-deftest py-ert-py--beginning-of-except-block-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-except-block-position-bol)))

(ert-deftest py-ert-py--beginning-of-top-level-position-bol-functionp-test ()
  (should (functionp 'py--beginning-of-top-level-position-bol)))

(ert-deftest py-ert-py-beginning-of-comment-functionp-test ()
  (should (functionp 'py-beginning-of-comment)))

(ert-deftest py-ert-py-end-of-comment-functionp-test ()
  (should (functionp 'py-end-of-comment)))

(ert-deftest py-ert-py--uncomment-intern-functionp-test ()
  (should (functionp 'py--uncomment-intern)))

(ert-deftest py-ert-py-uncomment-functionp-test ()
  (should (functionp 'py-uncomment)))

(ert-deftest py-ert-py-comment-region-functionp-test ()
  (should (functionp 'py-comment-region)))

(ert-deftest py-ert-py-comment-block-functionp-test ()
  (should (functionp 'py-comment-block)))

(ert-deftest py-ert-py-comment-minor-block-functionp-test ()
  (should (functionp 'py-comment-minor-block)))

(ert-deftest py-ert-py-comment-top-level-functionp-test ()
  (should (functionp 'py-comment-top-level)))

(ert-deftest py-ert-py-comment-clause-functionp-test ()
  (should (functionp 'py-comment-clause)))

(ert-deftest py-ert-py-comment-block-or-clause-functionp-test ()
  (should (functionp 'py-comment-block-or-clause)))

(ert-deftest py-ert-py-comment-def-functionp-test ()
  (should (functionp 'py-comment-def)))

(ert-deftest py-ert-py-comment-class-functionp-test ()
  (should (functionp 'py-comment-class)))

(ert-deftest py-ert-py-comment-def-or-class-functionp-test ()
  (should (functionp 'py-comment-def-or-class)))

(ert-deftest py-ert-py-comment-statement-functionp-test ()
  (should (functionp 'py-comment-statement)))

(ert-deftest py-ert-py-delete-statement-functionp-test ()
  (should (functionp 'py-delete-statement)))

(ert-deftest py-ert-py-delete-top-level-functionp-test ()
  (should (functionp 'py-delete-top-level)))

(ert-deftest py-ert-py-delete-block-functionp-test ()
  (should (functionp 'py-delete-block)))

(ert-deftest py-ert-py-delete-clause-functionp-test ()
  (should (functionp 'py-delete-clause)))

(ert-deftest py-ert-py-delete-block-or-clause-functionp-test ()
  (should (functionp 'py-delete-block-or-clause)))

(ert-deftest py-ert-py-delete-def-functionp-test ()
  (should (functionp 'py-delete-def)))

(ert-deftest py-ert-py-delete-class-functionp-test ()
  (should (functionp 'py-delete-class)))

(ert-deftest py-ert-py-delete-def-or-class-functionp-test ()
  (should (functionp 'py-delete-def-or-class)))

(ert-deftest py-ert-py-delete-expression-functionp-test ()
  (should (functionp 'py-delete-expression)))

(ert-deftest py-ert-py-delete-partial-expression-functionp-test ()
  (should (functionp 'py-delete-partial-expression)))

(ert-deftest py-ert-py-delete-minor-block-functionp-test ()
  (should (functionp 'py-delete-minor-block)))

(ert-deftest py-ert-py-delete-paragraph-bol-functionp-test ()
  (should (functionp 'py-delete-paragraph-bol)))

(ert-deftest py-ert-py-delete-block-bol-functionp-test ()
  (should (functionp 'py-delete-block-bol)))

(ert-deftest py-ert-py-delete-minor-block-bol-functionp-test ()
  (should (functionp 'py-delete-minor-block-bol)))

(ert-deftest py-ert-py-delete-clause-bol-functionp-test ()
  (should (functionp 'py-delete-clause-bol)))

(ert-deftest py-ert-py-delete-block-or-clause-bol-functionp-test ()
  (should (functionp 'py-delete-block-or-clause-bol)))

(ert-deftest py-ert-py-delete-def-bol-functionp-test ()
  (should (functionp 'py-delete-def-bol)))

(ert-deftest py-ert-py-delete-class-bol-functionp-test ()
  (should (functionp 'py-delete-class-bol)))

(ert-deftest py-ert-py-delete-def-or-class-bol-functionp-test ()
  (should (functionp 'py-delete-def-or-class-bol)))

(ert-deftest py-ert-py-delete-statement-bol-functionp-test ()
  (should (functionp 'py-delete-statement-bol)))

(ert-deftest py-ert-py-switch-imenu-index-function-functionp-test ()
  (should (functionp 'py-switch-imenu-index-function)))

(ert-deftest py-ert-py--imenu-create-index-functionp-test ()
  (should (functionp 'py--imenu-create-index)))

(ert-deftest py-ert-py--imenu-create-index-engine-functionp-test ()
  (should (functionp 'py--imenu-create-index-engine)))

(ert-deftest py-ert-py--imenu-create-index-new-intern-functionp-test ()
  (should (functionp 'py--imenu-create-index-new-intern)))

(ert-deftest py-ert-py--imenu-create-index-new-functionp-test ()
  (should (functionp 'py--imenu-create-index-new)))

(ert-deftest py-ert-py-execute-file-python-functionp-test ()
  (should (functionp 'py-execute-file-python)))

(ert-deftest py-ert-py-execute-file-python-switch-functionp-test ()
  (should (functionp 'py-execute-file-python-switch)))

(ert-deftest py-ert-py-execute-file-python-no-switch-functionp-test ()
  (should (functionp 'py-execute-file-python-no-switch)))

(ert-deftest py-ert-py-execute-file-python-dedicated-functionp-test ()
  (should (functionp 'py-execute-file-python-dedicated)))

(ert-deftest py-ert-py-execute-file-python-dedicated-switch-functionp-test ()
  (should (functionp 'py-execute-file-python-dedicated-switch)))

(ert-deftest py-ert-py-execute-file-ipython-functionp-test ()
  (should (functionp 'py-execute-file-ipython)))

(ert-deftest py-ert-py-execute-file-ipython-switch-functionp-test ()
  (should (functionp 'py-execute-file-ipython-switch)))

(ert-deftest py-ert-py-execute-file-ipython-no-switch-functionp-test ()
  (should (functionp 'py-execute-file-ipython-no-switch)))

(ert-deftest py-ert-py-execute-file-ipython-dedicated-functionp-test ()
  (should (functionp 'py-execute-file-ipython-dedicated)))

(ert-deftest py-ert-py-execute-file-ipython-dedicated-switch-functionp-test ()
  (should (functionp 'py-execute-file-ipython-dedicated-switch)))

(ert-deftest py-ert-py-execute-file-python3-functionp-test ()
  (should (functionp 'py-execute-file-python3)))

(ert-deftest py-ert-py-execute-file-python3-switch-functionp-test ()
  (should (functionp 'py-execute-file-python3-switch)))

(ert-deftest py-ert-py-execute-file-python3-no-switch-functionp-test ()
  (should (functionp 'py-execute-file-python3-no-switch)))

(ert-deftest py-ert-py-execute-file-python3-dedicated-functionp-test ()
  (should (functionp 'py-execute-file-python3-dedicated)))

(ert-deftest py-ert-py-execute-file-python3-dedicated-switch-functionp-test ()
  (should (functionp 'py-execute-file-python3-dedicated-switch)))

(ert-deftest py-ert-py-execute-file-python2-functionp-test ()
  (should (functionp 'py-execute-file-python2)))

(ert-deftest py-ert-py-execute-file-python2-switch-functionp-test ()
  (should (functionp 'py-execute-file-python2-switch)))

(ert-deftest py-ert-py-execute-file-python2-no-switch-functionp-test ()
  (should (functionp 'py-execute-file-python2-no-switch)))

(ert-deftest py-ert-py-execute-file-python2-dedicated-functionp-test ()
  (should (functionp 'py-execute-file-python2-dedicated)))

(ert-deftest py-ert-py-execute-file-python2-dedicated-switch-functionp-test ()
  (should (functionp 'py-execute-file-python2-dedicated-switch)))

(ert-deftest py-ert-py-execute-file-jython-functionp-test ()
  (should (functionp 'py-execute-file-jython)))

(ert-deftest py-ert-py-execute-file-jython-switch-functionp-test ()
  (should (functionp 'py-execute-file-jython-switch)))

(ert-deftest py-ert-py-execute-file-jython-no-switch-functionp-test ()
  (should (functionp 'py-execute-file-jython-no-switch)))

(ert-deftest py-ert-py-execute-file-jython-dedicated-functionp-test ()
  (should (functionp 'py-execute-file-jython-dedicated)))

(ert-deftest py-ert-py-execute-file-jython-dedicated-switch-functionp-test ()
  (should (functionp 'py-execute-file-jython-dedicated-switch)))


(ert-deftest py-ert-py-mark-paragraph-functionp-test ()
  (should (functionp 'py-mark-paragraph)))

(ert-deftest py-ert-py-mark-block-functionp-test ()
  (should (functionp 'py-mark-block)))

(ert-deftest py-ert-py-mark-minor-block-functionp-test ()
  (should (functionp 'py-mark-minor-block)))

(ert-deftest py-ert-py-mark-clause-functionp-test ()
  (should (functionp 'py-mark-clause)))

(ert-deftest py-ert-py-mark-block-or-clause-functionp-test ()
  (should (functionp 'py-mark-block-or-clause)))

(ert-deftest py-ert-py-mark-def-functionp-test ()
  (should (functionp 'py-mark-def)))

(ert-deftest py-ert-py-mark-class-functionp-test ()
  (should (functionp 'py-mark-class)))

(ert-deftest py-ert-py-mark-def-or-class-functionp-test ()
  (should (functionp 'py-mark-def-or-class)))

(ert-deftest py-ert-py-mark-line-functionp-test ()
  (should (functionp 'py-mark-line)))

(ert-deftest py-ert-py-mark-statement-functionp-test ()
  (should (functionp 'py-mark-statement)))

(ert-deftest py-ert-py-mark-comment-functionp-test ()
  (should (functionp 'py-mark-comment)))

(ert-deftest py-ert-py-mark-top-level-functionp-test ()
  (should (functionp 'py-mark-top-level)))

(ert-deftest py-ert-py-mark-partial-expression-functionp-test ()
  (should (functionp 'py-mark-partial-expression)))

(ert-deftest py-ert-py-mark-expression-functionp-test ()
  (should (functionp 'py-mark-expression)))

(ert-deftest py-ert-py-mark-paragraph-bol-functionp-test ()
  (should (functionp 'py-mark-paragraph-bol)))

(ert-deftest py-ert-py-mark-block-bol-functionp-test ()
  (should (functionp 'py-mark-block-bol)))

(ert-deftest py-ert-py-mark-minor-block-bol-functionp-test ()
  (should (functionp 'py-mark-minor-block-bol)))

(ert-deftest py-ert-py-mark-clause-bol-functionp-test ()
  (should (functionp 'py-mark-clause-bol)))

(ert-deftest py-ert-py-mark-block-or-clause-bol-functionp-test ()
  (should (functionp 'py-mark-block-or-clause-bol)))

(ert-deftest py-ert-py-mark-def-bol-functionp-test ()
  (should (functionp 'py-mark-def-bol)))

(ert-deftest py-ert-py-mark-class-bol-functionp-test ()
  (should (functionp 'py-mark-class-bol)))

(ert-deftest py-ert-py-mark-def-or-class-bol-functionp-test ()
  (should (functionp 'py-mark-def-or-class-bol)))

(ert-deftest py-ert-py-mark-line-bol-functionp-test ()
  (should (functionp 'py-mark-line-bol)))

(ert-deftest py-ert-py-mark-statement-bol-functionp-test ()
  (should (functionp 'py-mark-statement-bol)))

(ert-deftest py-ert-py-mark-comment-bol-functionp-test ()
  (should (functionp 'py-mark-comment-bol)))

(ert-deftest py-ert-py-mark-top-level-bol-functionp-test ()
  (should (functionp 'py-mark-top-level-bol)))

(ert-deftest py-ert-py-mark-partial-expression-bol-functionp-test ()
  (should (functionp 'py-mark-partial-expression-bol)))

(ert-deftest py-ert-py-mark-expression-bol-functionp-test ()
  (should (functionp 'py-mark-expression-bol)))

(ert-deftest py-ert-py--beginning-of-block-p-functionp-test ()
  (should (functionp 'py--beginning-of-block-p)))

(ert-deftest py-ert-py--beginning-of-clause-p-functionp-test ()
  (should (functionp 'py--beginning-of-clause-p)))

(ert-deftest py-ert-py--beginning-of-block-or-clause-p-functionp-test ()
  (should (functionp 'py--beginning-of-block-or-clause-p)))

(ert-deftest py-ert-py--beginning-of-def-p-functionp-test ()
  (should (functionp 'py--beginning-of-def-p)))

(ert-deftest py-ert-py--beginning-of-class-p-functionp-test ()
  (should (functionp 'py--beginning-of-class-p)))

(ert-deftest py-ert-py--beginning-of-def-or-class-p-functionp-test ()
  (should (functionp 'py--beginning-of-def-or-class-p)))

(ert-deftest py-ert-py--beginning-of-if-block-p-functionp-test ()
  (should (functionp 'py--beginning-of-if-block-p)))

(ert-deftest py-ert-py--beginning-of-try-block-p-functionp-test ()
  (should (functionp 'py--beginning-of-try-block-p)))

(ert-deftest py-ert-py--beginning-of-minor-block-p-functionp-test ()
  (should (functionp 'py--beginning-of-minor-block-p)))

(ert-deftest py-ert-py--beginning-of-for-block-p-functionp-test ()
  (should (functionp 'py--beginning-of-for-block-p)))

(ert-deftest py-ert-py--beginning-of-top-level-p-functionp-test ()
  (should (functionp 'py--beginning-of-top-level-p)))

(ert-deftest py-ert-py--beginning-of-statement-p-functionp-test ()
  (should (functionp 'py--beginning-of-statement-p)))

(ert-deftest py-ert-py--beginning-of-expression-p-functionp-test ()
  (should (functionp 'py--beginning-of-expression-p)))

(ert-deftest py-ert-py--beginning-of-partial-expression-p-functionp-test ()
  (should (functionp 'py--beginning-of-partial-expression-p)))

(ert-deftest py-ert-py--beginning-of-block-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-block-bol-p)))

(ert-deftest py-ert-py--beginning-of-clause-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-clause-bol-p)))

(ert-deftest py-ert-py--beginning-of-block-or-clause-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-block-or-clause-bol-p)))

(ert-deftest py-ert-py--beginning-of-def-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-def-bol-p)))

(ert-deftest py-ert-py--beginning-of-class-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-class-bol-p)))

(ert-deftest py-ert-py--beginning-of-def-or-class-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-def-or-class-bol-p)))

(ert-deftest py-ert-py--beginning-of-if-block-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-if-block-bol-p)))

(ert-deftest py-ert-py--beginning-of-try-block-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-try-block-bol-p)))

(ert-deftest py-ert-py--beginning-of-minor-block-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-minor-block-bol-p)))

(ert-deftest py-ert-py--beginning-of-for-block-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-for-block-bol-p)))

(ert-deftest py-ert-py--beginning-of-statement-bol-p-functionp-test ()
  (should (functionp 'py--beginning-of-statement-bol-p)))

(ert-deftest py-ert-py--shell-completion-get-completions-functionp-test ()
  (should (functionp 'py--shell-completion-get-completions)))

(ert-deftest py-ert-py--after-change-function-functionp-test ()
  (should (functionp 'py--after-change-function)))

(ert-deftest py-ert-py--try-completion-intern-functionp-test ()
  (should (functionp 'py--try-completion-intern)))

(ert-deftest py-ert-py--try-completion-functionp-test ()
  (should (functionp 'py--try-completion)))

(ert-deftest py-ert-py--shell--do-completion-at-point-functionp-test ()
  (should (functionp 'py--shell--do-completion-at-point)))

(ert-deftest py-ert-py--complete-base-functionp-test ()
  (should (functionp 'py--complete-base)))

(ert-deftest py-ert-py--complete-prepare-functionp-test ()
  (should (functionp 'py--complete-prepare)))

(ert-deftest py-ert-py-shell-complete-functionp-test ()
  (should (functionp 'py-shell-complete)))

(ert-deftest py-ert-py-indent-or-complete-functionp-test ()
  (should (functionp 'py-indent-or-complete)))

(ert-deftest py-ert-py-shift-left-functionp-test ()
  (should (functionp 'py-shift-left)))

(ert-deftest py-ert-py-shift-right-functionp-test ()
  (should (functionp 'py-shift-right)))

(ert-deftest py-ert-py--shift-intern-functionp-test ()
  (should (functionp 'py--shift-intern)))

(ert-deftest py-ert-py--shift-forms-base-functionp-test ()
  (should (functionp 'py--shift-forms-base)))

(ert-deftest py-ert-py-shift-paragraph-right-functionp-test ()
  (should (functionp 'py-shift-paragraph-right)))

(ert-deftest py-ert-py-shift-paragraph-left-functionp-test ()
  (should (functionp 'py-shift-paragraph-left)))

(ert-deftest py-ert-py-shift-block-right-functionp-test ()
  (should (functionp 'py-shift-block-right)))

(ert-deftest py-ert-py-shift-block-left-functionp-test ()
  (should (functionp 'py-shift-block-left)))

(ert-deftest py-ert-py-shift-minor-block-left-functionp-test ()
  (should (functionp 'py-shift-minor-block-left)))

(ert-deftest py-ert-py-shift-minor-block-right-functionp-test ()
  (should (functionp 'py-shift-minor-block-right)))

(ert-deftest py-ert-py-shift-clause-right-functionp-test ()
  (should (functionp 'py-shift-clause-right)))

(ert-deftest py-ert-py-shift-clause-left-functionp-test ()
  (should (functionp 'py-shift-clause-left)))

(ert-deftest py-ert-py-shift-block-or-clause-right-functionp-test ()
  (should (functionp 'py-shift-block-or-clause-right)))

(ert-deftest py-ert-py-shift-block-or-clause-left-functionp-test ()
  (should (functionp 'py-shift-block-or-clause-left)))

(ert-deftest py-ert-py-shift-def-right-functionp-test ()
  (should (functionp 'py-shift-def-right)))

(ert-deftest py-ert-py-shift-def-left-functionp-test ()
  (should (functionp 'py-shift-def-left)))

(ert-deftest py-ert-py-shift-class-right-functionp-test ()
  (should (functionp 'py-shift-class-right)))

(ert-deftest py-ert-py-shift-class-left-functionp-test ()
  (should (functionp 'py-shift-class-left)))

(ert-deftest py-ert-py-shift-def-or-class-right-functionp-test ()
  (should (functionp 'py-shift-def-or-class-right)))

(ert-deftest py-ert-py-shift-def-or-class-left-functionp-test ()
  (should (functionp 'py-shift-def-or-class-left)))

(ert-deftest py-ert-py-shift-line-right-functionp-test ()
  (should (functionp 'py-shift-line-right)))

(ert-deftest py-ert-py-shift-line-left-functionp-test ()
  (should (functionp 'py-shift-line-left)))

(ert-deftest py-ert-py-shift-statement-right-functionp-test ()
  (should (functionp 'py-shift-statement-right)))

(ert-deftest py-ert-py-shift-statement-left-functionp-test ()
  (should (functionp 'py-shift-statement-left)))

(ert-deftest py-ert-py--bounds-of-statement-functionp-test ()
  (should (functionp 'py--bounds-of-statement)))

(ert-deftest py-ert-py--bounds-of-block-functionp-test ()
  (should (functionp 'py--bounds-of-block)))

(ert-deftest py-ert-py--bounds-of-clause-functionp-test ()
  (should (functionp 'py--bounds-of-clause)))

(ert-deftest py-ert-py--bounds-of-block-or-clause-functionp-test ()
  (should (functionp 'py--bounds-of-block-or-clause)))

(ert-deftest py-ert-py--bounds-of-def-functionp-test ()
  (should (functionp 'py--bounds-of-def)))

(ert-deftest py-ert-py--bounds-of-class-functionp-test ()
  (should (functionp 'py--bounds-of-class)))

(ert-deftest py-ert-py--bounds-of-def-or-class-functionp-test ()
  (should (functionp 'py--bounds-of-def-or-class)))

(ert-deftest py-ert-py--bounds-of-buffer-functionp-test ()
  (should (functionp 'py--bounds-of-buffer)))

(ert-deftest py-ert-py--bounds-of-expression-functionp-test ()
  (should (functionp 'py--bounds-of-expression)))

(ert-deftest py-ert-py--bounds-of-partial-expression-functionp-test ()
  (should (functionp 'py--bounds-of-partial-expression)))

(ert-deftest py-ert-py--bounds-of-minor-block-functionp-test ()
  (should (functionp 'py--bounds-of-minor-block)))

(ert-deftest py-ert-py--bounds-of-if-block-functionp-test ()
  (should (functionp 'py--bounds-of-if-block)))

(ert-deftest py-ert-py--bounds-of-try-block-functionp-test ()
  (should (functionp 'py--bounds-of-try-block)))

(ert-deftest py-ert-py--bounds-of-except-block-functionp-test ()
  (should (functionp 'py--bounds-of-except-block)))

(ert-deftest py-ert-py--bounds-of-top-level-functionp-test ()
  (should (functionp 'py--bounds-of-top-level)))

(ert-deftest py-ert-py--bounds-of-statement-bol-functionp-test ()
  (should (functionp 'py--bounds-of-statement-bol)))

(ert-deftest py-ert-py--bounds-of-block-bol-functionp-test ()
  (should (functionp 'py--bounds-of-block-bol)))

(ert-deftest py-ert-py--bounds-of-clause-bol-functionp-test ()
  (should (functionp 'py--bounds-of-clause-bol)))

(ert-deftest py-ert-py--bounds-of-block-or-clause-bol-functionp-test ()
  (should (functionp 'py--bounds-of-block-or-clause-bol)))

(ert-deftest py-ert-py--bounds-of-def-bol-functionp-test ()
  (should (functionp 'py--bounds-of-def-bol)))

(ert-deftest py-ert-py--bounds-of-class-bol-functionp-test ()
  (should (functionp 'py--bounds-of-class-bol)))

(ert-deftest py-ert-py--bounds-of-minor-block-bol-functionp-test ()
  (should (functionp 'py--bounds-of-minor-block-bol)))

(ert-deftest py-ert-py--bounds-of-if-block-bol-functionp-test ()
  (should (functionp 'py--bounds-of-if-block-bol)))

(ert-deftest py-ert-py--bounds-of-try-block-bol-functionp-test ()
  (should (functionp 'py--bounds-of-try-block-bol)))

(ert-deftest py-ert-py-beginning-of-top-level-functionp-test ()
  (should (functionp 'py-beginning-of-top-level)))

(ert-deftest py-ert-py-end-of-top-level-functionp-test ()
  (should (functionp 'py-end-of-top-level)))

(ert-deftest py-ert-py-beginning-functionp-test ()
  (should (functionp 'py-beginning)))

(ert-deftest py-ert-py-end-functionp-test ()
  (should (functionp 'py-end)))

(ert-deftest py-ert-py-up-functionp-test ()
  (should (functionp 'py-up)))

(ert-deftest py-ert-py-down-functionp-test ()
  (should (functionp 'py-down)))

(ert-deftest py-ert-py-backward-same-level-functionp-test ()
  (should (functionp 'py-backward-same-level)))

(ert-deftest py-ert-py-end-of-block-functionp-test ()
  (should (functionp 'py-end-of-block)))

(ert-deftest py-ert-py-end-of-clause-functionp-test ()
  (should (functionp 'py-end-of-clause)))

(ert-deftest py-ert-py-end-of-block-or-clause-functionp-test ()
  (should (functionp 'py-end-of-block-or-clause)))

(ert-deftest py-ert-py-end-of-def-functionp-test ()
  (should (functionp 'py-end-of-def)))

(ert-deftest py-ert-py-end-of-class-functionp-test ()
  (should (functionp 'py-end-of-class)))

(ert-deftest py-ert-py-end-of-def-or-class-functionp-test ()
  (should (functionp 'py-end-of-def-or-class)))

(ert-deftest py-ert-py-end-of-if-block-functionp-test ()
  (should (functionp 'py-end-of-if-block)))

(ert-deftest py-ert-py-end-of-elif-block-functionp-test ()
  (should (functionp 'py-end-of-elif-block)))

(ert-deftest py-ert-py-end-of-try-block-functionp-test ()
  (should (functionp 'py-end-of-try-block)))

(ert-deftest py-ert-py-end-of-minor-block-functionp-test ()
  (should (functionp 'py-end-of-minor-block)))

(ert-deftest py-ert-py-beginning-of-buffer-functionp-test ()
  (should (functionp 'py-beginning-of-buffer)))

(ert-deftest py-ert-py-end-of-buffer-functionp-test ()
  (should (functionp 'py-end-of-buffer)))

(provide 'py-ert-function-tests)
;;; py-ert-function-tests.el ends here
