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

(ert-deftest py-ert-virtualenv-filter-test ()
  (should (functionp 'virtualenv-filter)))

(ert-deftest py-ert-virtualenv-append-path-test ()
  (should (functionp 'virtualenv-append-path)))

(ert-deftest py-ert-virtualenv-add-to-path-test ()
  (should (functionp 'virtualenv-add-to-path)))

(ert-deftest py-ert-virtualenv-current-test ()
  (should (functionp 'virtualenv-current)))

(ert-deftest py-ert-virtualenv-deactivate-test ()
  (should (functionp 'virtualenv-deactivate)))

(ert-deftest py-ert-virtualenv-activate-test ()
  (should (functionp 'virtualenv-activate)))

(ert-deftest py-ert-virtualenv-deactivate-test ()
  (should (functionp 'virtualenv-deactivate)))

(ert-deftest py-ert-virtualenv-p-test ()
  (should (functionp 'virtualenv-p)))

(ert-deftest py-ert-virtualenv-workon-complete-test ()
  (should (functionp 'virtualenv-workon-complete)))

(ert-deftest py-ert-virtualenv-workon-test ()
  (should (functionp 'virtualenv-workon)))

(ert-deftest py-ert-py--beginning-of-block-p-test ()
  (should (functionp 'py--beginning-of-block-p)))

(ert-deftest py-ert-py--beginning-of-clause-p-test ()
  (should (functionp 'py--beginning-of-clause-p)))

(ert-deftest py-ert-py--beginning-of-block-or-clause-p-test ()
  (should (functionp 'py--beginning-of-block-or-clause-p)))

(ert-deftest py-ert-py--beginning-of-def-p-test ()
  (should (functionp 'py--beginning-of-def-p)))

(ert-deftest py-ert-py--beginning-of-class-p-test ()
  (should (functionp 'py--beginning-of-class-p)))

(ert-deftest py-ert-py--beginning-of-def-or-class-p-test ()
  (should (functionp 'py--beginning-of-def-or-class-p)))

(ert-deftest py-ert-py--beginning-of-if-block-p-test ()
  (should (functionp 'py--beginning-of-if-block-p)))

(ert-deftest py-ert-py--beginning-of-try-block-p-test ()
  (should (functionp 'py--beginning-of-try-block-p)))

(ert-deftest py-ert-py--beginning-of-minor-block-p-test ()
  (should (functionp 'py--beginning-of-minor-block-p)))

(ert-deftest py-ert-py--beginning-of-for-block-p-test ()
  (should (functionp 'py--beginning-of-for-block-p)))

(ert-deftest py-ert-py--beginning-of-top-level-p-test ()
  (should (functionp 'py--beginning-of-top-level-p)))

(ert-deftest py-ert-py--beginning-of-statement-p-test ()
  (should (functionp 'py--beginning-of-statement-p)))

(ert-deftest py-ert-py--beginning-of-expression-p-test ()
  (should (functionp 'py--beginning-of-expression-p)))

(ert-deftest py-ert-py--beginning-of-partial-expression-p-test ()
  (should (functionp 'py--beginning-of-partial-expression-p)))

(ert-deftest py-ert-py--beginning-of-block-bol-p-test ()
  (should (functionp 'py--beginning-of-block-bol-p)))

(ert-deftest py-ert-py--beginning-of-clause-bol-p-test ()
  (should (functionp 'py--beginning-of-clause-bol-p)))

(ert-deftest py-ert-py--beginning-of-block-or-clause-bol-p-test ()
  (should (functionp 'py--beginning-of-block-or-clause-bol-p)))

(ert-deftest py-ert-py--beginning-of-def-bol-p-test ()
  (should (functionp 'py--beginning-of-def-bol-p)))

(ert-deftest py-ert-py--beginning-of-class-bol-p-test ()
  (should (functionp 'py--beginning-of-class-bol-p)))

(ert-deftest py-ert-py--beginning-of-def-or-class-bol-p-test ()
  (should (functionp 'py--beginning-of-def-or-class-bol-p)))

(ert-deftest py-ert-py--beginning-of-if-block-bol-p-test ()
  (should (functionp 'py--beginning-of-if-block-bol-p)))

(ert-deftest py-ert-py--beginning-of-try-block-bol-p-test ()
  (should (functionp 'py--beginning-of-try-block-bol-p)))

(ert-deftest py-ert-py--beginning-of-minor-block-bol-p-test ()
  (should (functionp 'py--beginning-of-minor-block-bol-p)))

(ert-deftest py-ert-py--beginning-of-for-block-bol-p-test ()
  (should (functionp 'py--beginning-of-for-block-bol-p)))

(ert-deftest py-ert-py--beginning-of-statement-bol-p-test ()
  (should (functionp 'py--beginning-of-statement-bol-p)))

(ert-deftest py-ert-py-up-statement-test ()
  (should (functionp 'py-up-statement)))

(ert-deftest py-ert-py-down-statement-test ()
  (should (functionp 'py-down-statement)))

(ert-deftest py-ert-py-up-base-test ()
  (should (functionp 'py-up-base)))

(ert-deftest py-ert-py-down-base-test ()
  (should (functionp 'py-down-base)))

(ert-deftest py-ert-py-up-base-bol-test ()
  (should (functionp 'py-up-base-bol)))

(ert-deftest py-ert-py-down-base-bol-test ()
  (should (functionp 'py-down-base-bol)))

(ert-deftest py-ert-py-up-block-test ()
  (should (functionp 'py-up-block)))

(ert-deftest py-ert-py-up-minor-block-test ()
  (should (functionp 'py-up-minor-block)))

(ert-deftest py-ert-py-up-clause-test ()
  (should (functionp 'py-up-clause)))

(ert-deftest py-ert-py-up-block-or-clause-test ()
  (should (functionp 'py-up-block-or-clause)))

(ert-deftest py-ert-py-up-def-test ()
  (should (functionp 'py-up-def)))

(ert-deftest py-ert-py-up-class-test ()
  (should (functionp 'py-up-class)))

(ert-deftest py-ert-py-up-def-or-class-test ()
  (should (functionp 'py-up-def-or-class)))

(ert-deftest py-ert-py-down-block-test ()
  (should (functionp 'py-down-block)))

(ert-deftest py-ert-py-down-minor-block-test ()
  (should (functionp 'py-down-minor-block)))

(ert-deftest py-ert-py-down-clause-test ()
  (should (functionp 'py-down-clause)))

(ert-deftest py-ert-py-down-block-or-clause-test ()
  (should (functionp 'py-down-block-or-clause)))

(ert-deftest py-ert-py-down-def-test ()
  (should (functionp 'py-down-def)))

(ert-deftest py-ert-py-down-class-test ()
  (should (functionp 'py-down-class)))

(ert-deftest py-ert-py-down-def-or-class-test ()
  (should (functionp 'py-down-def-or-class)))

(ert-deftest py-ert-py-up-block-bol-test ()
  (should (functionp 'py-up-block-bol)))

(ert-deftest py-ert-py-up-minor-block-bol-test ()
  (should (functionp 'py-up-minor-block-bol)))

(ert-deftest py-ert-py-up-clause-bol-test ()
  (should (functionp 'py-up-clause-bol)))

(ert-deftest py-ert-py-up-block-or-clause-bol-test ()
  (should (functionp 'py-up-block-or-clause-bol)))

(ert-deftest py-ert-py-up-def-bol-test ()
  (should (functionp 'py-up-def-bol)))

(ert-deftest py-ert-py-up-class-bol-test ()
  (should (functionp 'py-up-class-bol)))

(ert-deftest py-ert-py-up-def-or-class-bol-test ()
  (should (functionp 'py-up-def-or-class-bol)))

(ert-deftest py-ert-py-down-block-bol-test ()
  (should (functionp 'py-down-block-bol)))

(ert-deftest py-ert-py-down-minor-block-bol-test ()
  (should (functionp 'py-down-minor-block-bol)))

(ert-deftest py-ert-py-down-clause-bol-test ()
  (should (functionp 'py-down-clause-bol)))

(ert-deftest py-ert-py-down-block-or-clause-bol-test ()
  (should (functionp 'py-down-block-or-clause-bol)))

(ert-deftest py-ert-py-down-def-bol-test ()
  (should (functionp 'py-down-def-bol)))

(ert-deftest py-ert-py-down-class-bol-test ()
  (should (functionp 'py-down-class-bol)))

(ert-deftest py-ert-py-down-def-or-class-bol-test ()
  (should (functionp 'py-down-def-or-class-bol)))

(ert-deftest py-ert-py--end-of-statement-position-test ()
  (should (functionp 'py--end-of-statement-position)))

(ert-deftest py-ert-py--end-of-block-position-test ()
  (should (functionp 'py--end-of-block-position)))

(ert-deftest py-ert-py--end-of-clause-position-test ()
  (should (functionp 'py--end-of-clause-position)))

(ert-deftest py-ert-py--end-of-block-or-clause-position-test ()
  (should (functionp 'py--end-of-block-or-clause-position)))

(ert-deftest py-ert-py--end-of-def-position-test ()
  (should (functionp 'py--end-of-def-position)))

(ert-deftest py-ert-py--end-of-class-position-test ()
  (should (functionp 'py--end-of-class-position)))

(ert-deftest py-ert-py--end-of-def-or-class-position-test ()
  (should (functionp 'py--end-of-def-or-class-position)))

(ert-deftest py-ert-py--end-of-buffer-position-test ()
  (should (functionp 'py--end-of-buffer-position)))

(ert-deftest py-ert-py--end-of-expression-position-test ()
  (should (functionp 'py--end-of-expression-position)))

(ert-deftest py-ert-py--end-of-partial-expression-position-test ()
  (should (functionp 'py--end-of-partial-expression-position)))

(ert-deftest py-ert-py--end-of-minor-block-position-test ()
  (should (functionp 'py--end-of-minor-block-position)))

(ert-deftest py-ert-py--end-of-if-block-position-test ()
  (should (functionp 'py--end-of-if-block-position)))

(ert-deftest py-ert-py--end-of-try-block-position-test ()
  (should (functionp 'py--end-of-try-block-position)))

(ert-deftest py-ert-py--end-of-except-block-position-test ()
  (should (functionp 'py--end-of-except-block-position)))

(ert-deftest py-ert-py--end-of-top-level-position-test ()
  (should (functionp 'py--end-of-top-level-position)))

(ert-deftest py-ert-py--end-of-statement-position-bol-test ()
  (should (functionp 'py--end-of-statement-position-bol)))

(ert-deftest py-ert-py--end-of-block-position-bol-test ()
  (should (functionp 'py--end-of-block-position-bol)))

(ert-deftest py-ert-py--end-of-clause-position-bol-test ()
  (should (functionp 'py--end-of-clause-position-bol)))

(ert-deftest py-ert-py--end-of-block-or-clause-position-bol-test ()
  (should (functionp 'py--end-of-block-or-clause-position-bol)))

(ert-deftest py-ert-py--end-of-def-position-bol-test ()
  (should (functionp 'py--end-of-def-position-bol)))

(ert-deftest py-ert-py--end-of-class-position-bol-test ()
  (should (functionp 'py--end-of-class-position-bol)))

(ert-deftest py-ert-py--end-of-minor-block-position-bol-test ()
  (should (functionp 'py--end-of-minor-block-position-bol)))

(ert-deftest py-ert-py--end-of-if-block-position-bol-test ()
  (should (functionp 'py--end-of-if-block-position-bol)))

(ert-deftest py-ert-py--end-of-try-block-position-bol-test ()
  (should (functionp 'py--end-of-try-block-position-bol)))

(ert-deftest py-ert-py-kill-block-test ()
  (should (functionp 'py-kill-block)))

(ert-deftest py-ert-py-kill-clause-test ()
  (should (functionp 'py-kill-clause)))

(ert-deftest py-ert-py-kill-block-or-clause-test ()
  (should (functionp 'py-kill-block-or-clause)))

(ert-deftest py-ert-py-kill-def-test ()
  (should (functionp 'py-kill-def)))

(ert-deftest py-ert-py-kill-class-test ()
  (should (functionp 'py-kill-class)))

(ert-deftest py-ert-py-kill-def-or-class-test ()
  (should (functionp 'py-kill-def-or-class)))

(ert-deftest py-ert-py-kill-if-block-test ()
  (should (functionp 'py-kill-if-block)))

(ert-deftest py-ert-py-kill-try-block-test ()
  (should (functionp 'py-kill-try-block)))

(ert-deftest py-ert-py-kill-minor-block-test ()
  (should (functionp 'py-kill-minor-block)))

(ert-deftest py-ert-py-kill-for-block-test ()
  (should (functionp 'py-kill-for-block)))

(ert-deftest py-ert-py-kill-top-level-test ()
  (should (functionp 'py-kill-top-level)))

(ert-deftest py-ert-py-kill-statement-test ()
  (should (functionp 'py-kill-statement)))

(ert-deftest py-ert-py-kill-expression-test ()
  (should (functionp 'py-kill-expression)))

(ert-deftest py-ert-py-kill-partial-expression-test ()
  (should (functionp 'py-kill-partial-expression)))

(ert-deftest py-ert-py-kill-block-bol-test ()
  (should (functionp 'py-kill-block-bol)))

(ert-deftest py-ert-py-kill-clause-bol-test ()
  (should (functionp 'py-kill-clause-bol)))

(ert-deftest py-ert-py-kill-block-or-clause-bol-test ()
  (should (functionp 'py-kill-block-or-clause-bol)))

(ert-deftest py-ert-py-kill-def-bol-test ()
  (should (functionp 'py-kill-def-bol)))

(ert-deftest py-ert-py-kill-class-bol-test ()
  (should (functionp 'py-kill-class-bol)))

(ert-deftest py-ert-py-kill-def-or-class-bol-test ()
  (should (functionp 'py-kill-def-or-class-bol)))

(ert-deftest py-ert-py-kill-if-block-bol-test ()
  (should (functionp 'py-kill-if-block-bol)))

(ert-deftest py-ert-py-kill-try-block-bol-test ()
  (should (functionp 'py-kill-try-block-bol)))

(ert-deftest py-ert-py-kill-minor-block-bol-test ()
  (should (functionp 'py-kill-minor-block-bol)))

(ert-deftest py-ert-py-kill-for-block-bol-test ()
  (should (functionp 'py-kill-for-block-bol)))

(ert-deftest py-ert-py-kill-top-level-bol-test ()
  (should (functionp 'py-kill-top-level-bol)))

(ert-deftest py-ert-py-kill-statement-bol-test ()
  (should (functionp 'py-kill-statement-bol)))

(ert-deftest py-ert-py-beginning-of-expression-test ()
  (should (functionp 'py-beginning-of-expression)))

(ert-deftest py-ert-py--beginning-of-expression-intern-test ()
  (should (functionp 'py--beginning-of-expression-intern)))

(ert-deftest py-ert-py-end-of-expression-test ()
  (should (functionp 'py-end-of-expression)))

(ert-deftest py-ert-py--end-of-expression-intern-test ()
  (should (functionp 'py--end-of-expression-intern)))

(ert-deftest py-ert-py-beginning-of-partial-expression-test ()
  (should (functionp 'py-beginning-of-partial-expression)))

(ert-deftest py-ert-py-end-of-partial-expression-test ()
  (should (functionp 'py-end-of-partial-expression)))

(ert-deftest py-ert-py-beginning-of-line-test ()
  (should (functionp 'py-beginning-of-line)))

(ert-deftest py-ert-py-end-of-line-test ()
  (should (functionp 'py-end-of-line)))

(ert-deftest py-ert-py-beginning-of-statement-test ()
  (should (functionp 'py-beginning-of-statement)))

(ert-deftest py-ert-py-beginning-of-statement-bol-test ()
  (should (functionp 'py-beginning-of-statement-bol)))

(ert-deftest py-ert-py-end-of-statement-test ()
  (should (functionp 'py-end-of-statement)))

(ert-deftest py-ert-py-end-of-statement-bol-test ()
  (should (functionp 'py-end-of-statement-bol)))

(ert-deftest py-ert-py-goto-statement-below-test ()
  (should (functionp 'py-goto-statement-below)))

(ert-deftest py-ert-py-beginning-of-decorator-test ()
  (should (functionp 'py-beginning-of-decorator)))

(ert-deftest py-ert-py-end-of-decorator-test ()
  (should (functionp 'py-end-of-decorator)))

(ert-deftest py-ert-py-forward-line-test ()
  (should (functionp 'py-forward-line)))

(ert-deftest py-ert-py-go-to-beginning-of-comment-test ()
  (should (functionp 'py-go-to-beginning-of-comment)))

(ert-deftest py-ert-py--go-to-keyword-test ()
  (should (functionp 'py--go-to-keyword)))

(ert-deftest py-ert-py--clause-lookup-keyword-test ()
  (should (functionp 'py--clause-lookup-keyword)))

(ert-deftest py-ert-py-leave-comment-or-string-backward-test ()
  (should (functionp 'py-leave-comment-or-string-backward)))

(ert-deftest py-ert-py-beginning-of-list-pps-test ()
  (should (functionp 'py-beginning-of-list-pps)))

(ert-deftest py-ert-py-forward-into-nomenclature-test ()
  (should (functionp 'py-forward-into-nomenclature)))

(ert-deftest py-ert-py-backward-into-nomenclature-test ()
  (should (functionp 'py-backward-into-nomenclature)))

(ert-deftest py-ert-match-paren-test ()
  (should (functionp 'match-paren)))

(ert-deftest py-ert-py--travel-current-indent-test ()
  (should (functionp 'py--travel-current-indent)))

(ert-deftest py-ert-py-beginning-of-block-current-column-test ()
  (should (functionp 'py-beginning-of-block-current-column)))

(ert-deftest py-ert-py--end-of-block-p-test ()
  (should (functionp 'py--end-of-block-p)))

(ert-deftest py-ert-py--end-of-clause-p-test ()
  (should (functionp 'py--end-of-clause-p)))

(ert-deftest py-ert-py--end-of-block-or-clause-p-test ()
  (should (functionp 'py--end-of-block-or-clause-p)))

(ert-deftest py-ert-py--end-of-def-p-test ()
  (should (functionp 'py--end-of-def-p)))

(ert-deftest py-ert-py--end-of-class-p-test ()
  (should (functionp 'py--end-of-class-p)))

(ert-deftest py-ert-py--end-of-def-or-class-p-test ()
  (should (functionp 'py--end-of-def-or-class-p)))

(ert-deftest py-ert-py--end-of-if-block-p-test ()
  (should (functionp 'py--end-of-if-block-p)))

(ert-deftest py-ert-py--end-of-try-block-p-test ()
  (should (functionp 'py--end-of-try-block-p)))

(ert-deftest py-ert-py--end-of-minor-block-p-test ()
  (should (functionp 'py--end-of-minor-block-p)))

(ert-deftest py-ert-py--end-of-for-block-p-test ()
  (should (functionp 'py--end-of-for-block-p)))

(ert-deftest py-ert-py--end-of-top-level-p-test ()
  (should (functionp 'py--end-of-top-level-p)))

(ert-deftest py-ert-py--end-of-statement-p-test ()
  (should (functionp 'py--end-of-statement-p)))

(ert-deftest py-ert-py--end-of-expression-p-test ()
  (should (functionp 'py--end-of-expression-p)))

(ert-deftest py-ert-py--end-of-partial-expression-p-test ()
  (should (functionp 'py--end-of-partial-expression-p)))

(ert-deftest py-ert-py--end-of-block-bol-p-test ()
  (should (functionp 'py--end-of-block-bol-p)))

(ert-deftest py-ert-py--end-of-clause-bol-p-test ()
  (should (functionp 'py--end-of-clause-bol-p)))

(ert-deftest py-ert-py--end-of-block-or-clause-bol-p-test ()
  (should (functionp 'py--end-of-block-or-clause-bol-p)))

(ert-deftest py-ert-py--end-of-def-bol-p-test ()
  (should (functionp 'py--end-of-def-bol-p)))

(ert-deftest py-ert-py--end-of-class-bol-p-test ()
  (should (functionp 'py--end-of-class-bol-p)))

(ert-deftest py-ert-py--end-of-def-or-class-bol-p-test ()
  (should (functionp 'py--end-of-def-or-class-bol-p)))

(ert-deftest py-ert-py--end-of-if-block-bol-p-test ()
  (should (functionp 'py--end-of-if-block-bol-p)))

(ert-deftest py-ert-py--end-of-try-block-bol-p-test ()
  (should (functionp 'py--end-of-try-block-bol-p)))

(ert-deftest py-ert-py--end-of-minor-block-bol-p-test ()
  (should (functionp 'py--end-of-minor-block-bol-p)))

(ert-deftest py-ert-py--end-of-for-block-bol-p-test ()
  (should (functionp 'py--end-of-for-block-bol-p)))

(ert-deftest py-ert-py--end-of-top-level-bol-p-test ()
  (should (functionp 'py--end-of-top-level-bol-p)))

(ert-deftest py-ert-py--end-of-statement-bol-p-test ()
  (should (functionp 'py--end-of-statement-bol-p)))

(ert-deftest py-ert-py--fast-completion-get-completions-test ()
  (should (functionp 'py--fast-completion-get-completions)))

(ert-deftest py-ert-py--fast--do-completion-at-point-test ()
  (should (functionp 'py--fast--do-completion-at-point)))

(ert-deftest py-ert-py--fast-complete-base-test ()
  (should (functionp 'py--fast-complete-base)))

(ert-deftest py-ert-py-fast-complete-test ()
  (should (functionp 'py-fast-complete)))

(ert-deftest py-ert-py--end-of-block-p-test ()
  (should (functionp 'py--end-of-block-p)))

(ert-deftest py-ert-py--end-of-clause-p-test ()
  (should (functionp 'py--end-of-clause-p)))

(ert-deftest py-ert-py--end-of-block-or-clause-p-test ()
  (should (functionp 'py--end-of-block-or-clause-p)))

(ert-deftest py-ert-py--end-of-def-p-test ()
  (should (functionp 'py--end-of-def-p)))

(ert-deftest py-ert-py--end-of-class-p-test ()
  (should (functionp 'py--end-of-class-p)))

(ert-deftest py-ert-py--end-of-def-or-class-p-test ()
  (should (functionp 'py--end-of-def-or-class-p)))

(ert-deftest py-ert-py--end-of-if-block-p-test ()
  (should (functionp 'py--end-of-if-block-p)))

(ert-deftest py-ert-py--end-of-try-block-p-test ()
  (should (functionp 'py--end-of-try-block-p)))

(ert-deftest py-ert-py--end-of-minor-block-p-test ()
  (should (functionp 'py--end-of-minor-block-p)))

(ert-deftest py-ert-py--end-of-for-block-p-test ()
  (should (functionp 'py--end-of-for-block-p)))

(ert-deftest py-ert-py--end-of-top-level-p-test ()
  (should (functionp 'py--end-of-top-level-p)))

(ert-deftest py-ert-py--end-of-statement-p-test ()
  (should (functionp 'py--end-of-statement-p)))

(ert-deftest py-ert-py--end-of-expression-p-test ()
  (should (functionp 'py--end-of-expression-p)))

(ert-deftest py-ert-py--end-of-partial-expression-p-test ()
  (should (functionp 'py--end-of-partial-expression-p)))

(ert-deftest py-ert-py--end-of-block-bol-p-test ()
  (should (functionp 'py--end-of-block-bol-p)))

(ert-deftest py-ert-py--end-of-clause-bol-p-test ()
  (should (functionp 'py--end-of-clause-bol-p)))

(ert-deftest py-ert-py--end-of-block-or-clause-bol-p-test ()
  (should (functionp 'py--end-of-block-or-clause-bol-p)))

(ert-deftest py-ert-py--end-of-def-bol-p-test ()
  (should (functionp 'py--end-of-def-bol-p)))

(ert-deftest py-ert-py--end-of-class-bol-p-test ()
  (should (functionp 'py--end-of-class-bol-p)))

(ert-deftest py-ert-py--end-of-def-or-class-bol-p-test ()
  (should (functionp 'py--end-of-def-or-class-bol-p)))

(ert-deftest py-ert-py--end-of-if-block-bol-p-test ()
  (should (functionp 'py--end-of-if-block-bol-p)))

(ert-deftest py-ert-py--end-of-try-block-bol-p-test ()
  (should (functionp 'py--end-of-try-block-bol-p)))

(ert-deftest py-ert-py--end-of-minor-block-bol-p-test ()
  (should (functionp 'py--end-of-minor-block-bol-p)))

(ert-deftest py-ert-py--end-of-for-block-bol-p-test ()
  (should (functionp 'py--end-of-for-block-bol-p)))

(ert-deftest py-ert-py--end-of-top-level-bol-p-test ()
  (should (functionp 'py--end-of-top-level-bol-p)))

(ert-deftest py-ert-py--end-of-statement-bol-p-test ()
  (should (functionp 'py--end-of-statement-bol-p)))

(ert-deftest py-ert-py--all-shell-mode-setting-test ()
  (should (functionp 'py--all-shell-mode-setting)))

(ert-deftest py-ert-py-fast-process-test ()
  (should (functionp 'py-fast-process)))

(ert-deftest py-ert-py--filter-result-test ()
  (should (functionp 'py--filter-result)))

(ert-deftest py-ert-py--fast-send-string-no-output-test ()
  (should (functionp 'py--fast-send-string-no-output)))

(ert-deftest py-ert-py--fast-send-string-intern-test ()
  (should (functionp 'py--fast-send-string-intern)))

(ert-deftest py-ert-py--fast-send-string-test ()
  (should (functionp 'py--fast-send-string)))

(ert-deftest py-ert-py-fast-send-string-test ()
  (should (functionp 'py-fast-send-string)))

(ert-deftest py-ert-py-process-region-fast-test ()
  (should (functionp 'py-process-region-fast)))

(ert-deftest py-ert-py-execute-statement-fast-test ()
  (should (functionp 'py-execute-statement-fast)))

(ert-deftest py-ert-py-execute-block-fast-test ()
  (should (functionp 'py-execute-block-fast)))

(ert-deftest py-ert-py-execute-block-or-clause-fast-test ()
  (should (functionp 'py-execute-block-or-clause-fast)))

(ert-deftest py-ert-py-execute-def-fast-test ()
  (should (functionp 'py-execute-def-fast)))

(ert-deftest py-ert-py-execute-class-fast-test ()
  (should (functionp 'py-execute-class-fast)))

(ert-deftest py-ert-py-execute-def-or-class-fast-test ()
  (should (functionp 'py-execute-def-or-class-fast)))

(ert-deftest py-ert-py-execute-expression-fast-test ()
  (should (functionp 'py-execute-expression-fast)))

(ert-deftest py-ert-py-execute-partial-expression-fast-test ()
  (should (functionp 'py-execute-partial-expression-fast)))

(ert-deftest py-ert-py-execute-top-level-fast-test ()
  (should (functionp 'py-execute-top-level-fast)))

(ert-deftest py-ert-py-execute-clause-fast-test ()
  (should (functionp 'py-execute-clause-fast)))

(ert-deftest py-ert-py-restore-window-configuration-test ()
  (should (functionp 'py-restore-window-configuration)))

(ert-deftest py-ert-py-shell-execute-string-now-test ()
  (should (functionp 'py-shell-execute-string-now)))

(ert-deftest py-ert-py-switch-to-python-test ()
  (should (functionp 'py-switch-to-python)))

(ert-deftest py-ert-py-send-file-test ()
  (should (functionp 'py-send-file)))

(ert-deftest py-ert-toggle-force-local-shell-test ()
  (should (functionp 'toggle-force-local-shell)))

(ert-deftest py-ert-py-force-local-shell-on-test ()
  (should (functionp 'py-force-local-shell-on)))

(ert-deftest py-ert-py-force-local-shell-off-test ()
  (should (functionp 'py-force-local-shell-off)))

(ert-deftest py-ert-toggle-force-py-shell-name-p-test ()
  (should (functionp 'toggle-force-py-shell-name-p)))

(ert-deftest py-ert-force-py-shell-name-p-on-test ()
  (should (functionp 'force-py-shell-name-p-on)))

(ert-deftest py-ert-force-py-shell-name-p-off-test ()
  (should (functionp 'force-py-shell-name-p-off)))

(ert-deftest py-ert-py-toggle-split-windows-on-execute-test ()
  (should (functionp 'py-toggle-split-windows-on-execute)))

(ert-deftest py-ert-py-split-windows-on-execute-on-test ()
  (should (functionp 'py-split-windows-on-execute-on)))

(ert-deftest py-ert-py-split-windows-on-execute-off-test ()
  (should (functionp 'py-split-windows-on-execute-off)))

(ert-deftest py-ert-py-toggle-shell-switch-buffers-on-execute-test ()
  (should (functionp 'py-toggle-shell-switch-buffers-on-execute)))

(ert-deftest py-ert-py-shell-switch-buffers-on-execute-on-test ()
  (should (functionp 'py-shell-switch-buffers-on-execute-on)))

(ert-deftest py-ert-py-shell-switch-buffers-on-execute-off-test ()
  (should (functionp 'py-shell-switch-buffers-on-execute-off)))

(ert-deftest py-ert-py-guess-default-python-test ()
  (should (functionp 'py-guess-default-python)))

(ert-deftest py-ert-py-dirstack-hook-test ()
  (should (functionp 'py-dirstack-hook)))

(ert-deftest py-ert-py-shell-dedicated-test ()
  (should (functionp 'py-shell-dedicated)))

(ert-deftest py-ert-py-set-ipython-completion-command-string-test ()
  (should (functionp 'py-set-ipython-completion-command-string)))

(ert-deftest py-ert-py-ipython--module-completion-import-test ()
  (should (functionp 'py-ipython--module-completion-import)))

(ert-deftest py-ert-py--compose-buffer-name-initials-test ()
  (should (functionp 'py--compose-buffer-name-initials)))

(ert-deftest py-ert-py--remove-home-directory-from-list-test ()
  (should (functionp 'py--remove-home-directory-from-list)))

(ert-deftest py-ert-py--choose-buffer-name-test ()
  (should (functionp 'py--choose-buffer-name)))

(ert-deftest py-ert-py--jump-to-exception-intern-test ()
  (should (functionp 'py--jump-to-exception-intern)))

(ert-deftest py-ert-py--jump-to-exception-test ()
  (should (functionp 'py--jump-to-exception)))

(ert-deftest py-ert-py-toggle-split-window-function-test ()
  (should (functionp 'py-toggle-split-window-function)))

(ert-deftest py-ert-py--manage-windows-set-and-switch-test ()
  (should (functionp 'py--manage-windows-set-and-switch)))

(ert-deftest py-ert-py--alternative-split-windows-on-execute-function-test ()
  (should (functionp 'py--alternative-split-windows-on-execute-function)))

(ert-deftest py-ert-py--get-splittable-window-test ()
  (should (functionp 'py--get-splittable-window)))

(ert-deftest py-ert-py--manage-windows-split-test ()
  (should (functionp 'py--manage-windows-split)))

(ert-deftest py-ert-py--display-windows-test ()
  (should (functionp 'py--display-windows)))

(ert-deftest py-ert-py--shell-manage-windows-test ()
  (should (functionp 'py--shell-manage-windows)))

(ert-deftest py-ert-py-kill-shell-unconditional-test ()
  (should (functionp 'py-kill-shell-unconditional)))

(ert-deftest py-ert-py-kill-default-shell-unconditional-test ()
  (should (functionp 'py-kill-default-shell-unconditional)))

(ert-deftest py-ert-py--report-executable-test ()
  (should (functionp 'py--report-executable)))

(ert-deftest py-ert-py--shell-make-comint-test ()
  (should (functionp 'py--shell-make-comint)))

(ert-deftest py-ert-py--guess-buffer-name-test ()
  (should (functionp 'py--guess-buffer-name)))

(ert-deftest py-ert-py--configured-shell-test ()
  (should (functionp 'py--configured-shell)))

(ert-deftest py-ert-py--grab-prompt-ps1-test ()
  (should (functionp 'py--grab-prompt-ps1)))

(ert-deftest py-ert-py--start-fast-process-test ()
  (should (functionp 'py--start-fast-process)))

(ert-deftest py-ert-py-shell-test ()
  (should (functionp 'py-shell)))

(ert-deftest py-ert-py-shell-get-process-test ()
  (should (functionp 'py-shell-get-process)))

(ert-deftest py-ert-py-switch-to-shell-test ()
  (should (functionp 'py-switch-to-shell)))

(ert-deftest py-ert-py-which-execute-file-command-test ()
  (should (functionp 'py-which-execute-file-command)))

(ert-deftest py-ert-py--store-result-maybe-test ()
  (should (functionp 'py--store-result-maybe)))

(ert-deftest py-ert-py--close-execution-test ()
  (should (functionp 'py--close-execution)))

(ert-deftest py-ert-py--execute-base-test ()
  (should (functionp 'py--execute-base)))

(ert-deftest py-ert-py--send-to-fast-process-test ()
  (should (functionp 'py--send-to-fast-process)))

(ert-deftest py-ert-py--execute-base-intern-test ()
  (should (functionp 'py--execute-base-intern)))

(ert-deftest py-ert-py--execute-buffer-finally-test ()
  (should (functionp 'py--execute-buffer-finally)))

(ert-deftest py-ert-py--fetch-error-test ()
  (should (functionp 'py--fetch-error)))

(ert-deftest py-ert-py--fetch-result-test ()
  (should (functionp 'py--fetch-result)))

(ert-deftest py-ert-py--postprocess-comint-test ()
  (should (functionp 'py--postprocess-comint)))

(ert-deftest py-ert-py--execute-ge24.3-test ()
  (should (functionp 'py--execute-ge24.3)))

(ert-deftest py-ert-py-execute-region-test ()
  (should (functionp 'py-execute-region)))

(ert-deftest py-ert-py-execute-region-default-test ()
  (should (functionp 'py-execute-region-default)))

(ert-deftest py-ert-py-execute-region-no-switch-test ()
  (should (functionp 'py-execute-region-no-switch)))

(ert-deftest py-ert-py-execute-region-dedicated-test ()
  (should (functionp 'py-execute-region-dedicated)))

(ert-deftest py-ert-py-execute-region-switch-test ()
  (should (functionp 'py-execute-region-switch)))

(ert-deftest py-ert-py-execute-region-default-dedicated-test ()
  (should (functionp 'py-execute-region-default-dedicated)))

(ert-deftest py-ert-py-delete-temporary-test ()
  (should (functionp 'py-delete-temporary)))

(ert-deftest py-ert-py-execute-python-mode-v5-test ()
  (should (functionp 'py-execute-python-mode-v5)))

(ert-deftest py-ert-py--insert-offset-lines-test ()
  (should (functionp 'py--insert-offset-lines)))

(ert-deftest py-ert-py--execute-file-base-test ()
  (should (functionp 'py--execute-file-base)))

(ert-deftest py-ert-py-execute-file-test ()
  (should (functionp 'py-execute-file)))

(ert-deftest py-ert-py--current-working-directory-test ()
  (should (functionp 'py--current-working-directory)))

(ert-deftest py-ert-py--update-execute-directory-intern-test ()
  (should (functionp 'py--update-execute-directory-intern)))

(ert-deftest py-ert-py--update-execute-directory-test ()
  (should (functionp 'py--update-execute-directory)))

(ert-deftest py-ert-py-execute-string-test ()
  (should (functionp 'py-execute-string)))

(ert-deftest py-ert-py-execute-string-dedicated-test ()
  (should (functionp 'py-execute-string-dedicated)))

(ert-deftest py-ert-py--insert-execute-directory-test ()
  (should (functionp 'py--insert-execute-directory)))

(ert-deftest py-ert-py--fix-if-name-main-permission-test ()
  (should (functionp 'py--fix-if-name-main-permission)))

(ert-deftest py-ert-py--fix-start-intern-test ()
  (should (functionp 'py--fix-start-intern)))

(ert-deftest py-ert-py--fix-start-test ()
  (should (functionp 'py--fix-start)))

(ert-deftest py-ert-py-fetch-py-master-file-test ()
  (should (functionp 'py-fetch-py-master-file)))

(ert-deftest py-ert-py-execute-import-or-reload-test ()
  (should (functionp 'py-execute-import-or-reload)))

(ert-deftest py-ert-py--qualified-module-name-test ()
  (should (functionp 'py--qualified-module-name)))

(ert-deftest py-ert-py-execute-buffer-test ()
  (should (functionp 'py-execute-buffer)))

(ert-deftest py-ert-py--execute-buffer-base-test ()
  (should (functionp 'py--execute-buffer-base)))

(ert-deftest py-ert-py-execute-buffer-dedicated-test ()
  (should (functionp 'py-execute-buffer-dedicated)))

(ert-deftest py-ert-py-execute-buffer-switch-test ()
  (should (functionp 'py-execute-buffer-switch)))

(ert-deftest py-ert-py-execute-buffer-no-switch-test ()
  (should (functionp 'py-execute-buffer-no-switch)))

(ert-deftest py-ert-py-execute-buffer-dedicated-switch-test ()
  (should (functionp 'py-execute-buffer-dedicated-switch)))

(ert-deftest py-ert-py-execute-region-python-test ()
  (should (functionp 'py-execute-region-python)))

(ert-deftest py-ert-py-execute-region-python-switch-test ()
  (should (functionp 'py-execute-region-python-switch)))

(ert-deftest py-ert-py-execute-region-python-no-switch-test ()
  (should (functionp 'py-execute-region-python-no-switch)))

(ert-deftest py-ert-py-execute-region-python2-test ()
  (should (functionp 'py-execute-region-python2)))

(ert-deftest py-ert-py-execute-region-python2-switch-test ()
  (should (functionp 'py-execute-region-python2-switch)))

(ert-deftest py-ert-py-execute-region-python2-no-switch-test ()
  (should (functionp 'py-execute-region-python2-no-switch)))

(ert-deftest py-ert-py-execute-region-python2.7-test ()
  (should (functionp 'py-execute-region-python2.7)))

(ert-deftest py-ert-py-execute-region-python2.7-switch-test ()
  (should (functionp 'py-execute-region-python2.7-switch)))

(ert-deftest py-ert-py-execute-region-python2.7-no-switch-test ()
  (should (functionp 'py-execute-region-python2.7-no-switch)))

(ert-deftest py-ert-py-execute-region-python3-test ()
  (should (functionp 'py-execute-region-python3)))

(ert-deftest py-ert-py-execute-region-python3-switch-test ()
  (should (functionp 'py-execute-region-python3-switch)))

(ert-deftest py-ert-py-execute-region-python3-no-switch-test ()
  (should (functionp 'py-execute-region-python3-no-switch)))

(ert-deftest py-ert-py-execute-region-python3.2-test ()
  (should (functionp 'py-execute-region-python3.2)))

(ert-deftest py-ert-py-execute-region-python3.2-switch-test ()
  (should (functionp 'py-execute-region-python3.2-switch)))

(ert-deftest py-ert-py-execute-region-python3.2-no-switch-test ()
  (should (functionp 'py-execute-region-python3.2-no-switch)))

(ert-deftest py-ert-py-execute-region-ipython-test ()
  (should (functionp 'py-execute-region-ipython)))

(ert-deftest py-ert-py-execute-region-ipython-switch-test ()
  (should (functionp 'py-execute-region-ipython-switch)))

(ert-deftest py-ert-py-execute-region-ipython-no-switch-test ()
  (should (functionp 'py-execute-region-ipython-no-switch)))

(ert-deftest py-ert-py-execute-region-jython-test ()
  (should (functionp 'py-execute-region-jython)))

(ert-deftest py-ert-py-execute-region-jython-switch-test ()
  (should (functionp 'py-execute-region-jython-switch)))

(ert-deftest py-ert-py-execute-region-jython-no-switch-test ()
  (should (functionp 'py-execute-region-jython-no-switch)))

(ert-deftest py-ert-py-execute-defun-test ()
  (should (functionp 'py-execute-defun)))

(ert-deftest py-ert-py-process-file-test ()
  (should (functionp 'py-process-file)))

(ert-deftest py-ert-py-execute-line-test ()
  (should (functionp 'py-execute-line)))

(ert-deftest py-ert-py-remove-overlays-at-point-test ()
  (should (functionp 'py-remove-overlays-at-point)))

(ert-deftest py-ert-py-mouseto-exception-test ()
  (should (functionp 'py-mouseto-exception)))

(ert-deftest py-ert-py-goto-exception-test ()
  (should (functionp 'py-goto-exception)))

(ert-deftest py-ert-py--find-next-exception-test ()
  (should (functionp 'py--find-next-exception)))

(ert-deftest py-ert-py-down-exception-test ()
  (should (functionp 'py-down-exception)))

(ert-deftest py-ert-py-up-exception-test ()
  (should (functionp 'py-up-exception)))

(ert-deftest py-ert-py--postprocess-intern-test ()
  (should (functionp 'py--postprocess-intern)))

(ert-deftest py-ert-py--find-next-exception-prepare-test ()
  (should (functionp 'py--find-next-exception-prepare)))

(ert-deftest py-ert-python-test ()
  (should (functionp 'python)))

(ert-deftest py-ert-ipython-test ()
  (should (functionp 'ipython)))

(ert-deftest py-ert-python2-test ()
  (should (functionp 'python2)))

(ert-deftest py-ert-jython-test ()
  (should (functionp 'jython)))

(ert-deftest py-ert-python3-test ()
  (should (functionp 'python3)))

(ert-deftest py-ert-python-dedicated-test ()
  (should (functionp 'python-dedicated)))

(ert-deftest py-ert-ipython-dedicated-test ()
  (should (functionp 'ipython-dedicated)))

(ert-deftest py-ert-python2-dedicated-test ()
  (should (functionp 'python2-dedicated)))

(ert-deftest py-ert-jython-dedicated-test ()
  (should (functionp 'jython-dedicated)))

(ert-deftest py-ert-python3-dedicated-test ()
  (should (functionp 'python3-dedicated)))

(ert-deftest py-ert-python-switch-test ()
  (should (functionp 'python-switch)))

(ert-deftest py-ert-ipython-switch-test ()
  (should (functionp 'ipython-switch)))

(ert-deftest py-ert-python2-switch-test ()
  (should (functionp 'python2-switch)))

(ert-deftest py-ert-jython-switch-test ()
  (should (functionp 'jython-switch)))

(ert-deftest py-ert-python3-switch-test ()
  (should (functionp 'python3-switch)))

(ert-deftest py-ert-python-no-switch-test ()
  (should (functionp 'python-no-switch)))

(ert-deftest py-ert-ipython-no-switch-test ()
  (should (functionp 'ipython-no-switch)))

(ert-deftest py-ert-python2-no-switch-test ()
  (should (functionp 'python2-no-switch)))

(ert-deftest py-ert-jython-no-switch-test ()
  (should (functionp 'jython-no-switch)))

(ert-deftest py-ert-python3-no-switch-test ()
  (should (functionp 'python3-no-switch)))

(ert-deftest py-ert-python-switch-dedicated-test ()
  (should (functionp 'python-switch-dedicated)))

(ert-deftest py-ert-ipython-switch-dedicated-test ()
  (should (functionp 'ipython-switch-dedicated)))

(ert-deftest py-ert-python2-switch-dedicated-test ()
  (should (functionp 'python2-switch-dedicated)))

(ert-deftest py-ert-jython-switch-dedicated-test ()
  (should (functionp 'jython-switch-dedicated)))

(ert-deftest py-ert-python3-switch-dedicated-test ()
  (should (functionp 'python3-switch-dedicated)))

(ert-deftest py-ert-py-hide-base-test ()
  (should (functionp 'py-hide-base)))

(ert-deftest py-ert-py-show-base-test ()
  (should (functionp 'py-show-base)))

(ert-deftest py-ert-py-hide-show-test ()
  (should (functionp 'py-hide-show)))

(ert-deftest py-ert-py-hide-region-test ()
  (should (functionp 'py-hide-region)))

(ert-deftest py-ert-py-show-region-test ()
  (should (functionp 'py-show-region)))

(ert-deftest py-ert-py-hide-statement-test ()
  (should (functionp 'py-hide-statement)))

(ert-deftest py-ert-py-show-statement-test ()
  (should (functionp 'py-show-statement)))

(ert-deftest py-ert-py-hide-block-test ()
  (should (functionp 'py-hide-block)))

(ert-deftest py-ert-py-show-block-test ()
  (should (functionp 'py-show-block)))

(ert-deftest py-ert-py-hide-clause-test ()
  (should (functionp 'py-hide-clause)))

(ert-deftest py-ert-py-show-clause-test ()
  (should (functionp 'py-show-clause)))

(ert-deftest py-ert-py-hide-block-or-clause-test ()
  (should (functionp 'py-hide-block-or-clause)))

(ert-deftest py-ert-py-show-block-or-clause-test ()
  (should (functionp 'py-show-block-or-clause)))

(ert-deftest py-ert-py-hide-def-test ()
  (should (functionp 'py-hide-def)))

(ert-deftest py-ert-py-show-def-test ()
  (should (functionp 'py-show-def)))

(ert-deftest py-ert-py-hide-class-test ()
  (should (functionp 'py-hide-class)))

(ert-deftest py-ert-py-show-class-test ()
  (should (functionp 'py-show-class)))

(ert-deftest py-ert-py-hide-expression-test ()
  (should (functionp 'py-hide-expression)))

(ert-deftest py-ert-py-show-expression-test ()
  (should (functionp 'py-show-expression)))

(ert-deftest py-ert-py-hide-partial-expression-test ()
  (should (functionp 'py-hide-partial-expression)))

(ert-deftest py-ert-py-show-partial-expression-test ()
  (should (functionp 'py-show-partial-expression)))

(ert-deftest py-ert-py-hide-line-test ()
  (should (functionp 'py-hide-line)))

(ert-deftest py-ert-py-show-line-test ()
  (should (functionp 'py-show-line)))

(ert-deftest py-ert-py-hide-top-level-test ()
  (should (functionp 'py-hide-top-level)))

(ert-deftest py-ert-py-show-top-level-test ()
  (should (functionp 'py-show-top-level)))

(ert-deftest py-ert-py-copy-statement-test ()
  (should (functionp 'py-copy-statement)))

(ert-deftest py-ert-py-copy-statement-bol-test ()
  (should (functionp 'py-copy-statement-bol)))

(ert-deftest py-ert-py-copy-top-level-test ()
  (should (functionp 'py-copy-top-level)))

(ert-deftest py-ert-py-copy-top-level-bol-test ()
  (should (functionp 'py-copy-top-level-bol)))

(ert-deftest py-ert-py-copy-block-test ()
  (should (functionp 'py-copy-block)))

(ert-deftest py-ert-py-copy-block-bol-test ()
  (should (functionp 'py-copy-block-bol)))

(ert-deftest py-ert-py-copy-clause-test ()
  (should (functionp 'py-copy-clause)))

(ert-deftest py-ert-py-copy-clause-bol-test ()
  (should (functionp 'py-copy-clause-bol)))

(ert-deftest py-ert-py-copy-block-or-clause-test ()
  (should (functionp 'py-copy-block-or-clause)))

(ert-deftest py-ert-py-copy-block-or-clause-bol-test ()
  (should (functionp 'py-copy-block-or-clause-bol)))

(ert-deftest py-ert-py-copy-def-test ()
  (should (functionp 'py-copy-def)))

(ert-deftest py-ert-py-copy-def-bol-test ()
  (should (functionp 'py-copy-def-bol)))

(ert-deftest py-ert-py-copy-class-test ()
  (should (functionp 'py-copy-class)))

(ert-deftest py-ert-py-copy-class-bol-test ()
  (should (functionp 'py-copy-class-bol)))

(ert-deftest py-ert-py-copy-def-or-class-test ()
  (should (functionp 'py-copy-def-or-class)))

(ert-deftest py-ert-py-copy-def-or-class-bol-test ()
  (should (functionp 'py-copy-def-or-class-bol)))

(ert-deftest py-ert-py-copy-expression-test ()
  (should (functionp 'py-copy-expression)))

(ert-deftest py-ert-py-copy-expression-bol-test ()
  (should (functionp 'py-copy-expression-bol)))

(ert-deftest py-ert-py-copy-partial-expression-test ()
  (should (functionp 'py-copy-partial-expression)))

(ert-deftest py-ert-py-copy-partial-expression-bol-test ()
  (should (functionp 'py-copy-partial-expression-bol)))

(ert-deftest py-ert-py-copy-minor-block-test ()
  (should (functionp 'py-copy-minor-block)))

(ert-deftest py-ert-py-copy-minor-block-bol-test ()
  (should (functionp 'py-copy-minor-block-bol)))

(ert-deftest py-ert-py-copy-statement-test ()
  (should (functionp 'py-copy-statement)))

(ert-deftest py-ert-py-copy-top-level-test ()
  (should (functionp 'py-copy-top-level)))

(ert-deftest py-ert-py-copy-block-test ()
  (should (functionp 'py-copy-block)))

(ert-deftest py-ert-py-copy-clause-test ()
  (should (functionp 'py-copy-clause)))

(ert-deftest py-ert-py-copy-block-or-clause-test ()
  (should (functionp 'py-copy-block-or-clause)))

(ert-deftest py-ert-py-copy-def-test ()
  (should (functionp 'py-copy-def)))

(ert-deftest py-ert-py-copy-class-test ()
  (should (functionp 'py-copy-class)))

(ert-deftest py-ert-py-copy-def-or-class-test ()
  (should (functionp 'py-copy-def-or-class)))

(ert-deftest py-ert-py-copy-expression-test ()
  (should (functionp 'py-copy-expression)))

(ert-deftest py-ert-py-copy-partial-expression-test ()
  (should (functionp 'py-copy-partial-expression)))

(ert-deftest py-ert-py-copy-minor-block-test ()
  (should (functionp 'py-copy-minor-block)))

(ert-deftest py-ert-py-statement-test ()
  (should (functionp 'py-statement)))

(ert-deftest py-ert-py-top-level-test ()
  (should (functionp 'py-top-level)))

(ert-deftest py-ert-py-block-test ()
  (should (functionp 'py-block)))

(ert-deftest py-ert-py-clause-test ()
  (should (functionp 'py-clause)))

(ert-deftest py-ert-py-block-or-clause-test ()
  (should (functionp 'py-block-or-clause)))

(ert-deftest py-ert-py-def-test ()
  (should (functionp 'py-def)))

(ert-deftest py-ert-py-class-test ()
  (should (functionp 'py-class)))

(ert-deftest py-ert-py-def-or-class-test ()
  (should (functionp 'py-def-or-class)))

(ert-deftest py-ert-py-expression-test ()
  (should (functionp 'py-expression)))

(ert-deftest py-ert-py-partial-expression-test ()
  (should (functionp 'py-partial-expression)))

(ert-deftest py-ert-py-minor-block-test ()
  (should (functionp 'py-minor-block)))

(ert-deftest py-ert-py-output-buffer-filter-test ()
  (should (functionp 'py-output-buffer-filter)))

(ert-deftest py-ert-py-output-filter-test ()
  (should (functionp 'py-output-filter)))

(ert-deftest py-ert-py-send-string-test ()
  (should (functionp 'py-send-string)))

(ert-deftest py-ert-py-copy-statement-test ()
  (should (functionp 'py-copy-statement)))

(ert-deftest py-ert-py-copy-statement-bol-test ()
  (should (functionp 'py-copy-statement-bol)))

(ert-deftest py-ert-py-copy-top-level-test ()
  (should (functionp 'py-copy-top-level)))

(ert-deftest py-ert-py-copy-top-level-bol-test ()
  (should (functionp 'py-copy-top-level-bol)))

(ert-deftest py-ert-py-copy-block-test ()
  (should (functionp 'py-copy-block)))

(ert-deftest py-ert-py-copy-block-bol-test ()
  (should (functionp 'py-copy-block-bol)))

(ert-deftest py-ert-py-copy-clause-test ()
  (should (functionp 'py-copy-clause)))

(ert-deftest py-ert-py-copy-clause-bol-test ()
  (should (functionp 'py-copy-clause-bol)))

(ert-deftest py-ert-py-copy-block-or-clause-test ()
  (should (functionp 'py-copy-block-or-clause)))

(ert-deftest py-ert-py-copy-block-or-clause-bol-test ()
  (should (functionp 'py-copy-block-or-clause-bol)))

(ert-deftest py-ert-py-copy-def-test ()
  (should (functionp 'py-copy-def)))

(ert-deftest py-ert-py-copy-def-bol-test ()
  (should (functionp 'py-copy-def-bol)))

(ert-deftest py-ert-py-copy-class-test ()
  (should (functionp 'py-copy-class)))

(ert-deftest py-ert-py-copy-class-bol-test ()
  (should (functionp 'py-copy-class-bol)))

(ert-deftest py-ert-py-copy-def-or-class-test ()
  (should (functionp 'py-copy-def-or-class)))

(ert-deftest py-ert-py-copy-def-or-class-bol-test ()
  (should (functionp 'py-copy-def-or-class-bol)))

(ert-deftest py-ert-py-copy-expression-test ()
  (should (functionp 'py-copy-expression)))

(ert-deftest py-ert-py-copy-expression-bol-test ()
  (should (functionp 'py-copy-expression-bol)))

(ert-deftest py-ert-py-copy-partial-expression-test ()
  (should (functionp 'py-copy-partial-expression)))

(ert-deftest py-ert-py-copy-partial-expression-bol-test ()
  (should (functionp 'py-copy-partial-expression-bol)))

(ert-deftest py-ert-py-copy-minor-block-test ()
  (should (functionp 'py-copy-minor-block)))

(ert-deftest py-ert-py-copy-minor-block-bol-test ()
  (should (functionp 'py-copy-minor-block-bol)))

(ert-deftest py-ert-py-delete-statement-test ()
  (should (functionp 'py-delete-statement)))

(ert-deftest py-ert-py-delete-top-level-test ()
  (should (functionp 'py-delete-top-level)))

(ert-deftest py-ert-py-delete-block-test ()
  (should (functionp 'py-delete-block)))

(ert-deftest py-ert-py-delete-clause-test ()
  (should (functionp 'py-delete-clause)))

(ert-deftest py-ert-py-delete-block-or-clause-test ()
  (should (functionp 'py-delete-block-or-clause)))

(ert-deftest py-ert-py-delete-def-test ()
  (should (functionp 'py-delete-def)))

(ert-deftest py-ert-py-delete-class-test ()
  (should (functionp 'py-delete-class)))

(ert-deftest py-ert-py-delete-def-or-class-test ()
  (should (functionp 'py-delete-def-or-class)))

(ert-deftest py-ert-py-delete-expression-test ()
  (should (functionp 'py-delete-expression)))

(ert-deftest py-ert-py-delete-partial-expression-test ()
  (should (functionp 'py-delete-partial-expression)))

(ert-deftest py-ert-py-delete-minor-block-test ()
  (should (functionp 'py-delete-minor-block)))

(ert-deftest py-ert-py-delete-paragraph-bol-test ()
  (should (functionp 'py-delete-paragraph-bol)))

(ert-deftest py-ert-py-delete-block-bol-test ()
  (should (functionp 'py-delete-block-bol)))

(ert-deftest py-ert-py-delete-minor-block-bol-test ()
  (should (functionp 'py-delete-minor-block-bol)))

(ert-deftest py-ert-py-delete-clause-bol-test ()
  (should (functionp 'py-delete-clause-bol)))

(ert-deftest py-ert-py-delete-block-or-clause-bol-test ()
  (should (functionp 'py-delete-block-or-clause-bol)))

(ert-deftest py-ert-py-delete-def-bol-test ()
  (should (functionp 'py-delete-def-bol)))

(ert-deftest py-ert-py-delete-class-bol-test ()
  (should (functionp 'py-delete-class-bol)))

(ert-deftest py-ert-py-delete-def-or-class-bol-test ()
  (should (functionp 'py-delete-def-or-class-bol)))

(ert-deftest py-ert-py-delete-statement-bol-test ()
  (should (functionp 'py-delete-statement-bol)))

(ert-deftest py-ert-py-delete-statement-test ()
  (should (functionp 'py-delete-statement)))

(ert-deftest py-ert-py-delete-top-level-test ()
  (should (functionp 'py-delete-top-level)))

(ert-deftest py-ert-py-delete-block-test ()
  (should (functionp 'py-delete-block)))

(ert-deftest py-ert-py-delete-block-or-clause-test ()
  (should (functionp 'py-delete-block-or-clause)))

(ert-deftest py-ert-py-delete-def-test ()
  (should (functionp 'py-delete-def)))

(ert-deftest py-ert-py-delete-class-test ()
  (should (functionp 'py-delete-class)))

(ert-deftest py-ert-py-delete-def-or-class-test ()
  (should (functionp 'py-delete-def-or-class)))

(ert-deftest py-ert-py-delete-expression-test ()
  (should (functionp 'py-delete-expression)))

(ert-deftest py-ert-py-delete-partial-expression-test ()
  (should (functionp 'py-delete-partial-expression)))

(ert-deftest py-ert-py-delete-minor-block-test ()
  (should (functionp 'py-delete-minor-block)))

(ert-deftest py-ert-py--beginning-of-statement-position-test ()
  (should (functionp 'py--beginning-of-statement-position)))

(ert-deftest py-ert-py--beginning-of-block-position-test ()
  (should (functionp 'py--beginning-of-block-position)))

(ert-deftest py-ert-py--beginning-of-clause-position-test ()
  (should (functionp 'py--beginning-of-clause-position)))

(ert-deftest py-ert-py--beginning-of-block-or-clause-position-test ()
  (should (functionp 'py--beginning-of-block-or-clause-position)))

(ert-deftest py-ert-py--beginning-of-def-position-test ()
  (should (functionp 'py--beginning-of-def-position)))

(ert-deftest py-ert-py--beginning-of-class-position-test ()
  (should (functionp 'py--beginning-of-class-position)))

(ert-deftest py-ert-py--beginning-of-def-or-class-position-test ()
  (should (functionp 'py--beginning-of-def-or-class-position)))

(ert-deftest py-ert-py--beginning-of-expression-position-test ()
  (should (functionp 'py--beginning-of-expression-position)))

(ert-deftest py-ert-py--beginning-of-partial-expression-position-test ()
  (should (functionp 'py--beginning-of-partial-expression-position)))

(ert-deftest py-ert-py--beginning-of-minor-block-position-test ()
  (should (functionp 'py--beginning-of-minor-block-position)))

(ert-deftest py-ert-py--beginning-of-if-block-position-test ()
  (should (functionp 'py--beginning-of-if-block-position)))

(ert-deftest py-ert-py--beginning-of-try-block-position-test ()
  (should (functionp 'py--beginning-of-try-block-position)))

(ert-deftest py-ert-py--beginning-of-except-block-position-test ()
  (should (functionp 'py--beginning-of-except-block-position)))

(ert-deftest py-ert-py--beginning-of-top-level-position-test ()
  (should (functionp 'py--beginning-of-top-level-position)))

(ert-deftest py-ert-py--beginning-of-statement-position-bol-test ()
  (should (functionp 'py--beginning-of-statement-position-bol)))

(ert-deftest py-ert-py--beginning-of-block-position-bol-test ()
  (should (functionp 'py--beginning-of-block-position-bol)))

(ert-deftest py-ert-py--beginning-of-clause-position-bol-test ()
  (should (functionp 'py--beginning-of-clause-position-bol)))

(ert-deftest py-ert-py--beginning-of-block-or-clause-position-bol-test ()
  (should (functionp 'py--beginning-of-block-or-clause-position-bol)))

(ert-deftest py-ert-py--beginning-of-def-position-bol-test ()
  (should (functionp 'py--beginning-of-def-position-bol)))

(ert-deftest py-ert-py--beginning-of-class-position-bol-test ()
  (should (functionp 'py--beginning-of-class-position-bol)))

(ert-deftest py-ert-py--beginning-of-def-or-class-position-bol-test ()
  (should (functionp 'py--beginning-of-def-or-class-position-bol)))

(ert-deftest py-ert-py--beginning-of-minor-block-position-bol-test ()
  (should (functionp 'py--beginning-of-minor-block-position-bol)))

(ert-deftest py-ert-py--beginning-of-if-block-position-bol-test ()
  (should (functionp 'py--beginning-of-if-block-position-bol)))

(ert-deftest py-ert-py--beginning-of-try-block-position-bol-test ()
  (should (functionp 'py--beginning-of-try-block-position-bol)))

(ert-deftest py-ert-py--beginning-of-except-block-position-bol-test ()
  (should (functionp 'py--beginning-of-except-block-position-bol)))

(ert-deftest py-ert-py--beginning-of-top-level-position-bol-test ()
  (should (functionp 'py--beginning-of-top-level-position-bol)))

(ert-deftest py-ert-py-fill-string-django-test ()
  (should (functionp 'py-fill-string-django)))

(ert-deftest py-ert-py-fill-string-onetwo-test ()
  (should (functionp 'py-fill-string-onetwo)))

(ert-deftest py-ert-py-fill-string-pep-257-test ()
  (should (functionp 'py-fill-string-pep-257)))

(ert-deftest py-ert-py-fill-string-pep-257-nn-test ()
  (should (functionp 'py-fill-string-pep-257-nn)))

(ert-deftest py-ert-py-fill-string-symmetric-test ()
  (should (functionp 'py-fill-string-symmetric)))

(ert-deftest py-ert-py-set-nil-docstring-style-test ()
  (should (functionp 'py-set-nil-docstring-style)))

(ert-deftest py-ert-py-set-pep-257-nn-docstring-style-test ()
  (should (functionp 'py-set-pep-257-nn-docstring-style)))

(ert-deftest py-ert-py-set-pep-257-docstring-style-test ()
  (should (functionp 'py-set-pep-257-docstring-style)))

(ert-deftest py-ert-py-set-django-docstring-style-test ()
  (should (functionp 'py-set-django-docstring-style)))

(ert-deftest py-ert-py-set-symmetric-docstring-style-test ()
  (should (functionp 'py-set-symmetric-docstring-style)))

(ert-deftest py-ert-py-set-onetwo-docstring-style-test ()
  (should (functionp 'py-set-onetwo-docstring-style)))

(ert-deftest py-ert-py-fill-decorator-test ()
  (should (functionp 'py-fill-decorator)))

(ert-deftest py-ert-py-fill-comment-test ()
  (should (functionp 'py-fill-comment)))

(ert-deftest py-ert-py-fill-labelled-string-test ()
  (should (functionp 'py-fill-labelled-string)))

(ert-deftest py-ert-py--in-or-behind-or-before-a-docstring-test ()
  (should (functionp 'py--in-or-behind-or-before-a-docstring)))

(ert-deftest py-ert-py--string-fence-delete-spaces-test ()
  (should (functionp 'py--string-fence-delete-spaces)))

(ert-deftest py-ert-py--fill-fix-end-test ()
  (should (functionp 'py--fill-fix-end)))

(ert-deftest py-ert-py--fill-docstring-base-test ()
  (should (functionp 'py--fill-docstring-base)))

(ert-deftest py-ert-py--fill-docstring-last-line-test ()
  (should (functionp 'py--fill-docstring-last-line)))

(ert-deftest py-ert-py--fill-docstring-first-line-test ()
  (should (functionp 'py--fill-docstring-first-line)))

(ert-deftest py-ert-py--fill-docstring-test ()
  (should (functionp 'py--fill-docstring)))

(ert-deftest py-ert-py-fill-string-test ()
  (should (functionp 'py-fill-string)))

(ert-deftest py-ert-py-fill-paragraph-test ()
  (should (functionp 'py-fill-paragraph)))

(ert-deftest py-ert-py-insert-default-shebang-test ()
  (should (functionp 'py-insert-default-shebang)))

(ert-deftest py-ert-py--top-level-form-p-test ()
  (should (functionp 'py--top-level-form-p)))

(ert-deftest py-ert-py-indent-line-outmost-test ()
  (should (functionp 'py-indent-line-outmost)))

(ert-deftest py-ert-py--indent-fix-region-intern-test ()
  (should (functionp 'py--indent-fix-region-intern)))

(ert-deftest py-ert-py--indent-line-intern-test ()
  (should (functionp 'py--indent-line-intern)))

(ert-deftest py-ert-py--indent-line-base-test ()
  (should (functionp 'py--indent-line-base)))

(ert-deftest py-ert-py--calculate-indent-backwards-test ()
  (should (functionp 'py--calculate-indent-backwards)))

(ert-deftest py-ert-py-indent-line-test ()
  (should (functionp 'py-indent-line)))

(ert-deftest py-ert-py--delete-trailing-whitespace-test ()
  (should (functionp 'py--delete-trailing-whitespace)))

(ert-deftest py-ert-py-newline-and-indent-test ()
  (should (functionp 'py-newline-and-indent)))

(ert-deftest py-ert-py-newline-and-dedent-test ()
  (should (functionp 'py-newline-and-dedent)))

(ert-deftest py-ert-py-toggle-indent-tabs-mode-test ()
  (should (functionp 'py-toggle-indent-tabs-mode)))

(ert-deftest py-ert-py-indent-tabs-mode-test ()
  (should (functionp 'py-indent-tabs-mode)))

(ert-deftest py-ert-py-indent-tabs-mode-on-test ()
  (should (functionp 'py-indent-tabs-mode-on)))

(ert-deftest py-ert-py-indent-tabs-mode-off-test ()
  (should (functionp 'py-indent-tabs-mode-off)))

(ert-deftest py-ert-py-guessed-sanity-check-test ()
  (should (functionp 'py-guessed-sanity-check)))

(ert-deftest py-ert-py--guess-indent-final-test ()
  (should (functionp 'py--guess-indent-final)))

(ert-deftest py-ert-py--guess-indent-forward-test ()
  (should (functionp 'py--guess-indent-forward)))

(ert-deftest py-ert-py--guess-indent-backward-test ()
  (should (functionp 'py--guess-indent-backward)))

(ert-deftest py-ert-py-guess-indent-offset-test ()
  (should (functionp 'py-guess-indent-offset)))

(ert-deftest py-ert-py--comment-indent-function-test ()
  (should (functionp 'py--comment-indent-function)))

(ert-deftest py-ert-py-narrow-to-defun-test ()
  (should (functionp 'py-narrow-to-defun)))

(ert-deftest py-ert-py-beginning-of-paragraph-test ()
  (should (functionp 'py-beginning-of-paragraph)))

(ert-deftest py-ert-py-end-of-paragraph-test ()
  (should (functionp 'py-end-of-paragraph)))

(ert-deftest py-ert-py-indent-and-forward-test ()
  (should (functionp 'py-indent-and-forward)))

(ert-deftest py-ert-py-indent-region-test ()
  (should (functionp 'py-indent-region)))

(ert-deftest py-ert-py--beginning-of-buffer-position-test ()
  (should (functionp 'py--beginning-of-buffer-position)))

(ert-deftest py-ert-py--end-of-buffer-position-test ()
  (should (functionp 'py--end-of-buffer-position)))

(ert-deftest py-ert-py--bounds-of-declarations-test ()
  (should (functionp 'py--bounds-of-declarations)))

(ert-deftest py-ert-py-beginning-of-declarations-test ()
  (should (functionp 'py-beginning-of-declarations)))

(ert-deftest py-ert-py-end-of-declarations-test ()
  (should (functionp 'py-end-of-declarations)))

(ert-deftest py-ert-py-declarations-test ()
  (should (functionp 'py-declarations)))

(ert-deftest py-ert-py-kill-declarations-test ()
  (should (functionp 'py-kill-declarations)))

(ert-deftest py-ert-py--bounds-of-statements-test ()
  (should (functionp 'py--bounds-of-statements)))

(ert-deftest py-ert-py-beginning-of-statements-test ()
  (should (functionp 'py-beginning-of-statements)))

(ert-deftest py-ert-py-end-of-statements-test ()
  (should (functionp 'py-end-of-statements)))

(ert-deftest py-ert-py-statements-test ()
  (should (functionp 'py-statements)))

(ert-deftest py-ert-py-kill-statements-test ()
  (should (functionp 'py-kill-statements)))

(ert-deftest py-ert-py--join-words-wrapping-test ()
  (should (functionp 'py--join-words-wrapping)))

(ert-deftest py-ert-py-insert-super-test ()
  (should (functionp 'py-insert-super)))

(ert-deftest py-ert-py-comment-region-test ()
  (should (functionp 'py-comment-region)))

(ert-deftest py-ert-py-delete-comments-in-def-or-class-test ()
  (should (functionp 'py-delete-comments-in-def-or-class)))

(ert-deftest py-ert-py-delete-comments-in-class-test ()
  (should (functionp 'py-delete-comments-in-class)))

(ert-deftest py-ert-py-delete-comments-in-block-test ()
  (should (functionp 'py-delete-comments-in-block)))

(ert-deftest py-ert-py-delete-comments-in-region-test ()
  (should (functionp 'py-delete-comments-in-region)))

(ert-deftest py-ert-py--delete-comments-intern-test ()
  (should (functionp 'py--delete-comments-intern)))

(ert-deftest py-ert-py-update-gud-pdb-history-test ()
  (should (functionp 'py-update-gud-pdb-history)))

(ert-deftest py-ert-py--pdbtrack-overlay-arrow-test ()
  (should (functionp 'py--pdbtrack-overlay-arrow)))

(ert-deftest py-ert-py--pdbtrack-track-stack-file-test ()
  (should (functionp 'py--pdbtrack-track-stack-file)))

(ert-deftest py-ert-py--pdbtrack-map-filename-test ()
  (should (functionp 'py--pdbtrack-map-filename)))

(ert-deftest py-ert-py--pdbtrack-get-source-buffer-test ()
  (should (functionp 'py--pdbtrack-get-source-buffer)))

(ert-deftest py-ert-py--pdbtrack-grub-for-buffer-test ()
  (should (functionp 'py--pdbtrack-grub-for-buffer)))

(ert-deftest py-ert-py-pdbtrack-toggle-stack-tracking-test ()
  (should (functionp 'py-pdbtrack-toggle-stack-tracking)))

(ert-deftest py-ert-turn-on-pdbtrack-test ()
  (should (functionp 'turn-on-pdbtrack)))

(ert-deftest py-ert-turn-off-pdbtrack-test ()
  (should (functionp 'turn-off-pdbtrack)))

(ert-deftest py-ert-py-execute-statement-pdb-test ()
  (should (functionp 'py-execute-statement-pdb)))

(ert-deftest py-ert-py-execute-region-pdb-test ()
  (should (functionp 'py-execute-region-pdb)))

(ert-deftest py-ert-py-pdb-execute-statement-test ()
  (should (functionp 'py-pdb-execute-statement)))

(ert-deftest py-ert-py-pdb-help-test ()
  (should (functionp 'py-pdb-help)))

(ert-deftest py-ert-py-pdb-break-test ()
  (should (functionp 'py-pdb-break)))

(ert-deftest py-ert-py-end-of-block-test ()
  (should (functionp 'py-end-of-block)))

(ert-deftest py-ert-py-end-of-block-bol-test ()
  (should (functionp 'py-end-of-block-bol)))

(ert-deftest py-ert-py-end-of-clause-test ()
  (should (functionp 'py-end-of-clause)))

(ert-deftest py-ert-py-end-of-clause-bol-test ()
  (should (functionp 'py-end-of-clause-bol)))

(ert-deftest py-ert-py-end-of-block-or-clause-test ()
  (should (functionp 'py-end-of-block-or-clause)))

(ert-deftest py-ert-py-end-of-block-or-clause-bol-test ()
  (should (functionp 'py-end-of-block-or-clause-bol)))

(ert-deftest py-ert-py-end-of-def-test ()
  (should (functionp 'py-end-of-def)))

(ert-deftest py-ert-py-end-of-def-bol-test ()
  (should (functionp 'py-end-of-def-bol)))

(ert-deftest py-ert-py-end-of-class-test ()
  (should (functionp 'py-end-of-class)))

(ert-deftest py-ert-py-end-of-class-bol-test ()
  (should (functionp 'py-end-of-class-bol)))

(ert-deftest py-ert-py-end-of-def-or-class-test ()
  (should (functionp 'py-end-of-def-or-class)))

(ert-deftest py-ert-py-end-of-def-or-class-bol-test ()
  (should (functionp 'py-end-of-def-or-class-bol)))

(ert-deftest py-ert-py-end-of-if-block-test ()
  (should (functionp 'py-end-of-if-block)))

(ert-deftest py-ert-py-end-of-if-block-bol-test ()
  (should (functionp 'py-end-of-if-block-bol)))

(ert-deftest py-ert-py-end-of-try-block-test ()
  (should (functionp 'py-end-of-try-block)))

(ert-deftest py-ert-py-end-of-try-block-bol-test ()
  (should (functionp 'py-end-of-try-block-bol)))

(ert-deftest py-ert-py-end-of-minor-block-test ()
  (should (functionp 'py-end-of-minor-block)))

(ert-deftest py-ert-py-end-of-minor-block-bol-test ()
  (should (functionp 'py-end-of-minor-block-bol)))

(ert-deftest py-ert-py-end-of-for-block-test ()
  (should (functionp 'py-end-of-for-block)))

(ert-deftest py-ert-py-end-of-for-block-bol-test ()
  (should (functionp 'py-end-of-for-block-bol)))

(ert-deftest py-ert-py-end-of-except-block-test ()
  (should (functionp 'py-end-of-except-block)))

(ert-deftest py-ert-py-end-of-except-block-bol-test ()
  (should (functionp 'py-end-of-except-block-bol)))

(ert-deftest py-ert-py-execute-statement-test ()
  (should (functionp 'py-execute-statement)))

(ert-deftest py-ert-py-execute-block-test ()
  (should (functionp 'py-execute-block)))

(ert-deftest py-ert-py-execute-block-or-clause-test ()
  (should (functionp 'py-execute-block-or-clause)))

(ert-deftest py-ert-py-execute-def-test ()
  (should (functionp 'py-execute-def)))

(ert-deftest py-ert-py-execute-class-test ()
  (should (functionp 'py-execute-class)))

(ert-deftest py-ert-py-execute-def-or-class-test ()
  (should (functionp 'py-execute-def-or-class)))

(ert-deftest py-ert-py-execute-expression-test ()
  (should (functionp 'py-execute-expression)))

(ert-deftest py-ert-py-execute-partial-expression-test ()
  (should (functionp 'py-execute-partial-expression)))

(ert-deftest py-ert-py-execute-top-level-test ()
  (should (functionp 'py-execute-top-level)))

(ert-deftest py-ert-py-execute-clause-test ()
  (should (functionp 'py-execute-clause)))

(ert-deftest py-ert-py--end-of-statement-position-test ()
  (should (functionp 'py--end-of-statement-position)))

(ert-deftest py-ert-py--end-of-block-position-test ()
  (should (functionp 'py--end-of-block-position)))

(ert-deftest py-ert-py--end-of-clause-position-test ()
  (should (functionp 'py--end-of-clause-position)))

(ert-deftest py-ert-py--end-of-block-or-clause-position-test ()
  (should (functionp 'py--end-of-block-or-clause-position)))

(ert-deftest py-ert-py--end-of-def-position-test ()
  (should (functionp 'py--end-of-def-position)))

(ert-deftest py-ert-py--end-of-class-position-test ()
  (should (functionp 'py--end-of-class-position)))

(ert-deftest py-ert-py--end-of-def-or-class-position-test ()
  (should (functionp 'py--end-of-def-or-class-position)))

(ert-deftest py-ert-py--end-of-buffer-position-test ()
  (should (functionp 'py--end-of-buffer-position)))

(ert-deftest py-ert-py--end-of-expression-position-test ()
  (should (functionp 'py--end-of-expression-position)))

(ert-deftest py-ert-py--end-of-partial-expression-position-test ()
  (should (functionp 'py--end-of-partial-expression-position)))

(ert-deftest py-ert-py--end-of-minor-block-position-test ()
  (should (functionp 'py--end-of-minor-block-position)))

(ert-deftest py-ert-py--end-of-if-block-position-test ()
  (should (functionp 'py--end-of-if-block-position)))

(ert-deftest py-ert-py--end-of-try-block-position-test ()
  (should (functionp 'py--end-of-try-block-position)))

(ert-deftest py-ert-py--end-of-except-block-position-test ()
  (should (functionp 'py--end-of-except-block-position)))

(ert-deftest py-ert-py--end-of-top-level-position-test ()
  (should (functionp 'py--end-of-top-level-position)))

(ert-deftest py-ert-py--end-of-statement-position-bol-test ()
  (should (functionp 'py--end-of-statement-position-bol)))

(ert-deftest py-ert-py--end-of-block-position-bol-test ()
  (should (functionp 'py--end-of-block-position-bol)))

(ert-deftest py-ert-py--end-of-clause-position-bol-test ()
  (should (functionp 'py--end-of-clause-position-bol)))

(ert-deftest py-ert-py--end-of-block-or-clause-position-bol-test ()
  (should (functionp 'py--end-of-block-or-clause-position-bol)))

(ert-deftest py-ert-py--end-of-def-position-bol-test ()
  (should (functionp 'py--end-of-def-position-bol)))

(ert-deftest py-ert-py--end-of-class-position-bol-test ()
  (should (functionp 'py--end-of-class-position-bol)))

(ert-deftest py-ert-py--end-of-minor-block-position-bol-test ()
  (should (functionp 'py--end-of-minor-block-position-bol)))

(ert-deftest py-ert-py--end-of-if-block-position-bol-test ()
  (should (functionp 'py--end-of-if-block-position-bol)))

(ert-deftest py-ert-py--end-of-try-block-position-bol-test ()
  (should (functionp 'py--end-of-try-block-position-bol)))

(ert-deftest py-ert-py-toggle-highlight-indentation-test ()
  (should (functionp 'py-toggle-highlight-indentation)))

(ert-deftest py-ert-py-highlight-indentation-off-test ()
  (should (functionp 'py-highlight-indentation-off)))

(ert-deftest py-ert-py-highlight-indentation-on-test ()
  (should (functionp 'py-highlight-indentation-on)))

(ert-deftest py-ert-py-toggle-smart-indentation-test ()
  (should (functionp 'py-toggle-smart-indentation)))

(ert-deftest py-ert-py-smart-indentation-on-test ()
  (should (functionp 'py-smart-indentation-on)))

(ert-deftest py-ert-py-smart-indentation-off-test ()
  (should (functionp 'py-smart-indentation-off)))

(ert-deftest py-ert-py-toggle-sexp-function-test ()
  (should (functionp 'py-toggle-sexp-function)))

(ert-deftest py-ert-py-toggle-autopair-mode-test ()
  (should (functionp 'py-toggle-autopair-mode)))

(ert-deftest py-ert-py-autopair-mode-on-test ()
  (should (functionp 'py-autopair-mode-on)))

(ert-deftest py-ert-py-autopair-mode-off-test ()
  (should (functionp 'py-autopair-mode-off)))

(ert-deftest py-ert-toggle-py-smart-operator-mode-p-test ()
  (should (functionp 'toggle-py-smart-operator-mode-p)))

(ert-deftest py-ert-py-smart-operator-mode-p-on-test ()
  (should (functionp 'py-smart-operator-mode-p-on)))

(ert-deftest py-ert-py-smart-operator-mode-p-off-test ()
  (should (functionp 'py-smart-operator-mode-p-off)))

(ert-deftest py-ert-toggle-py-switch-buffers-on-execute-p-test ()
  (should (functionp 'toggle-py-switch-buffers-on-execute-p)))

(ert-deftest py-ert-py-switch-buffers-on-execute-p-on-test ()
  (should (functionp 'py-switch-buffers-on-execute-p-on)))

(ert-deftest py-ert-py-switch-buffers-on-execute-p-off-test ()
  (should (functionp 'py-switch-buffers-on-execute-p-off)))

(ert-deftest py-ert-toggle-py-split-window-on-execute-test ()
  (should (functionp 'toggle-py-split-window-on-execute)))

(ert-deftest py-ert-py-split-window-on-execute-on-test ()
  (should (functionp 'py-split-window-on-execute-on)))

(ert-deftest py-ert-py-split-window-on-execute-off-test ()
  (should (functionp 'py-split-window-on-execute-off)))

(ert-deftest py-ert-toggle-py-fontify-shell-buffer-p-test ()
  (should (functionp 'toggle-py-fontify-shell-buffer-p)))

(ert-deftest py-ert-py-fontify-shell-buffer-p-on-test ()
  (should (functionp 'py-fontify-shell-buffer-p-on)))

(ert-deftest py-ert-py-fontify-shell-buffer-p-off-test ()
  (should (functionp 'py-fontify-shell-buffer-p-off)))

(ert-deftest py-ert-toggle-python-mode-v5-behavior-p-test ()
  (should (functionp 'toggle-python-mode-v5-behavior-p)))

(ert-deftest py-ert-python-mode-v5-behavior-p-on-test ()
  (should (functionp 'python-mode-v5-behavior-p-on)))

(ert-deftest py-ert-python-mode-v5-behavior-p-off-test ()
  (should (functionp 'python-mode-v5-behavior-p-off)))

(ert-deftest py-ert-toggle-py-jump-on-exception-test ()
  (should (functionp 'toggle-py-jump-on-exception)))

(ert-deftest py-ert-py-jump-on-exception-on-test ()
  (should (functionp 'py-jump-on-exception-on)))

(ert-deftest py-ert-py-jump-on-exception-off-test ()
  (should (functionp 'py-jump-on-exception-off)))

(ert-deftest py-ert-toggle-py-use-current-dir-when-execute-p-test ()
  (should (functionp 'toggle-py-use-current-dir-when-execute-p)))

(ert-deftest py-ert-py-use-current-dir-when-execute-p-on-test ()
  (should (functionp 'py-use-current-dir-when-execute-p-on)))

(ert-deftest py-ert-py-use-current-dir-when-execute-p-off-test ()
  (should (functionp 'py-use-current-dir-when-execute-p-off)))

(ert-deftest py-ert-toggle-py-electric-comment-p-test ()
  (should (functionp 'toggle-py-electric-comment-p)))

(ert-deftest py-ert-py-electric-comment-p-on-test ()
  (should (functionp 'py-electric-comment-p-on)))

(ert-deftest py-ert-py-electric-comment-p-off-test ()
  (should (functionp 'py-electric-comment-p-off)))

(ert-deftest py-ert-toggle-py-underscore-word-syntax-p-test ()
  (should (functionp 'toggle-py-underscore-word-syntax-p)))

(ert-deftest py-ert-py-underscore-word-syntax-p-on-test ()
  (should (functionp 'py-underscore-word-syntax-p-on)))

(ert-deftest py-ert-py-underscore-word-syntax-p-off-test ()
  (should (functionp 'py-underscore-word-syntax-p-off)))

(ert-deftest py-ert-py-beginning-of-block-test ()
  (should (functionp 'py-beginning-of-block)))

(ert-deftest py-ert-py-beginning-of-clause-test ()
  (should (functionp 'py-beginning-of-clause)))

(ert-deftest py-ert-py-beginning-of-block-or-clause-test ()
  (should (functionp 'py-beginning-of-block-or-clause)))

(ert-deftest py-ert-py-beginning-of-def-test ()
  (should (functionp 'py-beginning-of-def)))

(ert-deftest py-ert-py-beginning-of-class-test ()
  (should (functionp 'py-beginning-of-class)))

(ert-deftest py-ert-py-beginning-of-def-or-class-test ()
  (should (functionp 'py-beginning-of-def-or-class)))

(ert-deftest py-ert-py-beginning-of-if-block-test ()
  (should (functionp 'py-beginning-of-if-block)))

(ert-deftest py-ert-py-beginning-of-try-block-test ()
  (should (functionp 'py-beginning-of-try-block)))

(ert-deftest py-ert-py-beginning-of-minor-block-test ()
  (should (functionp 'py-beginning-of-minor-block)))

(ert-deftest py-ert-py-beginning-of-for-block-test ()
  (should (functionp 'py-beginning-of-for-block)))

(ert-deftest py-ert-py-beginning-of-except-block-test ()
  (should (functionp 'py-beginning-of-except-block)))

(ert-deftest py-ert-py-beginning-of-block-bol-test ()
  (should (functionp 'py-beginning-of-block-bol)))

(ert-deftest py-ert-py-beginning-of-clause-bol-test ()
  (should (functionp 'py-beginning-of-clause-bol)))

(ert-deftest py-ert-py-beginning-of-block-or-clause-bol-test ()
  (should (functionp 'py-beginning-of-block-or-clause-bol)))

(ert-deftest py-ert-py-beginning-of-def-bol-test ()
  (should (functionp 'py-beginning-of-def-bol)))

(ert-deftest py-ert-py-beginning-of-class-bol-test ()
  (should (functionp 'py-beginning-of-class-bol)))

(ert-deftest py-ert-py-beginning-of-def-or-class-bol-test ()
  (should (functionp 'py-beginning-of-def-or-class-bol)))

(ert-deftest py-ert-py-beginning-of-if-block-bol-test ()
  (should (functionp 'py-beginning-of-if-block-bol)))

(ert-deftest py-ert-py-beginning-of-try-block-bol-test ()
  (should (functionp 'py-beginning-of-try-block-bol)))

(ert-deftest py-ert-py-beginning-of-minor-block-bol-test ()
  (should (functionp 'py-beginning-of-minor-block-bol)))

(ert-deftest py-ert-py-beginning-of-for-block-bol-test ()
  (should (functionp 'py-beginning-of-for-block-bol)))

(ert-deftest py-ert-py-beginning-of-except-block-bol-test ()
  (should (functionp 'py-beginning-of-except-block-bol)))

(ert-deftest py-ert-py-comment-auto-fill-test ()
  (should (functionp 'py-comment-auto-fill)))

(ert-deftest py-ert-py-comment-auto-fill-on-test ()
  (should (functionp 'py-comment-auto-fill-on)))

(ert-deftest py-ert-py-comment-auto-fill-off-test ()
  (should (functionp 'py-comment-auto-fill-off)))

(ert-deftest py-ert-py-beginning-of-block-test ()
  (should (functionp 'py-beginning-of-block)))

(ert-deftest py-ert-py-beginning-of-clause-test ()
  (should (functionp 'py-beginning-of-clause)))

(ert-deftest py-ert-py-beginning-of-block-or-clause-test ()
  (should (functionp 'py-beginning-of-block-or-clause)))

(ert-deftest py-ert-py-beginning-of-def-test ()
  (should (functionp 'py-beginning-of-def)))

(ert-deftest py-ert-py-beginning-of-class-test ()
  (should (functionp 'py-beginning-of-class)))

(ert-deftest py-ert-py-beginning-of-def-or-class-test ()
  (should (functionp 'py-beginning-of-def-or-class)))

(ert-deftest py-ert-py-beginning-of-if-block-test ()
  (should (functionp 'py-beginning-of-if-block)))

(ert-deftest py-ert-py-beginning-of-elif-block-test ()
  (should (functionp 'py-beginning-of-elif-block)))

(ert-deftest py-ert-py-beginning-of-else-block-test ()
  (should (functionp 'py-beginning-of-else-block)))

(ert-deftest py-ert-py-beginning-of-try-block-test ()
  (should (functionp 'py-beginning-of-try-block)))

(ert-deftest py-ert-py-beginning-of-minor-block-test ()
  (should (functionp 'py-beginning-of-minor-block)))

(ert-deftest py-ert-py-beginning-of-for-block-test ()
  (should (functionp 'py-beginning-of-for-block)))

(ert-deftest py-ert-py-beginning-of-except-block-test ()
  (should (functionp 'py-beginning-of-except-block)))

(ert-deftest py-ert-py-beginning-of-block-bol-test ()
  (should (functionp 'py-beginning-of-block-bol)))

(ert-deftest py-ert-py-beginning-of-clause-bol-test ()
  (should (functionp 'py-beginning-of-clause-bol)))

(ert-deftest py-ert-py-beginning-of-block-or-clause-bol-test ()
  (should (functionp 'py-beginning-of-block-or-clause-bol)))

(ert-deftest py-ert-py-beginning-of-def-bol-test ()
  (should (functionp 'py-beginning-of-def-bol)))

(ert-deftest py-ert-py-beginning-of-class-bol-test ()
  (should (functionp 'py-beginning-of-class-bol)))

(ert-deftest py-ert-py-beginning-of-def-or-class-bol-test ()
  (should (functionp 'py-beginning-of-def-or-class-bol)))

(ert-deftest py-ert-py-beginning-of-if-block-bol-test ()
  (should (functionp 'py-beginning-of-if-block-bol)))

(ert-deftest py-ert-py-beginning-of-elif-block-bol-test ()
  (should (functionp 'py-beginning-of-elif-block-bol)))

(ert-deftest py-ert-py-beginning-of-else-block-bol-test ()
  (should (functionp 'py-beginning-of-else-block-bol)))

(ert-deftest py-ert-py-beginning-of-try-block-bol-test ()
  (should (functionp 'py-beginning-of-try-block-bol)))

(ert-deftest py-ert-py-beginning-of-minor-block-bol-test ()
  (should (functionp 'py-beginning-of-minor-block-bol)))

(ert-deftest py-ert-py-beginning-of-for-block-bol-test ()
  (should (functionp 'py-beginning-of-for-block-bol)))

(ert-deftest py-ert-py-beginning-of-except-block-bol-test ()
  (should (functionp 'py-beginning-of-except-block-bol)))

(ert-deftest py-ert-py-indent-forward-line-test ()
  (should (functionp 'py-indent-forward-line)))

(ert-deftest py-ert-py-dedent-forward-line-test ()
  (should (functionp 'py-dedent-forward-line)))

(ert-deftest py-ert-py-dedent-test ()
  (should (functionp 'py-dedent)))

(ert-deftest py-ert-py--close-intern-test ()
  (should (functionp 'py--close-intern)))

(ert-deftest py-ert-py-close-def-test ()
  (should (functionp 'py-close-def)))

(ert-deftest py-ert-py-close-class-test ()
  (should (functionp 'py-close-class)))

(ert-deftest py-ert-py-close-clause-test ()
  (should (functionp 'py-close-clause)))

(ert-deftest py-ert-py-close-block-test ()
  (should (functionp 'py-close-block)))

(ert-deftest py-ert-py-class-at-point-test ()
  (should (functionp 'py-class-at-point)))

(ert-deftest py-ert-ar-py-function-at-point-test ()
  (should (functionp 'ar-py-function-at-point)))

(ert-deftest py-ert-ar-py-beginning-of-function-test ()
  (should (functionp 'ar-py-beginning-of-function)))

(ert-deftest py-ert-ar-py-beginning-of-class-test ()
  (should (functionp 'ar-py-beginning-of-class)))

(ert-deftest py-ert-ar-py-end-of-function-test ()
  (should (functionp 'ar-py-end-of-function)))

(ert-deftest py-ert-ar-py-line-at-point-test ()
  (should (functionp 'ar-py-line-at-point)))

(ert-deftest py-ert-ar-py-looking-at-keywords-p-test ()
  (should (functionp 'ar-py-looking-at-keywords-p)))

(ert-deftest py-ert-ar-py-match-paren-mode-test ()
  (should (functionp 'ar-py-match-paren-mode)))

(ert-deftest py-ert-ar-py-match-paren-test ()
  (should (functionp 'ar-py-match-paren)))

(ert-deftest py-ert-ar-py-documentation-test ()
  (should (functionp 'ar-py-documentation)))

(ert-deftest py-ert-eva-test ()
  (should (functionp 'eva)))

(ert-deftest py-ert-pst-here-test ()
  (should (functionp 'pst-here)))

(ert-deftest py-ert-py-printform-insert-test ()
  (should (functionp 'py-printform-insert)))

(ert-deftest py-ert-py-line-to-printform-python2-test ()
  (should (functionp 'py-line-to-printform-python2)))

(ert-deftest py-ert-py-boolswitch-test ()
  (should (functionp 'py-boolswitch)))

(ert-deftest py-ert-py-end-of-block-test ()
  (should (functionp 'py-end-of-block)))

(ert-deftest py-ert-py-end-of-block-bol-test ()
  (should (functionp 'py-end-of-block-bol)))

(ert-deftest py-ert-py-end-of-clause-test ()
  (should (functionp 'py-end-of-clause)))

(ert-deftest py-ert-py-end-of-clause-bol-test ()
  (should (functionp 'py-end-of-clause-bol)))

(ert-deftest py-ert-py-end-of-block-or-clause-test ()
  (should (functionp 'py-end-of-block-or-clause)))

(ert-deftest py-ert-py-end-of-block-or-clause-bol-test ()
  (should (functionp 'py-end-of-block-or-clause-bol)))

(ert-deftest py-ert-py-end-of-def-test ()
  (should (functionp 'py-end-of-def)))

(ert-deftest py-ert-py-end-of-def-bol-test ()
  (should (functionp 'py-end-of-def-bol)))

(ert-deftest py-ert-py-end-of-class-test ()
  (should (functionp 'py-end-of-class)))

(ert-deftest py-ert-py-end-of-class-bol-test ()
  (should (functionp 'py-end-of-class-bol)))

(ert-deftest py-ert-py-end-of-def-or-class-test ()
  (should (functionp 'py-end-of-def-or-class)))

(ert-deftest py-ert-py-end-of-def-or-class-bol-test ()
  (should (functionp 'py-end-of-def-or-class-bol)))

(ert-deftest py-ert-py-end-of-if-block-test ()
  (should (functionp 'py-end-of-if-block)))

(ert-deftest py-ert-py-end-of-if-block-bol-test ()
  (should (functionp 'py-end-of-if-block-bol)))

(ert-deftest py-ert-py-end-of-elif-block-test ()
  (should (functionp 'py-end-of-elif-block)))

(ert-deftest py-ert-py-end-of-elif-block-bol-test ()
  (should (functionp 'py-end-of-elif-block-bol)))

(ert-deftest py-ert-py-end-of-else-block-test ()
  (should (functionp 'py-end-of-else-block)))

(ert-deftest py-ert-py-end-of-else-block-bol-test ()
  (should (functionp 'py-end-of-else-block-bol)))

(ert-deftest py-ert-py-end-of-try-block-test ()
  (should (functionp 'py-end-of-try-block)))

(ert-deftest py-ert-py-end-of-try-block-bol-test ()
  (should (functionp 'py-end-of-try-block-bol)))

(ert-deftest py-ert-py-end-of-minor-block-test ()
  (should (functionp 'py-end-of-minor-block)))

(ert-deftest py-ert-py-end-of-minor-block-bol-test ()
  (should (functionp 'py-end-of-minor-block-bol)))

(ert-deftest py-ert-py-end-of-for-block-test ()
  (should (functionp 'py-end-of-for-block)))

(ert-deftest py-ert-py-end-of-for-block-bol-test ()
  (should (functionp 'py-end-of-for-block-bol)))

(ert-deftest py-ert-py-end-of-except-block-test ()
  (should (functionp 'py-end-of-except-block)))

(ert-deftest py-ert-py-end-of-except-block-bol-test ()
  (should (functionp 'py-end-of-except-block-bol)))

(ert-deftest py-ert-py-mark-paragraph-test ()
  (should (functionp 'py-mark-paragraph)))

(ert-deftest py-ert-py-mark-block-test ()
  (should (functionp 'py-mark-block)))

(ert-deftest py-ert-py-mark-minor-block-test ()
  (should (functionp 'py-mark-minor-block)))

(ert-deftest py-ert-py-mark-clause-test ()
  (should (functionp 'py-mark-clause)))

(ert-deftest py-ert-py-mark-block-or-clause-test ()
  (should (functionp 'py-mark-block-or-clause)))

(ert-deftest py-ert-py-mark-def-test ()
  (should (functionp 'py-mark-def)))

(ert-deftest py-ert-py-mark-class-test ()
  (should (functionp 'py-mark-class)))

(ert-deftest py-ert-py-mark-def-or-class-test ()
  (should (functionp 'py-mark-def-or-class)))

(ert-deftest py-ert-py-mark-line-test ()
  (should (functionp 'py-mark-line)))

(ert-deftest py-ert-py-mark-statement-test ()
  (should (functionp 'py-mark-statement)))

(ert-deftest py-ert-py-mark-comment-test ()
  (should (functionp 'py-mark-comment)))

(ert-deftest py-ert-py-mark-top-level-test ()
  (should (functionp 'py-mark-top-level)))

(ert-deftest py-ert-py-mark-partial-expression-test ()
  (should (functionp 'py-mark-partial-expression)))

(ert-deftest py-ert-py-mark-expression-test ()
  (should (functionp 'py-mark-expression)))

(ert-deftest py-ert-py-mark-paragraph-bol-test ()
  (should (functionp 'py-mark-paragraph-bol)))

(ert-deftest py-ert-py-mark-block-bol-test ()
  (should (functionp 'py-mark-block-bol)))

(ert-deftest py-ert-py-mark-minor-block-bol-test ()
  (should (functionp 'py-mark-minor-block-bol)))

(ert-deftest py-ert-py-mark-clause-bol-test ()
  (should (functionp 'py-mark-clause-bol)))

(ert-deftest py-ert-py-mark-block-or-clause-bol-test ()
  (should (functionp 'py-mark-block-or-clause-bol)))

(ert-deftest py-ert-py-mark-def-bol-test ()
  (should (functionp 'py-mark-def-bol)))

(ert-deftest py-ert-py-mark-class-bol-test ()
  (should (functionp 'py-mark-class-bol)))

(ert-deftest py-ert-py-mark-def-or-class-bol-test ()
  (should (functionp 'py-mark-def-or-class-bol)))

(ert-deftest py-ert-py-mark-line-bol-test ()
  (should (functionp 'py-mark-line-bol)))

(ert-deftest py-ert-py-mark-statement-bol-test ()
  (should (functionp 'py-mark-statement-bol)))

(ert-deftest py-ert-py-mark-comment-bol-test ()
  (should (functionp 'py-mark-comment-bol)))

(ert-deftest py-ert-py-mark-top-level-bol-test ()
  (should (functionp 'py-mark-top-level-bol)))

(ert-deftest py-ert-py-mark-partial-expression-bol-test ()
  (should (functionp 'py-mark-partial-expression-bol)))

(ert-deftest py-ert-py-mark-expression-bol-test ()
  (should (functionp 'py-mark-expression-bol)))

(ert-deftest py-ert-py--kill-emacs-hook-test ()
  (should (functionp 'py--kill-emacs-hook)))

(ert-deftest py-ert-py-python-version-test ()
  (should (functionp 'py-python-version)))

(ert-deftest py-ert-py-version-test ()
  (should (functionp 'py-version)))

(ert-deftest py-ert-py-history-input-filter-test ()
  (should (functionp 'py-history-input-filter)))

(ert-deftest py-ert-py-load-file-test ()
  (should (functionp 'py-load-file)))

(ert-deftest py-ert-py-proc-test ()
  (should (functionp 'py-proc)))

(ert-deftest py-ert-py--shell-simple-send-test ()
  (should (functionp 'py--shell-simple-send)))

(ert-deftest py-ert-py-guess-pdb-path-test ()
  (should (functionp 'py-guess-pdb-path)))

(ert-deftest py-ert-py-switch-shell-test ()
  (should (functionp 'py-switch-shell)))

(ert-deftest py-ert-py-toggle-local-default-use-test ()
  (should (functionp 'py-toggle-local-default-use)))

(ert-deftest py-ert-py--input-filter-test ()
  (should (functionp 'py--input-filter)))

(ert-deftest py-ert-py--set-auto-fill-values-test ()
  (should (functionp 'py--set-auto-fill-values)))

(ert-deftest py-ert-py--run-auto-fill-timer-test ()
  (should (functionp 'py--run-auto-fill-timer)))

(ert-deftest py-ert-py--unfontify-banner-intern-test ()
  (should (functionp 'py--unfontify-banner-intern)))

(ert-deftest py-ert-py--unfontify-banner-test ()
  (should (functionp 'py--unfontify-banner)))

(ert-deftest py-ert-py--run-unfontify-timer-test ()
  (should (functionp 'py--run-unfontify-timer)))

(ert-deftest py-ert-py-complete-auto-test ()
  (should (functionp 'py-complete-auto)))

(ert-deftest py-ert-py-set-command-args-test ()
  (should (functionp 'py-set-command-args)))

(ert-deftest py-ert-py---emacs-version-greater-23-test ()
  (should (functionp 'py---emacs-version-greater-23)))

(ert-deftest py-ert-py-beginning-of-commented-section-test ()
  (should (functionp 'py-beginning-of-commented-section)))

(ert-deftest py-ert-py--empty-arglist-indent-test ()
  (should (functionp 'py--empty-arglist-indent)))

(ert-deftest py-ert-py-symbol-at-point-test ()
  (should (functionp 'py-symbol-at-point)))

(ert-deftest py-ert-py-kill-buffer-unconditional-test ()
  (should (functionp 'py-kill-buffer-unconditional)))

(ert-deftest py-ert-py--line-backward-maybe-test ()
  (should (functionp 'py--line-backward-maybe)))

(ert-deftest py-ert-py--after-empty-line-test ()
  (should (functionp 'py--after-empty-line)))

(ert-deftest py-ert-py-compute-indentation-test ()
  (should (functionp 'py-compute-indentation)))

(ert-deftest py-ert-py--fetch-previous-indent-test ()
  (should (functionp 'py--fetch-previous-indent)))

(ert-deftest py-ert-py-continuation-offset-test ()
  (should (functionp 'py-continuation-offset)))

(ert-deftest py-ert-py-indentation-of-statement-test ()
  (should (functionp 'py-indentation-of-statement)))

(ert-deftest py-ert-py-list-beginning-position-test ()
  (should (functionp 'py-list-beginning-position)))

(ert-deftest py-ert-py-end-of-list-position-test ()
  (should (functionp 'py-end-of-list-position)))

(ert-deftest py-ert-py--in-comment-p-test ()
  (should (functionp 'py--in-comment-p)))

(ert-deftest py-ert-py-in-triplequoted-string-p-test ()
  (should (functionp 'py-in-triplequoted-string-p)))

(ert-deftest py-ert-py-in-string-p-test ()
  (should (functionp 'py-in-string-p)))

(ert-deftest py-ert-py-in-statement-p-test ()
  (should (functionp 'py-in-statement-p)))

(ert-deftest py-ert-py-beginning-of-top-level-p-test ()
  (should (functionp 'py-beginning-of-top-level-p)))

(ert-deftest py-ert-py--beginning-of-line-p-test ()
  (should (functionp 'py--beginning-of-line-p)))

(ert-deftest py-ert-py--beginning-of-buffer-p-test ()
  (should (functionp 'py--beginning-of-buffer-p)))

(ert-deftest py-ert-py--beginning-of-paragraph-p-test ()
  (should (functionp 'py--beginning-of-paragraph-p)))

(ert-deftest py-ert-py--end-of-line-p-test ()
  (should (functionp 'py--end-of-line-p)))

(ert-deftest py-ert-py--end-of-paragraph-p-test ()
  (should (functionp 'py--end-of-paragraph-p)))

(ert-deftest py-ert-py--statement-opens-block-p-test ()
  (should (functionp 'py--statement-opens-block-p)))

(ert-deftest py-ert-py--statement-opens-base-test ()
  (should (functionp 'py--statement-opens-base)))

(ert-deftest py-ert-py--statement-opens-clause-p-test ()
  (should (functionp 'py--statement-opens-clause-p)))

(ert-deftest py-ert-py--statement-opens-block-or-clause-p-test ()
  (should (functionp 'py--statement-opens-block-or-clause-p)))

(ert-deftest py-ert-py--statement-opens-class-p-test ()
  (should (functionp 'py--statement-opens-class-p)))

(ert-deftest py-ert-py--statement-opens-def-p-test ()
  (should (functionp 'py--statement-opens-def-p)))

(ert-deftest py-ert-py--statement-opens-def-or-class-p-test ()
  (should (functionp 'py--statement-opens-def-or-class-p)))

(ert-deftest py-ert-py--record-list-error-test ()
  (should (functionp 'py--record-list-error)))

(ert-deftest py-ert-py--message-error-test ()
  (should (functionp 'py--message-error)))

(ert-deftest py-ert-py--end-base-test ()
  (should (functionp 'py--end-base)))

(ert-deftest py-ert-py--look-downward-for-beginning-test ()
  (should (functionp 'py--look-downward-for-beginning)))

(ert-deftest py-ert-py-look-downward-for-clause-test ()
  (should (functionp 'py-look-downward-for-clause)))

(ert-deftest py-ert-py-current-defun-test ()
  (should (functionp 'py-current-defun)))

(ert-deftest py-ert-py-sort-imports-test ()
  (should (functionp 'py-sort-imports)))

(ert-deftest py-ert-py--in-literal-test ()
  (should (functionp 'py--in-literal)))

(ert-deftest py-ert-py-count-lines-test ()
  (should (functionp 'py-count-lines)))

(ert-deftest py-ert-py-which-function-test ()
  (should (functionp 'py-which-function)))

(ert-deftest py-ert-py--point-test ()
  (should (functionp 'py--point)))

(ert-deftest py-ert-py-install-search-local-test ()
  (should (functionp 'py-install-search-local)))

(ert-deftest py-ert-py-install-local-shells-test ()
  (should (functionp 'py-install-local-shells)))

(ert-deftest py-ert-py-end-of-string-test ()
  (should (functionp 'py-end-of-string)))

(ert-deftest py-ert-py--until-found-test ()
  (should (functionp 'py--until-found)))

(ert-deftest py-ert-py--delay-process-dependent-test ()
  (should (functionp 'py--delay-process-dependent)))

(ert-deftest py-ert-py--send-string-no-output-test ()
  (should (functionp 'py--send-string-no-output)))

(ert-deftest py-ert-py--send-string-return-output-test ()
  (should (functionp 'py--send-string-return-output)))

(ert-deftest py-ert-py-which-def-or-class-test ()
  (should (functionp 'py-which-def-or-class)))

(ert-deftest py-ert-py--beginning-of-form-intern-test ()
  (should (functionp 'py--beginning-of-form-intern)))

(ert-deftest py-ert-py--beginning-of-prepare-test ()
  (should (functionp 'py--beginning-of-prepare)))

(ert-deftest py-ert-py--fetch-first-python-buffer-test ()
  (should (functionp 'py--fetch-first-python-buffer)))

(ert-deftest py-ert-py-unload-python-el-test ()
  (should (functionp 'py-unload-python-el)))

(ert-deftest py-ert-py--skip-to-semicolon-backward-test ()
  (should (functionp 'py--skip-to-semicolon-backward)))

(ert-deftest py-ert-py--eos-in-string-test ()
  (should (functionp 'py--eos-in-string)))

(ert-deftest py-ert-py--end-of-comment-intern-test ()
  (should (functionp 'py--end-of-comment-intern)))

(ert-deftest py-ert-py--skip-to-comment-or-semicolon-test ()
  (should (functionp 'py--skip-to-comment-or-semicolon)))

(ert-deftest py-ert-py-beginning-of-top-level-test ()
  (should (functionp 'py-beginning-of-top-level)))

(ert-deftest py-ert-py-end-of-top-level-test ()
  (should (functionp 'py-end-of-top-level)))

(ert-deftest py-ert-py-end-of-top-level-bol-test ()
  (should (functionp 'py-end-of-top-level-bol)))

(ert-deftest py-ert-py-up-test ()
  (should (functionp 'py-up)))

(ert-deftest py-ert-py-down-test ()
  (should (functionp 'py-down)))

(ert-deftest py-ert-py--beginning-of-line-form-test ()
  (should (functionp 'py--beginning-of-line-form)))

(ert-deftest py-ert-py--mark-base-test ()
  (should (functionp 'py--mark-base)))

(ert-deftest py-ert-py--mark-base-bol-test ()
  (should (functionp 'py--mark-base-bol)))

(ert-deftest py-ert-py-mark-base-test ()
  (should (functionp 'py-mark-base)))

(ert-deftest py-ert-py-beginning-test ()
  (should (functionp 'py-beginning)))

(ert-deftest py-ert-py-end-test ()
  (should (functionp 'py-end)))

(ert-deftest py-ert-py-beginning-of-buffer-test ()
  (should (functionp 'py-beginning-of-buffer)))

(ert-deftest py-ert-py-end-of-buffer-test ()
  (should (functionp 'py-end-of-buffer)))

(ert-deftest py-ert-py-backward-same-level-test ()
  (should (functionp 'py-backward-same-level)))

(ert-deftest py-ert-py--end-of-line-p-test ()
  (should (functionp 'py--end-of-line-p)))

(ert-deftest py-ert-py--end-of-buffer-p-test ()
  (should (functionp 'py--end-of-buffer-p)))

(ert-deftest py-ert-py--bounds-of-region-test ()
  (should (functionp 'py--bounds-of-region)))

(ert-deftest py-ert-py--beginning-of-paragraph-position-test ()
  (should (functionp 'py--beginning-of-paragraph-position)))

(ert-deftest py-ert-py--end-of-paragraph-position-test ()
  (should (functionp 'py--end-of-paragraph-position)))

(ert-deftest py-ert-py--beginning-of-comment-position-test ()
  (should (functionp 'py--beginning-of-comment-position)))

(ert-deftest py-ert-py--end-of-comment-position-test ()
  (should (functionp 'py--end-of-comment-position)))

(ert-deftest py-ert-py-info-lookup-symbol-test ()
  (should (functionp 'py-info-lookup-symbol)))

(ert-deftest py-ert-python-after-info-look-test ()
  (should (functionp 'python-after-info-look)))

(ert-deftest py-ert-py--warn-tmp-files-left-test ()
  (should (functionp 'py--warn-tmp-files-left)))

(ert-deftest py-ert-py-fetch-docu-test ()
  (should (functionp 'py-fetch-docu)))

(ert-deftest py-ert-py-info-current-defun-test ()
  (should (functionp 'py-info-current-defun)))

(ert-deftest py-ert-py-help-at-point-test ()
  (should (functionp 'py-help-at-point)))

(ert-deftest py-ert-py--dump-help-string-test ()
  (should (functionp 'py--dump-help-string)))

(ert-deftest py-ert-py-describe-mode-test ()
  (should (functionp 'py-describe-mode)))

(ert-deftest py-ert-py-find-definition-test ()
  (should (functionp 'py-find-definition)))

(ert-deftest py-ert-py-find-imports-test ()
  (should (functionp 'py-find-imports)))

(ert-deftest py-ert-py-update-imports-test ()
  (should (functionp 'py-update-imports)))

(ert-deftest py-ert-py-pep8-run-test ()
  (should (functionp 'py-pep8-run)))

(ert-deftest py-ert-py-pep8-help-test ()
  (should (functionp 'py-pep8-help)))

(ert-deftest py-ert-py-pylint-run-test ()
  (should (functionp 'py-pylint-run)))

(ert-deftest py-ert-py-pylint-help-test ()
  (should (functionp 'py-pylint-help)))

(ert-deftest py-ert-py-pylint-doku-test ()
  (should (functionp 'py-pylint-doku)))

(ert-deftest py-ert-py-pyflakes-run-test ()
  (should (functionp 'py-pyflakes-run)))

(ert-deftest py-ert-py-pyflakes-help-test ()
  (should (functionp 'py-pyflakes-help)))

(ert-deftest py-ert-py-pyflakespep8-run-test ()
  (should (functionp 'py-pyflakespep8-run)))

(ert-deftest py-ert-py-pyflakespep8-help-test ()
  (should (functionp 'py-pyflakespep8-help)))

(ert-deftest py-ert-py-pychecker-run-test ()
  (should (functionp 'py-pychecker-run)))

(ert-deftest py-ert-py-check-command-test ()
  (should (functionp 'py-check-command)))

(ert-deftest py-ert-py-flake8-run-test ()
  (should (functionp 'py-flake8-run)))

(ert-deftest py-ert-py-flake8-help-test ()
  (should (functionp 'py-flake8-help)))

(ert-deftest py-ert-py--string-strip-test ()
  (should (functionp 'py--string-strip)))

(ert-deftest py-ert-py-nesting-level-test ()
  (should (functionp 'py-nesting-level)))

(ert-deftest py-ert-py-ffap-module-path-test ()
  (should (functionp 'py-ffap-module-path)))

(ert-deftest py-ert-py-toggle-flymake-intern-test ()
  (should (functionp 'py-toggle-flymake-intern)))

(ert-deftest py-ert-pylint-flymake-mode-test ()
  (should (functionp 'pylint-flymake-mode)))

(ert-deftest py-ert-pyflakes-flymake-mode-test ()
  (should (functionp 'pyflakes-flymake-mode)))

(ert-deftest py-ert-pychecker-flymake-mode-test ()
  (should (functionp 'pychecker-flymake-mode)))

(ert-deftest py-ert-pep8-flymake-mode-test ()
  (should (functionp 'pep8-flymake-mode)))

(ert-deftest py-ert-pyflakespep8-flymake-mode-test ()
  (should (functionp 'pyflakespep8-flymake-mode)))

(ert-deftest py-ert-variables-state-test ()
  (should (functionp 'variables-state)))

(ert-deftest py-ert-variables-base-state-test ()
  (should (functionp 'variables-base-state)))

(ert-deftest py-ert-py-kill-block-test ()
  (should (functionp 'py-kill-block)))

(ert-deftest py-ert-py-kill-clause-test ()
  (should (functionp 'py-kill-clause)))

(ert-deftest py-ert-py-kill-block-or-clause-test ()
  (should (functionp 'py-kill-block-or-clause)))

(ert-deftest py-ert-py-kill-def-test ()
  (should (functionp 'py-kill-def)))

(ert-deftest py-ert-py-kill-class-test ()
  (should (functionp 'py-kill-class)))

(ert-deftest py-ert-py-kill-def-or-class-test ()
  (should (functionp 'py-kill-def-or-class)))

(ert-deftest py-ert-py-kill-if-block-test ()
  (should (functionp 'py-kill-if-block)))

(ert-deftest py-ert-py-kill-try-block-test ()
  (should (functionp 'py-kill-try-block)))

(ert-deftest py-ert-py-kill-minor-block-test ()
  (should (functionp 'py-kill-minor-block)))

(ert-deftest py-ert-py-kill-for-block-test ()
  (should (functionp 'py-kill-for-block)))

(ert-deftest py-ert-py-kill-top-level-test ()
  (should (functionp 'py-kill-top-level)))

(ert-deftest py-ert-py-kill-statement-test ()
  (should (functionp 'py-kill-statement)))

(ert-deftest py-ert-py-kill-expression-test ()
  (should (functionp 'py-kill-expression)))

(ert-deftest py-ert-py-kill-partial-expression-test ()
  (should (functionp 'py-kill-partial-expression)))

(ert-deftest py-ert-py-kill-block-bol-test ()
  (should (functionp 'py-kill-block-bol)))

(ert-deftest py-ert-py-kill-clause-bol-test ()
  (should (functionp 'py-kill-clause-bol)))

(ert-deftest py-ert-py-kill-block-or-clause-bol-test ()
  (should (functionp 'py-kill-block-or-clause-bol)))

(ert-deftest py-ert-py-kill-def-bol-test ()
  (should (functionp 'py-kill-def-bol)))

(ert-deftest py-ert-py-kill-class-bol-test ()
  (should (functionp 'py-kill-class-bol)))

(ert-deftest py-ert-py-kill-def-or-class-bol-test ()
  (should (functionp 'py-kill-def-or-class-bol)))

(ert-deftest py-ert-py-kill-if-block-bol-test ()
  (should (functionp 'py-kill-if-block-bol)))

(ert-deftest py-ert-py-kill-try-block-bol-test ()
  (should (functionp 'py-kill-try-block-bol)))

(ert-deftest py-ert-py-kill-minor-block-bol-test ()
  (should (functionp 'py-kill-minor-block-bol)))

(ert-deftest py-ert-py-kill-for-block-bol-test ()
  (should (functionp 'py-kill-for-block-bol)))

(ert-deftest py-ert-py-kill-top-level-bol-test ()
  (should (functionp 'py-kill-top-level-bol)))

(ert-deftest py-ert-py-kill-statement-bol-test ()
  (should (functionp 'py-kill-statement-bol)))

(ert-deftest py-ert-py--bounds-of-statement-test ()
  (should (functionp 'py--bounds-of-statement)))

(ert-deftest py-ert-py--bounds-of-block-test ()
  (should (functionp 'py--bounds-of-block)))

(ert-deftest py-ert-py--bounds-of-clause-test ()
  (should (functionp 'py--bounds-of-clause)))

(ert-deftest py-ert-py--bounds-of-block-or-clause-test ()
  (should (functionp 'py--bounds-of-block-or-clause)))

(ert-deftest py-ert-py--bounds-of-def-test ()
  (should (functionp 'py--bounds-of-def)))

(ert-deftest py-ert-py--bounds-of-class-test ()
  (should (functionp 'py--bounds-of-class)))

(ert-deftest py-ert-py--bounds-of-def-or-class-test ()
  (should (functionp 'py--bounds-of-def-or-class)))

(ert-deftest py-ert-py--bounds-of-buffer-test ()
  (should (functionp 'py--bounds-of-buffer)))

(ert-deftest py-ert-py--bounds-of-expression-test ()
  (should (functionp 'py--bounds-of-expression)))

(ert-deftest py-ert-py--bounds-of-partial-expression-test ()
  (should (functionp 'py--bounds-of-partial-expression)))

(ert-deftest py-ert-py--bounds-of-minor-block-test ()
  (should (functionp 'py--bounds-of-minor-block)))

(ert-deftest py-ert-py--bounds-of-if-block-test ()
  (should (functionp 'py--bounds-of-if-block)))

(ert-deftest py-ert-py--bounds-of-try-block-test ()
  (should (functionp 'py--bounds-of-try-block)))

(ert-deftest py-ert-py--bounds-of-except-block-test ()
  (should (functionp 'py--bounds-of-except-block)))

(ert-deftest py-ert-py--bounds-of-top-level-test ()
  (should (functionp 'py--bounds-of-top-level)))

(ert-deftest py-ert-py--bounds-of-statement-bol-test ()
  (should (functionp 'py--bounds-of-statement-bol)))

(ert-deftest py-ert-py--bounds-of-block-bol-test ()
  (should (functionp 'py--bounds-of-block-bol)))

(ert-deftest py-ert-py--bounds-of-clause-bol-test ()
  (should (functionp 'py--bounds-of-clause-bol)))

(ert-deftest py-ert-py--bounds-of-block-or-clause-bol-test ()
  (should (functionp 'py--bounds-of-block-or-clause-bol)))

(ert-deftest py-ert-py--bounds-of-def-bol-test ()
  (should (functionp 'py--bounds-of-def-bol)))

(ert-deftest py-ert-py--bounds-of-class-bol-test ()
  (should (functionp 'py--bounds-of-class-bol)))

(ert-deftest py-ert-py--bounds-of-minor-block-bol-test ()
  (should (functionp 'py--bounds-of-minor-block-bol)))

(ert-deftest py-ert-py--bounds-of-if-block-bol-test ()
  (should (functionp 'py--bounds-of-if-block-bol)))

(ert-deftest py-ert-py--bounds-of-try-block-bol-test ()
  (should (functionp 'py--bounds-of-try-block-bol)))

(ert-deftest py-ert-py-smart-operator-check-test ()
  (should (functionp 'py-smart-operator-check)))

(ert-deftest py-ert-py-autopair-check-test ()
  (should (functionp 'py-autopair-check)))

(ert-deftest py-ert-py--set-ffap-form-test ()
  (should (functionp 'py--set-ffap-form)))

(ert-deftest py-ert-py--quote-syntax-test ()
  (should (functionp 'py--quote-syntax)))

(ert-deftest py-ert-py--delete-all-but-first-prompt-test ()
  (should (functionp 'py--delete-all-but-first-prompt)))

(ert-deftest py-ert-py--python-send-setup-code-intern-test ()
  (should (functionp 'py--python-send-setup-code-intern)))

(ert-deftest py-ert-py--python-send-completion-setup-code-test ()
  (should (functionp 'py--python-send-completion-setup-code)))

(ert-deftest py-ert-py--python-send-ffap-setup-code-test ()
  (should (functionp 'py--python-send-ffap-setup-code)))

(ert-deftest py-ert-py--python-send-eldoc-setup-code-test ()
  (should (functionp 'py--python-send-eldoc-setup-code)))

(ert-deftest py-ert-py--ipython-import-module-completion-test ()
  (should (functionp 'py--ipython-import-module-completion)))

(ert-deftest py-ert-py--docstring-p-test ()
  (should (functionp 'py--docstring-p)))

(ert-deftest py-ert-py--font-lock-syntactic-face-function-test ()
  (should (functionp 'py--font-lock-syntactic-face-function)))

(ert-deftest py-ert-py-choose-shell-by-shebang-test ()
  (should (functionp 'py-choose-shell-by-shebang)))

(ert-deftest py-ert-py--choose-shell-by-import-test ()
  (should (functionp 'py--choose-shell-by-import)))

(ert-deftest py-ert-py-choose-shell-by-path-test ()
  (should (functionp 'py-choose-shell-by-path)))

(ert-deftest py-ert-py-which-python-test ()
  (should (functionp 'py-which-python)))

(ert-deftest py-ert-py-python-current-environment-test ()
  (should (functionp 'py-python-current-environment)))

(ert-deftest py-ert-py--cleanup-process-name-test ()
  (should (functionp 'py--cleanup-process-name)))

(ert-deftest py-ert-py-choose-shell-test ()
  (should (functionp 'py-choose-shell)))

(ert-deftest py-ert-py--normalize-directory-test ()
  (should (functionp 'py--normalize-directory)))

(ert-deftest py-ert-py-install-directory-check-test ()
  (should (functionp 'py-install-directory-check)))

(ert-deftest py-ert-py-guess-py-install-directory-test ()
  (should (functionp 'py-guess-py-install-directory)))

(ert-deftest py-ert-py-load-pymacs-test ()
  (should (functionp 'py-load-pymacs)))

(ert-deftest py-ert-py-set-load-path-test ()
  (should (functionp 'py-set-load-path)))

(ert-deftest py-ert-py-separator-char-test ()
  (should (functionp 'py-separator-char)))

(ert-deftest py-ert-pps-emacs-version-test ()
  (should (functionp 'pps-emacs-version)))

(ert-deftest py-ert-py-in-string-or-comment-p-test ()
  (should (functionp 'py-in-string-or-comment-p)))

(ert-deftest py-ert-py-electric-colon-test ()
  (should (functionp 'py-electric-colon)))

(ert-deftest py-ert-py-electric-space-test ()
  (should (functionp 'py-electric-space)))

(ert-deftest py-ert-py-electric-comment-test ()
  (should (functionp 'py-electric-comment)))

(ert-deftest py-ert-py-empty-out-list-backward-test ()
  (should (functionp 'py-empty-out-list-backward)))

(ert-deftest py-ert-py-electric-backspace-test ()
  (should (functionp 'py-electric-backspace)))

(ert-deftest py-ert-py-electric-delete-test ()
  (should (functionp 'py-electric-delete)))

(ert-deftest py-ert-py-electric-yank-test ()
  (should (functionp 'py-electric-yank)))

(ert-deftest py-ert-py--beginning-of-statement-position-test ()
  (should (functionp 'py--beginning-of-statement-position)))

(ert-deftest py-ert-py--beginning-of-block-position-test ()
  (should (functionp 'py--beginning-of-block-position)))

(ert-deftest py-ert-py--beginning-of-clause-position-test ()
  (should (functionp 'py--beginning-of-clause-position)))

(ert-deftest py-ert-py--beginning-of-block-or-clause-position-test ()
  (should (functionp 'py--beginning-of-block-or-clause-position)))

(ert-deftest py-ert-py--beginning-of-def-position-test ()
  (should (functionp 'py--beginning-of-def-position)))

(ert-deftest py-ert-py--beginning-of-class-position-test ()
  (should (functionp 'py--beginning-of-class-position)))

(ert-deftest py-ert-py--beginning-of-def-or-class-position-test ()
  (should (functionp 'py--beginning-of-def-or-class-position)))

(ert-deftest py-ert-py--beginning-of-expression-position-test ()
  (should (functionp 'py--beginning-of-expression-position)))

(ert-deftest py-ert-py--beginning-of-partial-expression-position-test ()
  (should (functionp 'py--beginning-of-partial-expression-position)))

(ert-deftest py-ert-py--beginning-of-minor-block-position-test ()
  (should (functionp 'py--beginning-of-minor-block-position)))

(ert-deftest py-ert-py--beginning-of-if-block-position-test ()
  (should (functionp 'py--beginning-of-if-block-position)))

(ert-deftest py-ert-py--beginning-of-try-block-position-test ()
  (should (functionp 'py--beginning-of-try-block-position)))

(ert-deftest py-ert-py--beginning-of-except-block-position-test ()
  (should (functionp 'py--beginning-of-except-block-position)))

(ert-deftest py-ert-py--beginning-of-top-level-position-test ()
  (should (functionp 'py--beginning-of-top-level-position)))

(ert-deftest py-ert-py--beginning-of-statement-position-bol-test ()
  (should (functionp 'py--beginning-of-statement-position-bol)))

(ert-deftest py-ert-py--beginning-of-block-position-bol-test ()
  (should (functionp 'py--beginning-of-block-position-bol)))

(ert-deftest py-ert-py--beginning-of-clause-position-bol-test ()
  (should (functionp 'py--beginning-of-clause-position-bol)))

(ert-deftest py-ert-py--beginning-of-block-or-clause-position-bol-test ()
  (should (functionp 'py--beginning-of-block-or-clause-position-bol)))

(ert-deftest py-ert-py--beginning-of-def-position-bol-test ()
  (should (functionp 'py--beginning-of-def-position-bol)))

(ert-deftest py-ert-py--beginning-of-class-position-bol-test ()
  (should (functionp 'py--beginning-of-class-position-bol)))

(ert-deftest py-ert-py--beginning-of-def-or-class-position-bol-test ()
  (should (functionp 'py--beginning-of-def-or-class-position-bol)))

(ert-deftest py-ert-py--beginning-of-minor-block-position-bol-test ()
  (should (functionp 'py--beginning-of-minor-block-position-bol)))

(ert-deftest py-ert-py--beginning-of-if-block-position-bol-test ()
  (should (functionp 'py--beginning-of-if-block-position-bol)))

(ert-deftest py-ert-py--beginning-of-try-block-position-bol-test ()
  (should (functionp 'py--beginning-of-try-block-position-bol)))

(ert-deftest py-ert-py--beginning-of-except-block-position-bol-test ()
  (should (functionp 'py--beginning-of-except-block-position-bol)))

(ert-deftest py-ert-py--beginning-of-top-level-position-bol-test ()
  (should (functionp 'py--beginning-of-top-level-position-bol)))

(ert-deftest py-ert-py-beginning-of-comment-test ()
  (should (functionp 'py-beginning-of-comment)))

(ert-deftest py-ert-py-end-of-comment-test ()
  (should (functionp 'py-end-of-comment)))

(ert-deftest py-ert-py--uncomment-intern-test ()
  (should (functionp 'py--uncomment-intern)))

(ert-deftest py-ert-py-uncomment-test ()
  (should (functionp 'py-uncomment)))

(ert-deftest py-ert-py-comment-region-test ()
  (should (functionp 'py-comment-region)))

(ert-deftest py-ert-py-comment-block-test ()
  (should (functionp 'py-comment-block)))

(ert-deftest py-ert-py-comment-minor-block-test ()
  (should (functionp 'py-comment-minor-block)))

(ert-deftest py-ert-py-comment-top-level-test ()
  (should (functionp 'py-comment-top-level)))

(ert-deftest py-ert-py-comment-clause-test ()
  (should (functionp 'py-comment-clause)))

(ert-deftest py-ert-py-comment-block-or-clause-test ()
  (should (functionp 'py-comment-block-or-clause)))

(ert-deftest py-ert-py-comment-def-test ()
  (should (functionp 'py-comment-def)))

(ert-deftest py-ert-py-comment-class-test ()
  (should (functionp 'py-comment-class)))

(ert-deftest py-ert-py-comment-def-or-class-test ()
  (should (functionp 'py-comment-def-or-class)))

(ert-deftest py-ert-py-comment-statement-test ()
  (should (functionp 'py-comment-statement)))

(ert-deftest py-ert-py-delete-statement-test ()
  (should (functionp 'py-delete-statement)))

(ert-deftest py-ert-py-delete-top-level-test ()
  (should (functionp 'py-delete-top-level)))

(ert-deftest py-ert-py-delete-block-test ()
  (should (functionp 'py-delete-block)))

(ert-deftest py-ert-py-delete-clause-test ()
  (should (functionp 'py-delete-clause)))

(ert-deftest py-ert-py-delete-block-or-clause-test ()
  (should (functionp 'py-delete-block-or-clause)))

(ert-deftest py-ert-py-delete-def-test ()
  (should (functionp 'py-delete-def)))

(ert-deftest py-ert-py-delete-class-test ()
  (should (functionp 'py-delete-class)))

(ert-deftest py-ert-py-delete-def-or-class-test ()
  (should (functionp 'py-delete-def-or-class)))

(ert-deftest py-ert-py-delete-expression-test ()
  (should (functionp 'py-delete-expression)))

(ert-deftest py-ert-py-delete-partial-expression-test ()
  (should (functionp 'py-delete-partial-expression)))

(ert-deftest py-ert-py-delete-minor-block-test ()
  (should (functionp 'py-delete-minor-block)))

(ert-deftest py-ert-py-delete-paragraph-bol-test ()
  (should (functionp 'py-delete-paragraph-bol)))

(ert-deftest py-ert-py-delete-block-bol-test ()
  (should (functionp 'py-delete-block-bol)))

(ert-deftest py-ert-py-delete-minor-block-bol-test ()
  (should (functionp 'py-delete-minor-block-bol)))

(ert-deftest py-ert-py-delete-clause-bol-test ()
  (should (functionp 'py-delete-clause-bol)))

(ert-deftest py-ert-py-delete-block-or-clause-bol-test ()
  (should (functionp 'py-delete-block-or-clause-bol)))

(ert-deftest py-ert-py-delete-def-bol-test ()
  (should (functionp 'py-delete-def-bol)))

(ert-deftest py-ert-py-delete-class-bol-test ()
  (should (functionp 'py-delete-class-bol)))

(ert-deftest py-ert-py-delete-def-or-class-bol-test ()
  (should (functionp 'py-delete-def-or-class-bol)))

(ert-deftest py-ert-py-delete-statement-bol-test ()
  (should (functionp 'py-delete-statement-bol)))

(ert-deftest py-ert-py-switch-imenu-index-function-test ()
  (should (functionp 'py-switch-imenu-index-function)))

(ert-deftest py-ert-py--imenu-create-index-test ()
  (should (functionp 'py--imenu-create-index)))

(ert-deftest py-ert-py--imenu-create-index-engine-test ()
  (should (functionp 'py--imenu-create-index-engine)))

(ert-deftest py-ert-py--imenu-create-index-new-intern-test ()
  (should (functionp 'py--imenu-create-index-new-intern)))

(ert-deftest py-ert-py--imenu-create-index-new-test ()
  (should (functionp 'py--imenu-create-index-new)))

(ert-deftest py-ert-py-execute-file-python-test ()
  (should (functionp 'py-execute-file-python)))

(ert-deftest py-ert-py-execute-file-python-switch-test ()
  (should (functionp 'py-execute-file-python-switch)))

(ert-deftest py-ert-py-execute-file-python-no-switch-test ()
  (should (functionp 'py-execute-file-python-no-switch)))

(ert-deftest py-ert-py-execute-file-python-dedicated-test ()
  (should (functionp 'py-execute-file-python-dedicated)))

(ert-deftest py-ert-py-execute-file-python-dedicated-switch-test ()
  (should (functionp 'py-execute-file-python-dedicated-switch)))

(ert-deftest py-ert-py-execute-file-ipython-test ()
  (should (functionp 'py-execute-file-ipython)))

(ert-deftest py-ert-py-execute-file-ipython-switch-test ()
  (should (functionp 'py-execute-file-ipython-switch)))

(ert-deftest py-ert-py-execute-file-ipython-no-switch-test ()
  (should (functionp 'py-execute-file-ipython-no-switch)))

(ert-deftest py-ert-py-execute-file-ipython-dedicated-test ()
  (should (functionp 'py-execute-file-ipython-dedicated)))

(ert-deftest py-ert-py-execute-file-ipython-dedicated-switch-test ()
  (should (functionp 'py-execute-file-ipython-dedicated-switch)))

(ert-deftest py-ert-py-execute-file-python3-test ()
  (should (functionp 'py-execute-file-python3)))

(ert-deftest py-ert-py-execute-file-python3-switch-test ()
  (should (functionp 'py-execute-file-python3-switch)))

(ert-deftest py-ert-py-execute-file-python3-no-switch-test ()
  (should (functionp 'py-execute-file-python3-no-switch)))

(ert-deftest py-ert-py-execute-file-python3-dedicated-test ()
  (should (functionp 'py-execute-file-python3-dedicated)))

(ert-deftest py-ert-py-execute-file-python3-dedicated-switch-test ()
  (should (functionp 'py-execute-file-python3-dedicated-switch)))

(ert-deftest py-ert-py-execute-file-python2-test ()
  (should (functionp 'py-execute-file-python2)))

(ert-deftest py-ert-py-execute-file-python2-switch-test ()
  (should (functionp 'py-execute-file-python2-switch)))

(ert-deftest py-ert-py-execute-file-python2-no-switch-test ()
  (should (functionp 'py-execute-file-python2-no-switch)))

(ert-deftest py-ert-py-execute-file-python2-dedicated-test ()
  (should (functionp 'py-execute-file-python2-dedicated)))

(ert-deftest py-ert-py-execute-file-python2-dedicated-switch-test ()
  (should (functionp 'py-execute-file-python2-dedicated-switch)))

(ert-deftest py-ert-py-execute-file-python2.7-test ()
  (should (functionp 'py-execute-file-python2.7)))

(ert-deftest py-ert-py-execute-file-python2.7-switch-test ()
  (should (functionp 'py-execute-file-python2.7-switch)))

(ert-deftest py-ert-py-execute-file-python2.7-no-switch-test ()
  (should (functionp 'py-execute-file-python2.7-no-switch)))

(ert-deftest py-ert-py-execute-file-python2.7-dedicated-test ()
  (should (functionp 'py-execute-file-python2.7-dedicated)))

(ert-deftest py-ert-py-execute-file-python2.7-dedicated-switch-test ()
  (should (functionp 'py-execute-file-python2.7-dedicated-switch)))

(ert-deftest py-ert-py-execute-file-jython-test ()
  (should (functionp 'py-execute-file-jython)))

(ert-deftest py-ert-py-execute-file-jython-switch-test ()
  (should (functionp 'py-execute-file-jython-switch)))

(ert-deftest py-ert-py-execute-file-jython-no-switch-test ()
  (should (functionp 'py-execute-file-jython-no-switch)))

(ert-deftest py-ert-py-execute-file-jython-dedicated-test ()
  (should (functionp 'py-execute-file-jython-dedicated)))

(ert-deftest py-ert-py-execute-file-jython-dedicated-switch-test ()
  (should (functionp 'py-execute-file-jython-dedicated-switch)))

(ert-deftest py-ert-py-execute-file-python3.2-test ()
  (should (functionp 'py-execute-file-python3.2)))

(ert-deftest py-ert-py-execute-file-python3.2-switch-test ()
  (should (functionp 'py-execute-file-python3.2-switch)))

(ert-deftest py-ert-py-execute-file-python3.2-no-switch-test ()
  (should (functionp 'py-execute-file-python3.2-no-switch)))

(ert-deftest py-ert-py-execute-file-python3.2-dedicated-test ()
  (should (functionp 'py-execute-file-python3.2-dedicated)))

(ert-deftest py-ert-py-execute-file-python3.2-dedicated-switch-test ()
  (should (functionp 'py-execute-file-python3.2-dedicated-switch)))

(ert-deftest py-ert-py-execute-file-python3.3-test ()
  (should (functionp 'py-execute-file-python3.3)))

(ert-deftest py-ert-py-execute-file-python3.3-switch-test ()
  (should (functionp 'py-execute-file-python3.3-switch)))

(ert-deftest py-ert-py-execute-file-python3.3-no-switch-test ()
  (should (functionp 'py-execute-file-python3.3-no-switch)))

(ert-deftest py-ert-py-execute-file-python3.3-dedicated-test ()
  (should (functionp 'py-execute-file-python3.3-dedicated)))

(ert-deftest py-ert-py-execute-file-python3.3-dedicated-switch-test ()
  (should (functionp 'py-execute-file-python3.3-dedicated-switch)))

(ert-deftest py-ert-py-mark-paragraph-test ()
  (should (functionp 'py-mark-paragraph)))

(ert-deftest py-ert-py-mark-block-test ()
  (should (functionp 'py-mark-block)))

(ert-deftest py-ert-py-mark-minor-block-test ()
  (should (functionp 'py-mark-minor-block)))

(ert-deftest py-ert-py-mark-clause-test ()
  (should (functionp 'py-mark-clause)))

(ert-deftest py-ert-py-mark-block-or-clause-test ()
  (should (functionp 'py-mark-block-or-clause)))

(ert-deftest py-ert-py-mark-def-test ()
  (should (functionp 'py-mark-def)))

(ert-deftest py-ert-py-mark-class-test ()
  (should (functionp 'py-mark-class)))

(ert-deftest py-ert-py-mark-def-or-class-test ()
  (should (functionp 'py-mark-def-or-class)))

(ert-deftest py-ert-py-mark-line-test ()
  (should (functionp 'py-mark-line)))

(ert-deftest py-ert-py-mark-statement-test ()
  (should (functionp 'py-mark-statement)))

(ert-deftest py-ert-py-mark-comment-test ()
  (should (functionp 'py-mark-comment)))

(ert-deftest py-ert-py-mark-top-level-test ()
  (should (functionp 'py-mark-top-level)))

(ert-deftest py-ert-py-mark-partial-expression-test ()
  (should (functionp 'py-mark-partial-expression)))

(ert-deftest py-ert-py-mark-expression-test ()
  (should (functionp 'py-mark-expression)))

(ert-deftest py-ert-py-mark-paragraph-bol-test ()
  (should (functionp 'py-mark-paragraph-bol)))

(ert-deftest py-ert-py-mark-block-bol-test ()
  (should (functionp 'py-mark-block-bol)))

(ert-deftest py-ert-py-mark-minor-block-bol-test ()
  (should (functionp 'py-mark-minor-block-bol)))

(ert-deftest py-ert-py-mark-clause-bol-test ()
  (should (functionp 'py-mark-clause-bol)))

(ert-deftest py-ert-py-mark-block-or-clause-bol-test ()
  (should (functionp 'py-mark-block-or-clause-bol)))

(ert-deftest py-ert-py-mark-def-bol-test ()
  (should (functionp 'py-mark-def-bol)))

(ert-deftest py-ert-py-mark-class-bol-test ()
  (should (functionp 'py-mark-class-bol)))

(ert-deftest py-ert-py-mark-def-or-class-bol-test ()
  (should (functionp 'py-mark-def-or-class-bol)))

(ert-deftest py-ert-py-mark-line-bol-test ()
  (should (functionp 'py-mark-line-bol)))

(ert-deftest py-ert-py-mark-statement-bol-test ()
  (should (functionp 'py-mark-statement-bol)))

(ert-deftest py-ert-py-mark-comment-bol-test ()
  (should (functionp 'py-mark-comment-bol)))

(ert-deftest py-ert-py-mark-top-level-bol-test ()
  (should (functionp 'py-mark-top-level-bol)))

(ert-deftest py-ert-py-mark-partial-expression-bol-test ()
  (should (functionp 'py-mark-partial-expression-bol)))

(ert-deftest py-ert-py-mark-expression-bol-test ()
  (should (functionp 'py-mark-expression-bol)))

(ert-deftest py-ert-py--beginning-of-block-p-test ()
  (should (functionp 'py--beginning-of-block-p)))

(ert-deftest py-ert-py--beginning-of-clause-p-test ()
  (should (functionp 'py--beginning-of-clause-p)))

(ert-deftest py-ert-py--beginning-of-block-or-clause-p-test ()
  (should (functionp 'py--beginning-of-block-or-clause-p)))

(ert-deftest py-ert-py--beginning-of-def-p-test ()
  (should (functionp 'py--beginning-of-def-p)))

(ert-deftest py-ert-py--beginning-of-class-p-test ()
  (should (functionp 'py--beginning-of-class-p)))

(ert-deftest py-ert-py--beginning-of-def-or-class-p-test ()
  (should (functionp 'py--beginning-of-def-or-class-p)))

(ert-deftest py-ert-py--beginning-of-if-block-p-test ()
  (should (functionp 'py--beginning-of-if-block-p)))

(ert-deftest py-ert-py--beginning-of-try-block-p-test ()
  (should (functionp 'py--beginning-of-try-block-p)))

(ert-deftest py-ert-py--beginning-of-minor-block-p-test ()
  (should (functionp 'py--beginning-of-minor-block-p)))

(ert-deftest py-ert-py--beginning-of-for-block-p-test ()
  (should (functionp 'py--beginning-of-for-block-p)))

(ert-deftest py-ert-py--beginning-of-top-level-p-test ()
  (should (functionp 'py--beginning-of-top-level-p)))

(ert-deftest py-ert-py--beginning-of-statement-p-test ()
  (should (functionp 'py--beginning-of-statement-p)))

(ert-deftest py-ert-py--beginning-of-expression-p-test ()
  (should (functionp 'py--beginning-of-expression-p)))

(ert-deftest py-ert-py--beginning-of-partial-expression-p-test ()
  (should (functionp 'py--beginning-of-partial-expression-p)))

(ert-deftest py-ert-py--beginning-of-block-bol-p-test ()
  (should (functionp 'py--beginning-of-block-bol-p)))

(ert-deftest py-ert-py--beginning-of-clause-bol-p-test ()
  (should (functionp 'py--beginning-of-clause-bol-p)))

(ert-deftest py-ert-py--beginning-of-block-or-clause-bol-p-test ()
  (should (functionp 'py--beginning-of-block-or-clause-bol-p)))

(ert-deftest py-ert-py--beginning-of-def-bol-p-test ()
  (should (functionp 'py--beginning-of-def-bol-p)))

(ert-deftest py-ert-py--beginning-of-class-bol-p-test ()
  (should (functionp 'py--beginning-of-class-bol-p)))

(ert-deftest py-ert-py--beginning-of-def-or-class-bol-p-test ()
  (should (functionp 'py--beginning-of-def-or-class-bol-p)))

(ert-deftest py-ert-py--beginning-of-if-block-bol-p-test ()
  (should (functionp 'py--beginning-of-if-block-bol-p)))

(ert-deftest py-ert-py--beginning-of-try-block-bol-p-test ()
  (should (functionp 'py--beginning-of-try-block-bol-p)))

(ert-deftest py-ert-py--beginning-of-minor-block-bol-p-test ()
  (should (functionp 'py--beginning-of-minor-block-bol-p)))

(ert-deftest py-ert-py--beginning-of-for-block-bol-p-test ()
  (should (functionp 'py--beginning-of-for-block-bol-p)))

(ert-deftest py-ert-py--beginning-of-statement-bol-p-test ()
  (should (functionp 'py--beginning-of-statement-bol-p)))

(ert-deftest py-ert-py--shell-completion-get-completions-test ()
  (should (functionp 'py--shell-completion-get-completions)))

(ert-deftest py-ert-py--after-change-function-test ()
  (should (functionp 'py--after-change-function)))

(ert-deftest py-ert-py--try-completion-intern-test ()
  (should (functionp 'py--try-completion-intern)))

(ert-deftest py-ert-py--try-completion-test ()
  (should (functionp 'py--try-completion)))

(ert-deftest py-ert-py--shell--do-completion-at-point-test ()
  (should (functionp 'py--shell--do-completion-at-point)))

(ert-deftest py-ert-py--complete-base-test ()
  (should (functionp 'py--complete-base)))

(ert-deftest py-ert-py--complete-prepare-test ()
  (should (functionp 'py--complete-prepare)))

(ert-deftest py-ert-py-shell-complete-test ()
  (should (functionp 'py-shell-complete)))

(ert-deftest py-ert-py-indent-or-complete-test ()
  (should (functionp 'py-indent-or-complete)))

(ert-deftest py-ert-py-shift-left-test ()
  (should (functionp 'py-shift-left)))

(ert-deftest py-ert-py-shift-right-test ()
  (should (functionp 'py-shift-right)))

(ert-deftest py-ert-py--shift-intern-test ()
  (should (functionp 'py--shift-intern)))

(ert-deftest py-ert-py--shift-forms-base-test ()
  (should (functionp 'py--shift-forms-base)))

(ert-deftest py-ert-py-shift-paragraph-right-test ()
  (should (functionp 'py-shift-paragraph-right)))

(ert-deftest py-ert-py-shift-paragraph-left-test ()
  (should (functionp 'py-shift-paragraph-left)))

(ert-deftest py-ert-py-shift-block-right-test ()
  (should (functionp 'py-shift-block-right)))

(ert-deftest py-ert-py-shift-block-left-test ()
  (should (functionp 'py-shift-block-left)))

(ert-deftest py-ert-py-shift-minor-block-left-test ()
  (should (functionp 'py-shift-minor-block-left)))

(ert-deftest py-ert-py-shift-minor-block-right-test ()
  (should (functionp 'py-shift-minor-block-right)))

(ert-deftest py-ert-py-shift-clause-right-test ()
  (should (functionp 'py-shift-clause-right)))

(ert-deftest py-ert-py-shift-clause-left-test ()
  (should (functionp 'py-shift-clause-left)))

(ert-deftest py-ert-py-shift-block-or-clause-right-test ()
  (should (functionp 'py-shift-block-or-clause-right)))

(ert-deftest py-ert-py-shift-block-or-clause-left-test ()
  (should (functionp 'py-shift-block-or-clause-left)))

(ert-deftest py-ert-py-shift-def-right-test ()
  (should (functionp 'py-shift-def-right)))

(ert-deftest py-ert-py-shift-def-left-test ()
  (should (functionp 'py-shift-def-left)))

(ert-deftest py-ert-py-shift-class-right-test ()
  (should (functionp 'py-shift-class-right)))

(ert-deftest py-ert-py-shift-class-left-test ()
  (should (functionp 'py-shift-class-left)))

(ert-deftest py-ert-py-shift-def-or-class-right-test ()
  (should (functionp 'py-shift-def-or-class-right)))

(ert-deftest py-ert-py-shift-def-or-class-left-test ()
  (should (functionp 'py-shift-def-or-class-left)))

(ert-deftest py-ert-py-shift-line-right-test ()
  (should (functionp 'py-shift-line-right)))

(ert-deftest py-ert-py-shift-line-left-test ()
  (should (functionp 'py-shift-line-left)))

(ert-deftest py-ert-py-shift-statement-right-test ()
  (should (functionp 'py-shift-statement-right)))

(ert-deftest py-ert-py-shift-statement-left-test ()
  (should (functionp 'py-shift-statement-left)))

(ert-deftest py-ert-py--bounds-of-statement-test ()
  (should (functionp 'py--bounds-of-statement)))

(ert-deftest py-ert-py--bounds-of-block-test ()
  (should (functionp 'py--bounds-of-block)))

(ert-deftest py-ert-py--bounds-of-clause-test ()
  (should (functionp 'py--bounds-of-clause)))

(ert-deftest py-ert-py--bounds-of-block-or-clause-test ()
  (should (functionp 'py--bounds-of-block-or-clause)))

(ert-deftest py-ert-py--bounds-of-def-test ()
  (should (functionp 'py--bounds-of-def)))

(ert-deftest py-ert-py--bounds-of-class-test ()
  (should (functionp 'py--bounds-of-class)))

(ert-deftest py-ert-py--bounds-of-def-or-class-test ()
  (should (functionp 'py--bounds-of-def-or-class)))

(ert-deftest py-ert-py--bounds-of-buffer-test ()
  (should (functionp 'py--bounds-of-buffer)))

(ert-deftest py-ert-py--bounds-of-expression-test ()
  (should (functionp 'py--bounds-of-expression)))

(ert-deftest py-ert-py--bounds-of-partial-expression-test ()
  (should (functionp 'py--bounds-of-partial-expression)))

(ert-deftest py-ert-py--bounds-of-minor-block-test ()
  (should (functionp 'py--bounds-of-minor-block)))

(ert-deftest py-ert-py--bounds-of-if-block-test ()
  (should (functionp 'py--bounds-of-if-block)))

(ert-deftest py-ert-py--bounds-of-try-block-test ()
  (should (functionp 'py--bounds-of-try-block)))

(ert-deftest py-ert-py--bounds-of-except-block-test ()
  (should (functionp 'py--bounds-of-except-block)))

(ert-deftest py-ert-py--bounds-of-top-level-test ()
  (should (functionp 'py--bounds-of-top-level)))

(ert-deftest py-ert-py--bounds-of-statement-bol-test ()
  (should (functionp 'py--bounds-of-statement-bol)))

(ert-deftest py-ert-py--bounds-of-block-bol-test ()
  (should (functionp 'py--bounds-of-block-bol)))

(ert-deftest py-ert-py--bounds-of-clause-bol-test ()
  (should (functionp 'py--bounds-of-clause-bol)))

(ert-deftest py-ert-py--bounds-of-block-or-clause-bol-test ()
  (should (functionp 'py--bounds-of-block-or-clause-bol)))

(ert-deftest py-ert-py--bounds-of-def-bol-test ()
  (should (functionp 'py--bounds-of-def-bol)))

(ert-deftest py-ert-py--bounds-of-class-bol-test ()
  (should (functionp 'py--bounds-of-class-bol)))

(ert-deftest py-ert-py--bounds-of-minor-block-bol-test ()
  (should (functionp 'py--bounds-of-minor-block-bol)))

(ert-deftest py-ert-py--bounds-of-if-block-bol-test ()
  (should (functionp 'py--bounds-of-if-block-bol)))

(ert-deftest py-ert-py--bounds-of-try-block-bol-test ()
  (should (functionp 'py--bounds-of-try-block-bol)))

(ert-deftest py-ert-py-beginning-of-top-level-test ()
  (should (functionp 'py-beginning-of-top-level)))

(ert-deftest py-ert-py-end-of-top-level-test ()
  (should (functionp 'py-end-of-top-level)))

(ert-deftest py-ert-py-beginning-test ()
  (should (functionp 'py-beginning)))

(ert-deftest py-ert-py-end-test ()
  (should (functionp 'py-end)))

(ert-deftest py-ert-py-up-test ()
  (should (functionp 'py-up)))

(ert-deftest py-ert-py-down-test ()
  (should (functionp 'py-down)))

(ert-deftest py-ert-py-backward-same-level-test ()
  (should (functionp 'py-backward-same-level)))

(ert-deftest py-ert-py-end-of-block-test ()
  (should (functionp 'py-end-of-block)))

(ert-deftest py-ert-py-end-of-clause-test ()
  (should (functionp 'py-end-of-clause)))

(ert-deftest py-ert-py-end-of-block-or-clause-test ()
  (should (functionp 'py-end-of-block-or-clause)))

(ert-deftest py-ert-py-end-of-def-test ()
  (should (functionp 'py-end-of-def)))

(ert-deftest py-ert-py-end-of-class-test ()
  (should (functionp 'py-end-of-class)))

(ert-deftest py-ert-py-end-of-def-or-class-test ()
  (should (functionp 'py-end-of-def-or-class)))

(ert-deftest py-ert-py-end-of-if-block-test ()
  (should (functionp 'py-end-of-if-block)))

(ert-deftest py-ert-py-end-of-elif-block-test ()
  (should (functionp 'py-end-of-elif-block)))

(ert-deftest py-ert-py-end-of-try-block-test ()
  (should (functionp 'py-end-of-try-block)))

(ert-deftest py-ert-py-end-of-minor-block-test ()
  (should (functionp 'py-end-of-minor-block)))

(ert-deftest py-ert-py-beginning-of-buffer-test ()
  (should (functionp 'py-beginning-of-buffer)))

(ert-deftest py-ert-py-end-of-buffer-test ()
  (should (functionp 'py-end-of-buffer)))

(provide 'py-ert-function-tests)
;;; py-ert-function-tests.el ends here
