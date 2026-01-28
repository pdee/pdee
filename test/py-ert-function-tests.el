;;; py-ert-function-tests.el --- functionp ert tests  -*- lexical-binding: t; -*-

;; URL: https://gitlab.com/python-mode-devs
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

(ert-deftest py-ert-virtualenv-filter-functionp-test-2b5yDV ()
  (should (functionp 'virtualenv-filter)))

(ert-deftest py-ert-py--beginning-of-statement-p-functionp-test-sHNAQu ()
  (should (functionp 'py--beginning-of-statement-p)))

(ert-deftest py-ert-virtualenv-append-path-functionp-test-isLD43 ()
  (should (functionp 'virtualenv-append-path)))

(ert-deftest py-ert-virtualenv-add-to-path-functionp-test-aoWwfD ()
  (should (functionp 'virtualenv-add-to-path)))

(ert-deftest py-ert-virtualenv-current-functionp-test-mmrSqc ()
  (should (functionp 'virtualenv-current)))

(ert-deftest py-ert-virtualenv-deactivate-functionp-test-uoLMJM ()
  (should (functionp 'virtualenv-deactivate)))

(ert-deftest py-ert-virtualenv-activate-functionp-test-YHIx0m ()
  (should (functionp 'virtualenv-activate)))

(ert-deftest py-ert-virtualenv-p-functionp-test-K2L1hX ()
  (should (functionp 'virtualenv-p)))

(ert-deftest py-ert-virtualenv-workon-complete-functionp-test-iQBPxx ()
  (should (functionp 'virtualenv-workon-complete)))

(ert-deftest py-ert-virtualenv-workon-functionp-test-ULMEJ7 ()
  (should (functionp 'virtualenv-workon)))

(ert-deftest py-ert--beginning-of-block-p-functionp-test-uYUXTH ()
  (should (functionp 'py--beginning-of-block-p)))

(ert-deftest py-ert--beginning-of-clause-p-functionp-test-i4xI4h ()
  (should (functionp 'py--beginning-of-clause-p)))

(ert-deftest py-ert--beginning-of-block-or-clause-p-functionp-test-A5kVcS ()
  (should (functionp 'py--beginning-of-block-or-clause-p)))

(ert-deftest py-ert--beginning-of-def-p-functionp-test-IHvJjs ()
  (should (functionp 'py--beginning-of-def-p)))

(ert-deftest py-ert--beginning-of-class-p-functionp-test-olo0n2 ()
  (should (functionp 'py--beginning-of-class-p)))

(ert-deftest py-ert--beginning-of-def-or-class-p-functionp-test-IHnaqC ()
  (should (functionp 'py--beginning-of-def-or-class-p)))

(ert-deftest py-ert--beginning-of-if-block-p-functionp-test-y4Q3pc ()
  (should (functionp 'py--beginning-of-if-block-p)))

(ert-deftest py-ert--beginning-of-try-block-p-functionp-test-Q34spM ()
  (should (functionp 'py--beginning-of-try-block-p)))

(ert-deftest py-ert--beginning-of-minor-block-p-functionp-test-Setglm ()
  (should (functionp 'py--beginning-of-minor-block-p)))

(ert-deftest py-ert--beginning-of-for-block-p-functionp-test-iESliW ()
  (should (functionp 'py--beginning-of-for-block-p)))

(ert-deftest py-ert--beginning-of-top-level-p-functionp-test-WYzTbw ()
  (should (functionp 'py--beginning-of-top-level-p)))

(ert-deftest py-ert--beginning-of-expression-p-functionp-test-04H1WF ()
  (should (functionp 'py--beginning-of-expression-p)))

(ert-deftest py-ert--beginning-of-partial-expression-p-functionp-test-2Xo5Lf ()
  (should (functionp 'py--beginning-of-partial-expression-p)))

(ert-deftest py-ert--beginning-of-block-bol-p-functionp-test-qymzCP ()
  (should (functionp 'py--beginning-of-block-bol-p)))

(ert-deftest py-ert--beginning-of-clause-bol-p-functionp-test-KOq7qp ()
  (should (functionp 'py--beginning-of-clause-bol-p)))

(ert-deftest py-ert--beginning-of-block-or-clause-bol-p-functionp-test-8MsncZ ()
  (should (functionp 'py--beginning-of-block-or-clause-bol-p)))

(ert-deftest py-ert--beginning-of-def-bol-p-functionp-test-I7edYy ()
  (should (functionp 'py--beginning-of-def-bol-p)))

(ert-deftest py-ert--beginning-of-class-bol-p-functionp-test-Q3vzdc ()
  (should (functionp 'py--beginning-of-class-bol-p)))

(ert-deftest py-ert--beginning-of-def-or-class-bol-p-functionp-test-8wlarP ()
  (should (functionp 'py--beginning-of-def-or-class-bol-p)))

(ert-deftest py-ert--beginning-of-if-block-bol-p-functionp-test-Qb4XAs ()
  (should (functionp 'py--beginning-of-if-block-bol-p)))

(ert-deftest py-ert--beginning-of-try-block-bol-p-functionp-test-YJuLI5 ()
  (should (functionp 'py--beginning-of-try-block-bol-p)))

(ert-deftest py-ert--beginning-of-minor-block-bol-p-functionp-test-UdmcOI ()
  (should (functionp 'py--beginning-of-minor-block-bol-p)))

(ert-deftest py-ert--beginning-of-for-block-bol-p-functionp-test-MNH0Ql ()
  (should (functionp 'py--beginning-of-for-block-bol-p)))

(ert-deftest py-ert--beginning-of-statement-bol-p-functionp-test-iADzRY ()
  (should (functionp 'py--beginning-of-statement-bol-p)))

(ert-deftest py-ert-up-statement-functionp-test-uEtaQB ()
  (should (functionp 'py-up-statement)))

(ert-deftest py-ert-down-statement-functionp-test-Se3rMe ()
  (should (functionp 'py-down-statement)))

(ert-deftest py-ert-up-base-functionp-test-0wzwJR ()
  (should (functionp 'py-up-base)))

(ert-deftest py-ert-down-base-functionp-test-yEH2Du ()
  (should (functionp 'py-down-base)))

(ert-deftest py-ert-up-block-functionp-test-GKVsz7 ()
  (should (functionp 'py-up-block)))

(ert-deftest py-ert-up-minor-block-functionp-test-mMIwrK ()
  (should (functionp 'py-up-minor-block)))

(ert-deftest py-ert-up-def-functionp-test-Ebagjn ()
  (should (functionp 'py-up-def)))

(ert-deftest py-ert-up-class-functionp-test-6BD58Z ()
  (should (functionp 'py-up-class)))

(ert-deftest py-ert-up-def-or-class-functionp-test-Gmp2ZC ()
  (should (functionp 'py-up-def-or-class)))

(ert-deftest py-ert-down-block-functionp-test-Yh7DNf ()
  (should (functionp 'py-down-block)))

(ert-deftest py-ert-down-minor-block-functionp-test-kV2dCS ()
  (should (functionp 'py-down-minor-block)))

(ert-deftest py-ert-down-def-functionp-test-QVhFnv ()
  (should (functionp 'py-down-def)))

(ert-deftest py-ert-down-class-functionp-test-EVzy97 ()
  (should (functionp 'py-down-class)))

(ert-deftest py-ert-down-def-or-class-functionp-test-qu08RK ()
  (should (functionp 'py-down-def-or-class)))

(ert-deftest py-ert-up-block-bol-functionp-test-aMONyn ()
  (should (functionp 'py-up-block-bol)))

(ert-deftest py-ert-up-minor-block-bol-functionp-test-GMNYf0 ()
  (should (functionp 'py-up-minor-block-bol)))

(ert-deftest py-ert-up-def-bol-functionp-test-YLt45D ()
  (should (functionp 'py-up-def-bol)))

(ert-deftest py-ert-up-class-bol-functionp-test-QFTfSh ()
  (should (functionp 'py-up-class-bol)))

(ert-deftest py-ert-up-def-or-class-bol-functionp-test-KYRcDV ()
  (should (functionp 'py-up-def-or-class-bol)))

(ert-deftest py-ert-down-block-bol-functionp-test-QLwzlz ()
  (should (functionp 'py-down-block-bol)))

(ert-deftest py-ert-down-minor-block-bol-functionp-test-8WYM4c ()
  (should (functionp 'py-down-minor-block-bol)))

(ert-deftest py-ert-down-def-bol-functionp-test-Gc9hLQ ()
  (should (functionp 'py-down-def-bol)))

(ert-deftest py-ert-down-class-bol-functionp-test-88epqu ()
  (should (functionp 'py-down-class-bol)))

(ert-deftest py-ert-down-def-or-class-bol-functionp-test-OEld37 ()
  (should (functionp 'py-down-def-or-class-bol)))

(ert-deftest py-ert-kill-clause-functionp-test-cZ6UDL ()
  (should (functionp 'py-kill-clause)))

(ert-deftest py-ert-kill-block-or-clause-functionp-test-E7Y9bp ()
  (should (functionp 'py-kill-block-or-clause)))

(ert-deftest py-ert-kill-def-functionp-test-kloeI2 ()
  (should (functionp 'py-kill-def)))

(ert-deftest py-ert-kill-class-functionp-test-kBRuaG ()
  (should (functionp 'py-kill-class)))

(ert-deftest py-ert-kill-def-or-class-functionp-test-Oeb4Dj ()
  (should (functionp 'py-kill-def-or-class)))

(ert-deftest py-ert-kill-if-block-functionp-test-oZgn7W ()
  (should (functionp 'py-kill-if-block)))

(ert-deftest py-ert-kill-try-block-functionp-test-AdUUwA ()
  (should (functionp 'py-kill-try-block)))

(ert-deftest py-ert-kill-minor-block-functionp-test-s5YvXd ()
  (should (functionp 'py-kill-minor-block)))

(ert-deftest py-ert-kill-for-block-functionp-test-qcMGkR ()
  (should (functionp 'py-kill-for-block)))

(ert-deftest py-ert-kill-top-level-functionp-test-uAAYIu ()
  (should (functionp 'py-kill-top-level)))

(ert-deftest py-ert-kill-statement-functionp-test-8uOj47 ()
  (should (functionp 'py-kill-statement)))

(ert-deftest py-ert-kill-expression-functionp-test-M1HCqL ()
  (should (functionp 'py-kill-expression)))

(ert-deftest py-ert-kill-partial-expression-functionp-test-cZkYKo ()
  (should (functionp 'py-kill-partial-expression)))

(ert-deftest py-ert-backward-expression-functionp-test-E7aa21 ()
  (should (functionp 'py-backward-expression)))

(ert-deftest py-ert-forward-expression-functionp-test-uqW5jF ()
  (should (functionp 'py-forward-expression)))

(ert-deftest py-ert-backward-partial-expression-functionp-test-6Btcph ()
  (should (functionp 'py-backward-partial-expression)))

(ert-deftest py-ert-forward-partial-expression-functionp-test-czVnqT ()
  (should (functionp 'py-forward-partial-expression)))

(ert-deftest py-ert-backward-line-functionp-test-q4Klqv ()
  (should (functionp 'py-backward-line)))

(ert-deftest py-ert-forward-line-functionp-test-6rtLn7 ()
  (should (functionp 'py-forward-line)))

(ert-deftest py-ert-backward-statement-functionp-test-Q1S0lJ ()
  (should (functionp 'py-backward-statement)))

(ert-deftest py-ert-backward-statement-bol-functionp-test-K2HFhl ()
  (should (functionp 'py-backward-statement-bol)))

(ert-deftest py-ert-end-of-statement-functionp-test-yeWNbX ()
  (should (functionp 'py-forward-statement)))

(ert-deftest py-ert-end-of-statement-bol-functionp-test-ysqL3y ()
  (should (functionp 'py-forward-statement-bol)))

(ert-deftest py-ert-backward-decorator-functionp-test-ob9uTa ()
  (should (functionp 'py-backward-decorator)))

(ert-deftest py-ert-end-of-decorator-functionp-test-qqBVGM ()
  (should (functionp 'py-forward-decorator)))

(ert-deftest py-ert-forward-line-functionp-test-4Idhro ()
  (should (functionp 'py-forward-line)))

(ert-deftest py-ert--go-to-keyword-functionp-test-iE10QB ()
  (should (functionp 'py--go-to-keyword)))

(ert-deftest py-ert-leave-comment-or-string-backward-functionp-test-GWv6vd ()
  (should (functionp 'py-leave-comment-or-string-backward)))

(ert-deftest py-ert-forward-into-nomenclature-functionp-test-8WApPq ()
  (should (functionp 'py-forward-into-nomenclature)))

(ert-deftest py-ert-backward-into-nomenclature-functionp-test-KWhit2 ()
  (should (functionp 'py-backward-into-nomenclature)))

(ert-deftest py-ert--travel-current-indent-functionp-test-CYDb4D ()
  (should (functionp 'py--travel-current-indent)))

(ert-deftest py-ert-backward-block-current-column-functionp-test-GUj4Ff ()
  (should (functionp 'py-backward-block-current-column)))

(ert-deftest py-ert--end-of-block-p-functionp-test-kJGLdR ()
  (should (functionp 'py--end-of-block-p)))

(ert-deftest py-ert--end-of-clause-p-functionp-test-eKXQJs ()
  (should (functionp 'py--end-of-clause-p)))

(ert-deftest py-ert--end-of-block-or-clause-p-functionp-test-sTHdh4 ()
  (should (functionp 'py--end-of-block-or-clause-p)))

(ert-deftest py-ert--end-of-def-p-functionp-test-qe0qVG ()
  (should (functionp 'py--end-of-def-p)))

(ert-deftest py-ert--end-of-class-p-functionp-test-2PYWxj ()
  (should (functionp 'py--end-of-class-p)))

(ert-deftest py-ert--end-of-def-or-class-p-functionp-test-SksUaW ()
  (should (functionp 'py--end-of-def-or-class-p)))

(ert-deftest py-ert--end-of-if-block-p-functionp-test-EVpfLy ()
  (should (functionp 'py--end-of-if-block-p)))

(ert-deftest py-ert--end-of-try-block-p-functionp-test-4O69jb ()
  (should (functionp 'py--end-of-try-block-p)))

(ert-deftest py-ert--end-of-minor-block-p-functionp-test-i8HzQN ()
  (should (functionp 'py--end-of-minor-block-p)))

(ert-deftest py-ert--end-of-for-block-p-functionp-test-sBTUkq ()
  (should (functionp 'py--end-of-for-block-p)))

(ert-deftest py-ert--end-of-top-level-p-functionp-test-ycCUM2 ()
  (should (functionp 'py--end-of-top-level-p)))

(ert-deftest py-ert--end-of-statement-p-functionp-test-4EZCcF ()
  (should (functionp 'py--end-of-statement-p)))

(ert-deftest py-ert--end-of-expression-p-functionp-test-WSt3zh ()
  (should (functionp 'py--end-of-expression-p)))

(ert-deftest py-ert--end-of-block-bol-p-functionp-test-Ok38UT ()
  (should (functionp 'py--end-of-block-bol-p)))

(ert-deftest py-ert--end-of-clause-bol-p-functionp-test-ABKWfw ()
  (should (functionp 'py--end-of-clause-bol-p)))

(ert-deftest py-ert--end-of-block-or-clause-bol-p-functionp-test-2Z6ox8 ()
  (should (functionp 'py--end-of-block-or-clause-bol-p)))

(ert-deftest py-ert--end-of-def-bol-p-functionp-test-w1myPK ()
  (should (functionp 'py--end-of-def-bol-p)))

(ert-deftest py-ert--end-of-class-bol-p-functionp-test-8mFy4m ()
  (should (functionp 'py--end-of-class-bol-p)))

(ert-deftest py-ert--end-of-def-or-class-bol-p-functionp-test-kdABkZ ()
  (should (functionp 'py--end-of-def-or-class-bol-p)))

(ert-deftest py-ert--end-of-if-block-bol-p-functionp-test-a8SixB ()
  (should (functionp 'py--end-of-if-block-bol-p)))

(ert-deftest py-ert--end-of-try-block-bol-p-functionp-test-cXvZKd ()
  (should (functionp 'py--end-of-try-block-bol-p)))

(ert-deftest py-ert--end-of-minor-block-bol-p-functionp-test-4EHuVP ()
  (should (functionp 'py--end-of-minor-block-bol-p)))

(ert-deftest py-ert--end-of-for-block-bol-p-functionp-test-yyf13r ()
  (should (functionp 'py--end-of-for-block-bol-p)))

(ert-deftest py-ert--fast-completion-get-completions-functionp-test-uAfhjG ()
  (should (functionp 'py--fast-completion-get-completions)))

(ert-deftest py-ert--fast--do-completion-at-point-functionp-test-yQ7QSl ()
  (should (functionp 'py--fast--do-completion-at-point)))

(ert-deftest py-ert--fast-complete-base-functionp-test-G0fNq1 ()
  (should (functionp 'py--fast-complete-base)))

(ert-deftest py-ert-fast-complete-functionp-test-qGevZG ()
  (should (functionp 'py-fast-complete)))

(ert-deftest py-ert-fast-process-functionp-test-GUgmwm ()
  (should (functionp 'py-fast-process)))

(ert-deftest py-ert--filter-result-functionp-test-gP5D01 ()
  (should (functionp 'py--filter-result)))

(ert-deftest py-ert-fast-send-string-functionp-test-MdJlrH ()
  (should (functionp 'py-fast-send-string)))

(ert-deftest py-ert-execute-region-fast-functionp-test-ANrTPm ()
  (should (functionp 'py-execute-region-fast)))

(ert-deftest py-ert-execute-statement-fast-functionp-test-sHN6b2 ()
  (should (functionp 'py-execute-statement-fast)))

(ert-deftest py-ert-execute-block-fast-functionp-test-uA88vH ()
  (should (functionp 'py-execute-block-fast)))

(ert-deftest py-ert-execute-block-or-clause-fast-functionp-test-wb2HNm ()
  (should (functionp 'py-execute-block-or-clause-fast)))

(ert-deftest py-ert-execute-def-fast-functionp-test-c9Ko41 ()
  (should (functionp 'py-execute-def-fast)))

(ert-deftest py-ert-execute-class-fast-functionp-test-Q19NkH ()
  (should (functionp 'py-execute-class-fast)))

(ert-deftest py-ert-execute-def-or-class-fast-functionp-test-w1Fvxm ()
  (should (functionp 'py-execute-def-or-class-fast)))

(ert-deftest py-ert-execute-expression-fast-functionp-test-q0PtL1 ()
  (should (functionp 'py-execute-expression-fast)))

(ert-deftest py-ert-execute-partial-expression-fast-functionp-test-cZVOWG ()
  (should (functionp 'py-execute-partial-expression-fast)))

(ert-deftest py-ert-execute-top-level-fast-functionp-test-kDfj9l ()
  (should (functionp 'py-execute-top-level-fast)))

(ert-deftest py-ert-execute-clause-fast-functionp-test-C2cHh1 ()
  (should (functionp 'py-execute-clause-fast)))

(ert-deftest py-ert-restore-window-configuration-functionp-test-eyA5qG ()
  (should (functionp 'py-restore-window-configuration)))

(ert-deftest py-ert-switch-to-python-functionp-test-ItywB0 ()
  (should (functionp 'py-switch-to-python)))

(ert-deftest py-ert-force-py-shell-name-p-on-functionp-test-yud7j3 ()
  (should (functionp 'force-py-shell-name-p-on)))

(ert-deftest py-ert-force-py-shell-name-p-off-functionp-test-wLjToJ ()
  (should (functionp 'force-py-shell-name-p-off)))

(ert-deftest py-ert-toggle-split-windows-on-execute-functionp-test-4Yc2pp ()
  (should (functionp 'py-toggle-split-windows-on-execute)))

(ert-deftest py-ert-split-windows-on-execute-on-functionp-test-0sUto5 ()
  (should (functionp 'py-split-windows-on-execute-on)))

(ert-deftest py-ert-split-windows-on-execute-off-functionp-test-gd12kL ()
  (should (functionp 'py-split-windows-on-execute-off)))

(ert-deftest py-ert-toggle-switch-buffers-on-execute-functionp-test-gjxnfr ()
  (should (functionp 'py-toggle-switch-buffers-on-execute)))

(ert-deftest py-ert-switch-buffers-on-execute-on-functionp-test-WO9P86 ()
  (should (functionp 'py-switch-buffers-on-execute-on)))

(ert-deftest py-ert-switch-buffers-on-execute-off-functionp-test-uA6R1M ()
  (should (functionp 'py-switch-buffers-on-execute-off)))

(ert-deftest py-ert-guess-default-python-functionp-test-kpWDRs ()
  (should (functionp 'py-guess-default-python)))

(ert-deftest py-ert-dirstack-hook-functionp-test-GANaI8 ()
  (should (functionp 'py-dirstack-hook)))

(ert-deftest py-ert-set-ipython-completion-command-string-functionp-test-EhfdvO ()
  (should (functionp 'py-set-ipython-completion-command-string)))

(ert-deftest py-ert-ipython--module-completion-import-functionp-test-0eLOju ()
  (should (functionp 'py-ipython--module-completion-import)))

(ert-deftest py-ert--compose-buffer-name-initials-functionp-test-Ed7S49 ()
  (should (functionp 'py--compose-buffer-name-initials)))

(ert-deftest py-ert--remove-home-directory-from-list-functionp-test-8eh9NP ()
  (should (functionp 'py--remove-home-directory-from-list)))

(ert-deftest py-ert--choose-buffer-name-functionp-test-K6fFyv ()
  (should (functionp 'py--choose-buffer-name)))

(ert-deftest py-ert--jump-to-exception-intern-functionp-test-mwykfb ()
  (should (functionp 'py--jump-to-exception-intern)))

(ert-deftest py-ert--jump-to-exception-functionp-test-mgLQTQ ()
  (should (functionp 'py--jump-to-exception)))

(ert-deftest py-ert-toggle-split-window-function-functionp-test-sdTVyw ()
  (should (functionp 'py-toggle-split-window-function)))

(ert-deftest py-ert--alternative-split-windows-on-execute-function-functionp-test-cBDqBQ ()
  (should (functionp 'py--alternative-split-windows-on-execute-function)))

(ert-deftest py-ert--get-splittable-window-functionp-test-2HeUXu ()
  (should (functionp 'py--get-splittable-window)))

(ert-deftest py-ert--manage-windows-split-functionp-test-qo7fi9 ()
  (should (functionp 'py--manage-windows-split)))

(ert-deftest py-ert--shell-manage-windows-functionp-test-W6QfBN ()
  (should (functionp 'py--shell-manage-windows)))

(ert-deftest py-ert-kill-shell-unconditional-functionp-test-mUQTRr ()
  (should (functionp 'py-kill-shell-unconditional)))

(ert-deftest py-ert-kill-default-shell-unconditional-functionp-test-eCMg65 ()
  (should (functionp 'py-kill-default-shell-unconditional)))

(ert-deftest py-ert--report-executable-functionp-test-cd9riK ()
  (should (functionp 'py--report-executable)))

(ert-deftest py-ert--guess-buffer-name-functionp-test-6PTsro ()
  (should (functionp 'py--guess-buffer-name)))

(ert-deftest py-ert--configured-shell-functionp-test-Wyo7B2 ()
  (should (functionp 'py--configured-shell)))

(ert-deftest py-ert-shell-functionp-test-OQlBTk ()
  (should (functionp 'py-shell)))

(ert-deftest py-ert-get-process-functionp-test-0OqP1Y ()
  (should (functionp 'py--get-process)))

(ert-deftest py-ert-switch-to-shell-functionp-test-I9gy6C ()
  (should (functionp 'py-switch-to-shell)))

(ert-deftest py-ert-execute-file-command-functionp-test-q8Pvch ()
  (should (functionp 'py-execute-file-command)))

(ert-deftest py-ert--store-result-functionp-test-CUmPeV ()
  (should (functionp 'py--store-result)))

(ert-deftest py-ert--close-execution-functionp-test-8sPofz ()
  (should (functionp 'py--close-execution)))

(ert-deftest py-ert--execute-base-functionp-test-8o8ghd ()
  (should (functionp 'py--execute-base)))

(ert-deftest py-ert--send-to-fast-process-functionp-test-SsvZeR ()
  (should (functionp 'py--send-to-fast-process)))

(ert-deftest py-ert--execute-base-intern-functionp-test-OsfYdv ()
  (should (functionp 'py--execute-base-intern)))

(ert-deftest py-ert--execute-buffer-finally-functionp-test-WWFma9 ()
  (should (functionp 'py--execute-buffer-finally)))

(ert-deftest py-ert--fetch-error-functionp-test-8km74M ()
  (should (functionp 'py--fetch-error)))

(ert-deftest py-ert--fetch-result-functionp-test-SCyeWq ()
  (should (functionp 'py--fetch-result)))

(ert-deftest py-ert--execute-ge24.3-functionp-test-qsfpW5 ()
  (should (functionp 'py--execute-ge24.3)))

(ert-deftest py-ert-delete-temporary-functionp-test-qaP1TK ()
  (should (functionp 'py-delete-temporary)))

(ert-deftest py-ert-execute-python-mode-v5-functionp-test-S8RoSp ()
  (should (functionp 'py-execute-python-mode-v5)))

(ert-deftest py-ert--insert-offset-lines-functionp-test-mSKiO4 ()
  (should (functionp 'py--insert-offset-lines)))

(ert-deftest py-ert--execute-file-base-functionp-test-6NxAIJ ()
  (should (functionp 'py--execute-file-base)))

(ert-deftest py-ert-execute-file-functionp-test-WCLGAo ()
  (should (functionp 'py-execute-file)))

(ert-deftest py-ert-current-working-directory-functionp-test-OGibq3 ()
  (should (functionp 'py-current-working-directory)))

(ert-deftest py-ert--update-execute-directory-intern-functionp-test-QLRLdI ()
  (should (functionp 'py--update-execute-directory-intern)))

(ert-deftest py-ert--update-execute-directory-functionp-test-IlF8Ym ()
  (should (functionp 'py--update-execute-directory)))

(ert-deftest py-ert-execute-string-functionp-test-YzBQG1 ()
  (should (functionp 'py-execute-string)))

(ert-deftest py-ert-execute-string-dedicated-functionp-test-u8D2nG ()
  (should (functionp 'py-execute-string-dedicated)))

(ert-deftest py-ert--insert-execute-directory-functionp-test-aAMu6k ()
  (should (functionp 'py--insert-execute-directory)))

(ert-deftest py-ert--fix-if-name-main-permission-functionp-test-UbjfLZ ()
  (should (functionp 'py--fix-if-name-main-permission)))

(ert-deftest py-ert--fix-start-functionp-test-0u6prE ()
  (should (functionp 'py--fix-start)))

(ert-deftest py-ert-execute-import-or-reload-functionp-test-y6CSEX ()
  (should (functionp 'py-execute-import-or-reload)))

(ert-deftest py-ert--qualified-module-name-functionp-test-8al8gC ()
  (should (functionp 'py--qualified-module-name)))

(ert-deftest py-ert-execute-buffer-functionp-test-uUR5Og ()
  (should (functionp 'py-execute-buffer)))

(ert-deftest py-ert-execute-buffer-dedicated-functionp-test-m4vmoV ()
  (should (functionp 'py-execute-buffer-dedicated)))

(ert-deftest py-ert-execute-region-python-functionp-test-g59lNA ()
  (should (functionp 'py-execute-region-python)))

(ert-deftest py-ert-execute-region-python2-functionp-test-8c7pvI ()
  (should (functionp 'py-execute-region-python2)))

(ert-deftest py-ert-execute-region-python3-functionp-test-m8bqUP ()
  (should (functionp 'py-execute-region-python3)))

(ert-deftest py-ert-execute-region-ipython-functionp-test-0GyaYW ()
  (should (functionp 'py-execute-region-ipython)))

(ert-deftest py-ert-execute-region-jython-functionp-test-eSdxV3 ()
  (should (functionp 'py-execute-region-jython)))

(ert-deftest py-ert-process-file-functionp-test-kv2RaS ()
  (should (functionp 'py-process-file)))

(ert-deftest py-ert-execute-line-functionp-test-mCIXEz ()
  (should (functionp 'py-execute-line)))

(ert-deftest py-ert-remove-overlays-at-point-functionp-test-6n4P9g ()
  (should (functionp 'py-remove-overlays-at-point)))

(ert-deftest py-ert--find-next-exception-functionp-test-6ByApn ()
  (should (functionp 'py--find-next-exception)))

(ert-deftest py-ert-down-exception-functionp-test-qC0eV5 ()
  (should (functionp 'py-down-exception)))

(ert-deftest py-ert-up-exception-functionp-test-GC3HrO ()
  (should (functionp 'py-up-exception)))

(ert-deftest py-ert--postprocess-intern-functionp-test-Se7GVw ()
  (should (functionp 'py--postprocess-intern)))

(ert-deftest py-ert--find-next-exception-prepare-functionp-test-46iCnf ()
  (should (functionp 'py--find-next-exception-prepare)))

(ert-deftest py-ert-python-functionp-test-eajcNX ()
  (should (functionp 'python)))

(ert-deftest py-ert-ipython-functionp-test-IV8taG ()
  (should (functionp 'ipython)))

(ert-deftest py-ert-python2-functionp-test-odIsvo ()
  (should (functionp 'python2)))

(ert-deftest py-ert-jython-functionp-test-gxxgO6 ()
  (should (functionp 'jython)))

(ert-deftest py-ert-python3-functionp-test-WKKq4O ()
  (should (functionp 'python3)))

(ert-deftest py-ert-hide-base-functionp-test-CADylx ()
  (should (functionp 'py-hide-base)))

(ert-deftest py-ert-hide-show-functionp-test-wHlaCf ()
  (should (functionp 'py-hide-show)))

(ert-deftest py-ert-hide-region-functionp-test-a0daPX ()
  (should (functionp 'py-hide-region)))

(ert-deftest py-ert-hide-statement-functionp-test-8qzr3F ()
  (should (functionp 'py-hide-statement)))

(ert-deftest py-ert-hide-block-functionp-test-Mt3eeo ()
  (should (functionp 'py-hide-block)))

(ert-deftest py-ert-hide-block-or-clause-functionp-test-CWnaq6 ()
  (should (functionp 'py-hide-block-or-clause)))

(ert-deftest py-ert-hide-def-functionp-test-ENfGyO ()
  (should (functionp 'py-hide-def)))

(ert-deftest py-ert-show-all-functionp-test-MpCaIw ()
  (should (functionp 'py-show-all)))

(ert-deftest py-ert-show-functionp-test-mQLyOe ()
  (should (functionp 'py-show)))

(ert-deftest py-ert-hide-expression-functionp-test-CyPYSW ()
  (should (functionp 'py-hide-expression)))

(ert-deftest py-ert-hide-partial-expression-functionp-test-2FvNYE ()
  (should (functionp 'py-hide-partial-expression)))

(ert-deftest py-ert-hide-line-functionp-test-qieL0m ()
  (should (functionp 'py-hide-line)))

(ert-deftest py-ert-hide-top-level-functionp-test-4yhH04 ()
  (should (functionp 'py-hide-top-level)))

(ert-deftest py-ert-copy-statement-functionp-test-wBM90M ()
  (should (functionp 'py-copy-statement)))

(ert-deftest py-ert-copy-statement-bol-functionp-test-ugrj0u ()
  (should (functionp 'py-copy-statement-bol)))

(ert-deftest py-ert-copy-top-level-functionp-test-mi9RKb ()
  (should (functionp 'py-copy-top-level)))

(ert-deftest py-ert-copy-top-level-bol-functionp-test-U7hBtS ()
  (should (functionp 'py-copy-top-level-bol)))

(ert-deftest py-ert-copy-block-functionp-test-YVRM9y ()
  (should (functionp 'py-copy-block)))

(ert-deftest py-ert-copy-block-bol-functionp-test-e8ePQf ()
  (should (functionp 'py-copy-block-bol)))

(ert-deftest py-ert-copy-clause-functionp-test-s1WEvW ()
  (should (functionp 'py-copy-clause)))

(ert-deftest py-ert-copy-clause-bol-functionp-test-ukzZ7C ()
  (should (functionp 'py-copy-clause-bol)))

(ert-deftest py-ert-copy-block-or-clause-functionp-test-mCTXHj ()
  (should (functionp 'py-copy-block-or-clause)))

(ert-deftest py-ert-copy-block-or-clause-bol-functionp-test-8u0If0 ()
  (should (functionp 'py-copy-block-or-clause-bol)))

(ert-deftest py-ert-copy-def-functionp-test-kJf4KG ()
  (should (functionp 'py-copy-def)))

(ert-deftest py-ert-copy-def-bol-functionp-test-cHP1dn ()
  (should (functionp 'py-copy-def-bol)))

(ert-deftest py-ert-copy-class-functionp-test-cXEPH3 ()
  (should (functionp 'py-copy-class)))

(ert-deftest py-ert-copy-class-bol-functionp-test-0mgcbK ()
  (should (functionp 'py-copy-class-bol)))

(ert-deftest py-ert-copy-def-or-class-functionp-test-WAnhBq ()
  (should (functionp 'py-copy-def-or-class)))

(ert-deftest py-ert-copy-def-or-class-bol-functionp-test-Oiz216 ()
  (should (functionp 'py-copy-def-or-class-bol)))

(ert-deftest py-ert-copy-expression-functionp-test-OiKnpN ()
  (should (functionp 'py-copy-expression)))

(ert-deftest py-ert-copy-expression-bol-functionp-test-U5sYNt ()
  (should (functionp 'py-copy-expression-bol)))

(ert-deftest py-ert-copy-partial-expression-functionp-test-EZCu99 ()
  (should (functionp 'py-copy-partial-expression)))

(ert-deftest py-ert-copy-partial-expression-bol-functionp-test-6dY2sQ ()
  (should (functionp 'py-copy-partial-expression-bol)))

(ert-deftest py-ert-copy-minor-block-functionp-test-IRVZNw ()
  (should (functionp 'py-copy-minor-block)))

(ert-deftest py-ert-copy-minor-block-bol-functionp-test-EtRA4c ()
  (should (functionp 'py-copy-minor-block-bol)))

(ert-deftest py-ert-statement-functionp-test-wjtFjT ()
  (should (functionp 'py-statement)))

(ert-deftest py-ert-top-level-functionp-test-eYNgzz ()
  (should (functionp 'py-top-level)))

(ert-deftest py-ert-block-functionp-test-OeKcNf ()
  (should (functionp 'py-block)))

(ert-deftest py-ert-clause-functionp-test-4yDH7W ()
  (should (functionp 'py-clause)))

(ert-deftest py-ert-block-or-clause-functionp-test-g7cGqE ()
  (should (functionp 'py-block-or-clause)))

(ert-deftest py-ert-def-functionp-test-oNjjHl ()
  (should (functionp 'py-def)))

(ert-deftest py-ert-class-functionp-test-2lYPY2 ()
  (should (functionp 'py-class)))

(ert-deftest py-ert-def-or-class-functionp-test-8MvOdK ()
  (should (functionp 'py-def-or-class)))

(ert-deftest py-ert-expression-functionp-test-i8i9qr ()
  (should (functionp 'py-expression)))

(ert-deftest py-ert-partial-expression-functionp-test-25EmC8 ()
  (should (functionp 'py-partial-expression)))

(ert-deftest py-ert-minor-block-functionp-test-QxDmIP ()
  (should (functionp 'py-minor-block)))

(ert-deftest py-ert-copy-statement-functionp-test-ik7wzB ()
  (should (functionp 'py-copy-statement)))

(ert-deftest py-ert-copy-top-level-functionp-test-wXsdti ()
  (should (functionp 'py-copy-top-level)))

(ert-deftest py-ert-copy-block-functionp-test-25nunZ ()
  (should (functionp 'py-copy-block)))

(ert-deftest py-ert-copy-clause-functionp-test-cPdyeG ()
  (should (functionp 'py-copy-clause)))

(ert-deftest py-ert-copy-block-or-clause-functionp-test-wvfq7m ()
  (should (functionp 'py-copy-block-or-clause)))

(ert-deftest py-ert-copy-def-functionp-test-iY0UW3 ()
  (should (functionp 'py-copy-def)))

(ert-deftest py-ert-copy-class-functionp-test-OA9MMK ()
  (should (functionp 'py-copy-class)))

(ert-deftest py-ert-copy-def-or-class-functionp-test-Qz39yr ()
  (should (functionp 'py-copy-def-or-class)))

(ert-deftest py-ert-copy-expression-functionp-test-637Vm8 ()
  (should (functionp 'py-copy-expression)))

(ert-deftest py-ert-copy-partial-expression-functionp-test-yGLJES ()
  (should (functionp 'py-copy-partial-expression)))

(ert-deftest py-ert-copy-minor-block-functionp-test-QHuuTC ()
  (should (functionp 'py-copy-minor-block)))

(ert-deftest py-ert-delete-statement-functionp-test-esFW8m ()
  (should (functionp 'py-delete-statement)))

(ert-deftest py-ert-delete-top-level-functionp-test-0wKfm7 ()
  (should (functionp 'py-delete-top-level)))

(ert-deftest py-ert-delete-block-functionp-test-aeVCvR ()
  (should (functionp 'py-delete-block)))

(ert-deftest py-ert-delete-clause-functionp-test-mO7CDB ()
  (should (functionp 'py-delete-clause)))

(ert-deftest py-ert-delete-block-or-clause-functionp-test-Y5ZeMl ()
  (should (functionp 'py-delete-block-or-clause)))

(ert-deftest py-ert-delete-def-functionp-test-MRygS5 ()
  (should (functionp 'py-delete-def)))

(ert-deftest py-ert-delete-class-functionp-test-4GtQWP ()
  (should (functionp 'py-delete-class)))

(ert-deftest py-ert-delete-def-or-class-functionp-test-8S1UYz ()
  (should (functionp 'py-delete-def-or-class)))

(ert-deftest py-ert-delete-expression-functionp-test-emWEYj ()
  (should (functionp 'py-delete-expression)))

(ert-deftest py-ert-delete-partial-expression-functionp-test-aEjFU3 ()
  (should (functionp 'py-delete-partial-expression)))

(ert-deftest py-ert-delete-minor-block-functionp-test-cjBlON ()
  (should (functionp 'py-delete-minor-block)))

(ert-deftest py-ert-fill-labelled-string-functionp-test-ysNVft ()
  (should (functionp 'py-fill-labelled-string)))

(ert-deftest py-ert--in-or-behind-or-before-a-docstring-functionp-test-I15y0d ()
  (should (functionp 'py--in-or-behind-or-before-a-docstring)))

(ert-deftest py-ert--string-fence-delete-spaces-functionp-test-GQjALY ()
  (should (functionp 'py--string-fence-delete-spaces)))

;; (ert-deftest py-ert--fill-fix-end-functionp-test-M5IhuJ ()
;;   (should (functionp 'py--fill-fix-end)))

(ert-deftest py-ert-insert-default-shebang-functionp-test-kvspuZ ()
  (should (functionp 'py-insert-default-shebang)))

(ert-deftest py-ert--top-level-form-p-functionp-test-qmv1WJ ()
  (should (functionp 'py--top-level-form-p)))

(ert-deftest py-ert-indent-line-outmost-functionp-test-8kO6mu ()
  (should (functionp 'py-indent-line-outmost)))

(ert-deftest py-ert--indent-fix-region-intern-functionp-test-kHXuOe ()
  (should (functionp 'py--indent-fix-region-intern)))

(ert-deftest py-ert--indent-line-intern-functionp-test-qsB1bZ ()
  (should (functionp 'py--indent-line-intern)))

(ert-deftest py-compute-comment-indentation-functionp-test-27PIAJ ()
  (should (functionp 'py-compute-comment-indentation)))

(ert-deftest py-ert--calculate-indent-backwards-functionp-test-0YyrWt ()
  (should (functionp 'py--calculate-indent-backwards)))

(ert-deftest py-ert-indent-line-functionp-test-COMfie ()
  (should (functionp 'py-indent-line)))

(ert-deftest py-ert--delete-trailing-whitespace-functionp-test-G8ytCY ()
  (should (functionp 'py--delete-trailing-whitespace)))

(ert-deftest py-ert-newline-and-indent-functionp-test-wJrUIH ()
  (should (functionp 'py-newline-and-indent)))

(ert-deftest py-ert-newline-and-dedent-functionp-test-Q5uGPq ()
  (should (functionp 'py-newline-and-dedent)))

(ert-deftest py-ert-toggle-indent-tabs-mode-functionp-test-mmCbT9 ()
  (should (functionp 'py-toggle-indent-tabs-mode)))

(ert-deftest py-ert-indent-tabs-mode-functionp-test-UXbUUS ()
  (should (functionp 'py-indent-tabs-mode)))

(ert-deftest py-ert-indent-tabs-mode-on-functionp-test-0sxfXB ()
  (should (functionp 'py-indent-tabs-mode-on)))

(ert-deftest py-ert-indent-tabs-mode-off-functionp-test-QNLdXk ()
  (should (functionp 'py-indent-tabs-mode-off)))

(ert-deftest py-ert-guessed-sanity-check-functionp-test-YTHIV3 ()
  (should (functionp 'py-guessed-sanity-check)))

(ert-deftest py-ert--guess-indent-final-functionp-test-m8bNNM ()
  (should (functionp 'py--guess-indent-final)))

(ert-deftest py-ert--guess-indent-forward-functionp-test-wLmDDv ()
  (should (functionp 'py--guess-indent-forward)))

(ert-deftest py-ert--guess-indent-backward-functionp-test-eYOlre ()
  (should (functionp 'py--guess-indent-backward)))

(ert-deftest py-ert-guess-indent-offset-functionp-test-eQPLcX ()
  (should (functionp 'py-guess-indent-offset)))

(ert-deftest py-ert-backward-paragraph-functionp-test-uaQLCo ()
  (should (functionp 'py-backward-paragraph)))

(ert-deftest py-ert-end-of-paragraph-functionp-test-K6idh7 ()
  (should (functionp 'py-forward-paragraph)))

(ert-deftest py-ert-indent-and-forward-functionp-test-sPLxWP ()
  (should (functionp 'py-indent-and-forward)))

(ert-deftest py-ert-indent-region-functionp-test-m2Uhzy ()
  (should (functionp 'py-indent-region)))

(ert-deftest py-ert-backward-declarations-functionp-test-cH5qch ()
  (should (functionp 'py-backward-declarations)))

(ert-deftest py-ert-end-of-declarations-functionp-test-0UohMZ ()
  (should (functionp 'py-forward-declarations)))

(ert-deftest py-ert-declarations-functionp-test-oDkanI ()
  (should (functionp 'py-declarations)))

(ert-deftest py-ert-kill-declarations-functionp-test-S6uLUq ()
  (should (functionp 'py-kill-declarations)))

(ert-deftest py-ert-backward-statements-functionp-test-C40dt9 ()
  (should (functionp 'py-backward-statements)))

(ert-deftest py-ert-end-of-statements-functionp-test-iCSn9S ()
  (should (functionp 'py-forward-statements)))

(ert-deftest py-ert-statements-functionp-test-ITJBQC ()
  (should (functionp 'py-statements)))

(ert-deftest py-ert-kill-statements-functionp-test-IjGQvm ()
  (should (functionp 'py-kill-statements)))

(ert-deftest py-ert--join-words-wrapping-functionp-test-iEmb75 ()
  (should (functionp 'py--join-words-wrapping)))

(ert-deftest py-ert-insert-super-functionp-test-gd4NJP ()
  (should (functionp 'py-insert-super)))

(ert-deftest py-ert-comment-region-functionp-test-icyVkz ()
  (should (functionp 'py-comment-region)))

(ert-deftest py-ert-delete-comments-in-def-or-class-functionp-test-usc5Ri ()
  (should (functionp 'py-delete-comments-in-def-or-class)))

(ert-deftest py-ert-delete-comments-in-class-functionp-test-2rUFm2 ()
  (should (functionp 'py-delete-comments-in-class)))

(ert-deftest py-ert-delete-comments-in-block-functionp-test-wbPSPL ()
  (should (functionp 'py-delete-comments-in-block)))

(ert-deftest py-ert-delete-comments-in-region-functionp-test-oH4wgv ()
  (should (functionp 'py-delete-comments-in-region)))

(ert-deftest py-ert--delete-comments-intern-functionp-test-UfVOHe ()
  (should (functionp 'py--delete-comments-intern)))

(ert-deftest py-ert-update-gud-pdb-history-functionp-test-0qkZ6X ()
  (should (functionp 'py-update-gud-pdb-history)))

(ert-deftest py-ert--pdbtrack-overlay-arrow-functionp-test-4KuxtH ()
  (should (functionp 'py--pdbtrack-overlay-arrow)))

(ert-deftest py-ert--pdbtrack-track-stack-file-functionp-test-eszLNq ()
  (should (functionp 'py--pdbtrack-track-stack-file)))

(ert-deftest py-ert--pdbtrack-map-filename-functionp-test-YNf959 ()
  (should (functionp 'py--pdbtrack-map-filename)))

(ert-deftest py-ert--pdbtrack-get-source-buffer-functionp-test-e0aTkT ()
  (should (functionp 'py--pdbtrack-get-source-buffer)))

(ert-deftest py-ert--pdbtrack-grub-for-buffer-functionp-test-mIOoBC ()
  (should (functionp 'py--pdbtrack-grub-for-buffer)))

(ert-deftest py-ert-pdbtrack-toggle-stack-tracking-functionp-test-QzTEOl ()
  (should (functionp 'py-pdbtrack-toggle-stack-tracking)))

(ert-deftest py-ert-turn-on-pdbtrack-functionp-test-SoQ524 ()
  (should (functionp 'turn-on-pdbtrack)))

(ert-deftest py-ert-turn-off-pdbtrack-functionp-test-gRH8dO ()
  (should (functionp 'turn-off-pdbtrack)))

(ert-deftest py-ert-execute-statement-pdb-functionp-test-WQb3px ()
  (should (functionp 'py-execute-statement-pdb)))

(ert-deftest py-ert-execute-region-pdb-functionp-test-GsuQxg ()
  (should (functionp 'py-execute-region-pdb)))

(ert-deftest py-ert-pdb-execute-statement-functionp-test-cR3HGZ ()
  (should (functionp 'py-pdb-execute-statement)))

(ert-deftest py-ert-pdb-help-functionp-test-ALfsiM ()
  (should (functionp 'py-pdb-help)))

;; (ert-deftest py-ert-pdb-break-functionp-test ()
;;   (should (functionp 'py-pdb-break)))

(ert-deftest py-ert-end-of-block-functionp-test-8IwzSy ()
  (should (functionp 'py-forward-block)))

(ert-deftest py-ert-end-of-block-bol-functionp-test-qkEVsl ()
  (should (functionp 'py-forward-block-bol)))

(ert-deftest py-ert-end-of-clause-functionp-test-wVit17 ()
  (should (functionp 'py-forward-clause)))

(ert-deftest py-ert-end-of-clause-bol-functionp-test-OQo3vU ()
  (should (functionp 'py-forward-clause-bol)))

(ert-deftest py-ert-end-of-block-or-clause-functionp-test-21doZG ()
  (should (functionp 'py-forward-block-or-clause)))

(ert-deftest py-ert-end-of-block-or-clause-bol-functionp-test-ElVbtt ()
  (should (functionp 'py-forward-block-or-clause-bol)))

(ert-deftest py-ert-end-of-def-functionp-test-CoGqUf ()
  (should (functionp 'py-forward-def)))

(ert-deftest py-ert-end-of-def-bol-functionp-test-MtHuj2 ()
  (should (functionp 'py-forward-def-bol)))

(ert-deftest py-ert-end-of-class-functionp-test-aeS4GO ()
  (should (functionp 'py-forward-class)))

(ert-deftest py-ert-end-of-class-bol-functionp-test-u6zj2A ()
  (should (functionp 'py-forward-class-bol)))

(ert-deftest py-ert-end-of-def-or-class-functionp-test-Mb9tln ()
  (should (functionp 'py-forward-def-or-class)))

(ert-deftest py-ert-end-of-def-or-class-bol-functionp-test-0SIaC9 ()
  (should (functionp 'py-forward-def-or-class-bol)))

(ert-deftest py-ert-end-of-if-block-functionp-test-cH0XPV ()
  (should (functionp 'py-forward-if-block)))

(ert-deftest py-ert-end-of-if-block-bol-functionp-test-6fR60H ()
  (should (functionp 'py-forward-if-block-bol)))

(ert-deftest py-ert-end-of-try-block-functionp-test-2tEddu ()
  (should (functionp 'py-forward-try-block)))

(ert-deftest py-ert-end-of-try-block-bol-functionp-test-Ip8Qlg ()
  (should (functionp 'py-forward-try-block-bol)))

(ert-deftest py-ert-end-of-minor-block-functionp-test-IhN7t2 ()
  (should (functionp 'py-forward-minor-block)))

(ert-deftest py-ert-end-of-minor-block-bol-functionp-test-eywqDO ()
  (should (functionp 'py-forward-minor-block-bol)))

(ert-deftest py-ert-end-of-for-block-functionp-test-OwdrJA ()
  (should (functionp 'py-forward-for-block)))

(ert-deftest py-ert-end-of-for-block-bol-functionp-test-OU8zNm ()
  (should (functionp 'py-forward-for-block-bol)))

(ert-deftest py-ert-end-of-except-block-functionp-test-U3yHS8 ()
  (should (functionp 'py-forward-except-block)))

(ert-deftest py-ert-end-of-except-block-bol-functionp-test-CM3k5V ()
  (should (functionp 'py-forward-except-block-bol)))

(ert-deftest py-ert-execute-statement-functionp-test-2DHCiJ ()
  (should (functionp 'py-execute-statement)))

(ert-deftest py-ert-execute-block-functionp-test-kznPtw ()
  (should (functionp 'py-execute-block)))

(ert-deftest py-ert-execute-block-or-clause-functionp-test-6LufBj ()
  (should (functionp 'py-execute-block-or-clause)))

(ert-deftest py-ert-execute-def-functionp-test-QdwSG6 ()
  (should (functionp 'py-execute-def)))

(ert-deftest py-ert-execute-class-functionp-test-8yj8MT ()
  (should (functionp 'py-execute-class)))

(ert-deftest py-ert-execute-def-or-class-functionp-test-W0JWRG ()
  (should (functionp 'py-execute-def-or-class)))

(ert-deftest py-ert-execute-expression-functionp-test-gLPHSt ()
  (should (functionp 'py-execute-expression)))

(ert-deftest py-ert-execute-partial-expression-functionp-test-iguOQg ()
  (should (functionp 'py-execute-partial-expression)))

(ert-deftest py-ert-execute-top-level-functionp-test-qKIyN3 ()
  (should (functionp 'py-execute-top-level)))

(ert-deftest py-ert-execute-clause-functionp-test-aUMbIQ ()
  (should (functionp 'py-execute-clause)))

(ert-deftest py-ert-toggle-smart-indentation-functionp-test-AjIzAD ()
  (should (functionp 'py-toggle-smart-indentation)))

(ert-deftest py-ert-smart-indentation-on-functionp-test-CSWHqq ()
  (should (functionp 'py-smart-indentation-on)))

(ert-deftest py-ert-smart-indentation-off-functionp-test-gzrjed ()
  (should (functionp 'py-smart-indentation-off)))

(ert-deftest py-ert-toggle-sexp-function-functionp-test-ymnQYZ ()
  (should (functionp 'py-toggle-sexp-function)))

;; (ert-deftest py-ert-toggle-autopair-mode-functionp-test-i4umLM ()
;;   (should (functionp 'py-toggle-autopair-mode)))

;; (ert-deftest py-ert-autopair-mode-on-functionp-test-GOXfuz ()
;;   (should (functionp 'py-autopair-mode-on)))

;; (ert-deftest py-ert-autopair-mode-off-functionp-test-cpfpem ()
;;   (should (functionp 'py-autopair-mode-off)))

(ert-deftest py-ert-switch-buffers-on-execute-p-on-functionp-test-2hUpEV ()
  (should (functionp 'py-switch-buffers-on-execute-p-on)))

(ert-deftest py-ert-switch-buffers-on-execute-p-off-functionp-test-ikuTiI ()
  (should (functionp 'py-switch-buffers-on-execute-p-off)))

(ert-deftest py-ert-split-window-on-execute-on-functionp-test-MbK0pg ()
  (should (functionp 'py-split-window-on-execute-on)))

(ert-deftest py-ert-split-window-on-execute-off-functionp-test-Co4jS1 ()
  (should (functionp 'py-split-window-on-execute-off)))

(ert-deftest py-ert-fontify-shell-buffer-p-on-functionp-test-O8aUEy ()
  (should (functionp 'py-fontify-shell-buffer-p-on)))

(ert-deftest py-ert-fontify-shell-buffer-p-off-functionp-test-yijvZj ()
  (should (functionp 'py-fontify-shell-buffer-p-off)))

(ert-deftest py-ert-jump-on-exception-on-functionp-test-iqoFl8 ()
  (should (functionp 'py-jump-on-exception-on)))

(ert-deftest py-ert-jump-on-exception-off-functionp-test-C4mwuT ()
  (should (functionp 'py-jump-on-exception-off)))

(ert-deftest py-ert-use-current-dir-when-execute-p-on-functionp-test-OA44Gp ()
  (should (functionp 'py-use-current-dir-when-execute-p-on)))

(ert-deftest py-ert-use-current-dir-when-execute-p-off-functionp-test-OSeJMa ()
  (should (functionp 'py-use-current-dir-when-execute-p-off)))

(ert-deftest py-ert-electric-comment-p-on-functionp-test-wLrwRG ()
  (should (functionp 'py-electric-comment-p-on)))

(ert-deftest py-ert-electric-comment-p-off-functionp-test-4EG3Qr ()
  (should (functionp 'py-electric-comment-p-off)))

(ert-deftest py-ert-underscore-word-syntax-p-on-functionp-test-6N2HPX ()
  (should (functionp 'py-underscore-word-syntax-p-on)))

(ert-deftest py-ert-underscore-word-syntax-p-off-functionp-test-olSdLI ()
  (should (functionp 'py-underscore-word-syntax-p-off)))

(ert-deftest py-ert-backward-block-functionp-test-m4IcIt ()
  (should (functionp 'py-backward-block)))

(ert-deftest py-ert-backward-clause-functionp-test-KYsoBe ()
  (should (functionp 'py-backward-clause)))

(ert-deftest py-ert-backward-block-or-clause-functionp-test-m0a9C0 ()
  (should (functionp 'py-backward-block-or-clause)))

(ert-deftest py-ert-backward-def-functionp-test-i4cmFM ()
  (should (functionp 'py-backward-def)))

(ert-deftest py-ert-backward-class-functionp-test-wnqXFy ()
  (should (functionp 'py-backward-class)))

(ert-deftest py-ert-backward-def-or-class-functionp-test-8kMTCk ()
  (should (functionp 'py-backward-def-or-class)))

(ert-deftest py-ert-backward-if-block-functionp-test-67k6x6 ()
  (should (functionp 'py-backward-if-block)))

(ert-deftest py-ert-backward-try-block-functionp-test-mypJqS ()
  (should (functionp 'py-backward-try-block)))

(ert-deftest py-ert-backward-minor-block-functionp-test-wzg9gE ()
  (should (functionp 'py-backward-minor-block)))

(ert-deftest py-ert-backward-for-block-functionp-test-6Njj8p ()
  (should (functionp 'py-backward-for-block)))

(ert-deftest py-ert-backward-except-block-functionp-test-WabjXb ()
  (should (functionp 'py-backward-except-block)))

(ert-deftest py-ert-backward-block-bol-functionp-test-C2g5JX ()
  (should (functionp 'py-backward-block-bol)))

(ert-deftest py-ert-backward-clause-bol-functionp-test-wtnyuJ ()
  (should (functionp 'py-backward-clause-bol)))

(ert-deftest py-ert-backward-block-or-clause-bol-functionp-test-OQNz9u ()
  (should (functionp 'py-backward-block-or-clause-bol)))

(ert-deftest py-ert-backward-def-bol-functionp-test-0I7OPg ()
  (should (functionp 'py-backward-def-bol)))

(ert-deftest py-ert-backward-class-bol-functionp-test-U7UKv2 ()
  (should (functionp 'py-backward-class-bol)))

(ert-deftest py-ert-backward-def-or-class-bol-functionp-test-8yMS7N ()
  (should (functionp 'py-backward-def-or-class-bol)))

(ert-deftest py-ert-backward-if-block-bol-functionp-test-IFR9Kz ()
  (should (functionp 'py-backward-if-block-bol)))

(ert-deftest py-ert-backward-try-block-bol-functionp-test-ozeqll ()
  (should (functionp 'py-backward-try-block-bol)))

(ert-deftest py-ert-backward-minor-block-bol-functionp-test-00LUW6 ()
  (should (functionp 'py-backward-minor-block-bol)))

(ert-deftest py-ert-backward-for-block-bol-functionp-test-sNiOuS ()
  (should (functionp 'py-backward-for-block-bol)))

(ert-deftest py-ert-backward-except-block-bol-functionp-test-I3PK0D ()
  (should (functionp 'py-backward-except-block-bol)))

(ert-deftest py-ert-toggle-comment-auto-fill-functionp-test-4Cr5xp ()
  (should (functionp 'py-toggle-comment-auto-fill)))

(ert-deftest py-ert-comment-auto-fill-on-functionp-test-uQUz1a ()
  (should (functionp 'py-comment-auto-fill-on)))

(ert-deftest py-ert-comment-auto-fill-off-functionp-test-UVllZZ ()
  (should (functionp 'py-comment-auto-fill-off)))

(ert-deftest py-ert-backward-elif-block-functionp-test-m89EXO ()
  (should (functionp 'py-backward-elif-block)))

(ert-deftest py-ert-backward-else-block-functionp-test-gj0fUD ()
  (should (functionp 'py-backward-else-block)))

(ert-deftest py-ert-backward-elif-block-bol-functionp-test-wx8LMs ()
  (should (functionp 'py-backward-elif-block-bol)))

(ert-deftest py-ert-backward-else-block-bol-functionp-test-4oMICh ()
  (should (functionp 'py-backward-else-block-bol)))

(ert-deftest py-ert-indent-forward-line-functionp-test-sRNgr6 ()
  (should (functionp 'py-indent-forward-line)))

(ert-deftest py-ert-dedent-forward-line-functionp-test-IPhkdV ()
  (should (functionp 'py-dedent-forward-line)))

(ert-deftest py-ert-dedent-functionp-test-2BSUWJ ()
  (should (functionp 'py-dedent)))

(ert-deftest py-ert--close-intern-functionp-test-OU4bFy ()
  (should (functionp 'py--close-intern)))

(ert-deftest py-ert-close-def-functionp-test-aqcMhn ()
  (should (functionp 'py-close-def)))

(ert-deftest py-ert-close-class-functionp-test-esN2Tb ()
  (should (functionp 'py-close-class)))

(ert-deftest py-ert-close-block-functionp-test-aqe8s0 ()
  (should (functionp 'py-close-block)))

(ert-deftest py-ert-class-at-point-functionp-test-KycHZO ()
  (should (functionp 'py-class-at-point)))

(ert-deftest py-ert-py-match-paren-mode-functionp-test-y8e7wD ()
  (should (functionp 'py-match-paren-mode)))

(ert-deftest py-ert-py-match-paren-functionp-test-eu8e4r ()
  (should (functionp 'py-match-paren)))

(ert-deftest py-ert-pst-here-functionp-test-KE7U14 ()
  (should (functionp 'pst-here)))

(ert-deftest py-ert-printform-insert-functionp-test-IdTTsT ()
  (should (functionp 'py-printform-insert)))

(ert-deftest py-ert-line-to-printform-python2-functionp-test-oZT4UH ()
  (should (functionp 'py-line-to-printform-python2)))

(ert-deftest py-ert-end-of-elif-block-functionp-test-YtomSl ()
  (should (functionp 'py-forward-elif-block)))

(ert-deftest py-ert-end-of-elif-block-bol-functionp-test-6pREqb ()
  (should (functionp 'py-forward-elif-block-bol)))

(ert-deftest py-ert-end-of-else-block-functionp-test-2RnhX0 ()
  (should (functionp 'py-forward-else-block)))

(ert-deftest py-ert-end-of-else-block-bol-functionp-test-gnNDqQ ()
  (should (functionp 'py-forward-else-block-bol)))

(ert-deftest py-ert-mark-paragraph-functionp-test-wv6GUF ()
  (should (functionp 'py-mark-paragraph)))

(ert-deftest py-ert-mark-block-functionp-test-iCQcmv ()
  (should (functionp 'py-mark-block)))

(ert-deftest py-ert-mark-minor-block-functionp-test-Epf9Lk ()
  (should (functionp 'py-mark-minor-block)))

(ert-deftest py-ert-mark-clause-functionp-test-YFDX79 ()
  (should (functionp 'py-mark-clause)))

(ert-deftest py-ert-mark-block-or-clause-functionp-test-OybtsZ ()
  (should (functionp 'py-mark-block-or-clause)))

(ert-deftest py-ert-mark-def-functionp-test-kFxsKO ()
  (should (functionp 'py-mark-def)))

(ert-deftest py-ert-mark-class-functionp-test-GCFf3D ()
  (should (functionp 'py-mark-class)))

(ert-deftest py-ert-mark-def-or-class-functionp-test-Kkmojt ()
  (should (functionp 'py-mark-def-or-class)))

(ert-deftest py-ert-mark-line-functionp-test-kz3oxi ()
  (should (functionp 'py-mark-line)))

(ert-deftest py-ert-mark-statement-functionp-test-ewA7I7 ()
  (should (functionp 'py-mark-statement)))

(ert-deftest py-ert-mark-comment-functionp-test-SMtkSW ()
  (should (functionp 'py-mark-comment)))

(ert-deftest py-ert-mark-top-level-functionp-test-YtPoZL ()
  (should (functionp 'py-mark-top-level)))

(ert-deftest py-ert-mark-partial-expression-functionp-test-wDGB4A ()
  (should (functionp 'py-mark-partial-expression)))

(ert-deftest py-ert-mark-expression-functionp-test-mMk05p ()
  (should (functionp 'py-mark-expression)))

(ert-deftest py-ert--kill-emacs-hook-functionp-test-Q7Op9e ()
  (should (functionp 'py--kill-emacs-hook)))

(ert-deftest py-ert-python-version-functionp-test-Cs6083 ()
  (should (functionp 'py-python-version)))

(ert-deftest py-ert-version-functionp-test-GSzT9S ()
  (should (functionp 'py-version)))

(ert-deftest py-ert-load-file-functionp-test-6jBlYv ()
  (should (functionp 'py-load-file)))

(ert-deftest py-ert-proc-functionp-test-y8qUIj ()
  (should (functionp 'py-proc)))

(ert-deftest py-ert-guess-pdb-path-functionp-test-WQvRcV ()
  (should (functionp 'py-guess-pdb-path)))

(ert-deftest py-ert-toggle-local-default-use-functionp-test-imh2VI ()
  (should (functionp 'py-toggle-local-default-use)))

(ert-deftest py-ert--set-auto-fill-values-functionp-test-EnQdgk ()
  (should (functionp 'py--set-auto-fill-values)))

(ert-deftest py-ert--run-auto-fill-timer-functionp-test-SM8FR7 ()
  (should (functionp 'py--run-auto-fill-timer)))

(ert-deftest py-ert-complete-auto-functionp-test-iybItV ()
  (should (functionp 'py-complete-auto)))

(ert-deftest py-ert-emacs-version-greater-23-functionp-test-AnhhAw ()
  (should (functionp 'py---emacs-version-greater-23)))

(ert-deftest py-ert-symbol-at-point-functionp-test-8YC24j ()
  (should (functionp 'py-symbol-at-point)))

(ert-deftest py-ert-kill-buffer-unconditional-functionp-test-IlOkx7 ()
  (should (functionp 'py-kill-buffer-unconditional)))

(ert-deftest py-ert--line-backward-maybe-functionp-test-oBqrXU ()
  (should (functionp 'py--line-backward-maybe)))

(ert-deftest py-ert--after-empty-line-functionp-test-aeXdlI ()
  (should (functionp 'py--after-empty-line)))

(ert-deftest py-ert-compute-indentation-functionp-test-sRXJGv ()
  (should (functionp 'py-compute-indentation)))

(ert-deftest py-ert--py--fetch-indent-statement-above-functionp-test-Yzv5Zi ()
  (should (functionp 'py--fetch-indent-statement-above)))

(ert-deftest py-ert-continuation-offset-functionp-test-gFGVg6 ()
  (should (functionp 'py-continuation-offset)))

(ert-deftest py-ert-indentation-of-statement-functionp-test-O60wxT ()
  (should (functionp 'py-indentation-of-statement)))

(ert-deftest py-ert--in-comment-p-functionp-test-kBdXOG ()
  (should (functionp 'py--in-comment-p)))

(ert-deftest py-ert-in-triplequoted-string-p-functionp-test-UNn52t ()
  (should (functionp 'py-in-triplequoted-string-p)))

(ert-deftest py-ert-in-string-p-functionp-test-QLUbjh ()
  (should (functionp 'py-in-string-p)))

(ert-deftest py-ert-backward-top-level-functionp-test-CqtW4T ()
  (should (functionp 'py-backward-top-level)))

(ert-deftest py-ert--beginning-of-line-p-functionp-test-cDLgpI ()
  (should (functionp 'py--beginning-of-line-p)))

(ert-deftest py-ert--beginning-of-buffer-p-functionp-test-MZiYKw ()
  (should (functionp 'py--beginning-of-buffer-p)))

(ert-deftest py-ert--beginning-of-paragraph-p-functionp-test-ExVv4k ()
  (should (functionp 'py--beginning-of-paragraph-p)))

(ert-deftest py-ert--end-of-line-p-functionp-test-Wgrgk9 ()
  (should (functionp 'py--end-of-line-p)))

(ert-deftest py-ert--end-of-paragraph-p-functionp-test-EhfjBX ()
  (should (functionp 'py--end-of-paragraph-p)))

(ert-deftest py-ert--statement-opens-block-p-functionp-test-akDOQL ()
  (should (functionp 'py--statement-opens-block-p)))

(ert-deftest py-ert--statement-opens-base-functionp-test-qu591z ()
  (should (functionp 'py--statement-opens-base)))

(ert-deftest py-ert--statement-opens-clause-p-functionp-test-yg9Pbo ()
  (should (functionp 'py--statement-opens-clause-p)))

(ert-deftest py-ert--statement-opens-block-or-clause-p-functionp-test-YFIWic ()
  (should (functionp 'py--statement-opens-block-or-clause-p)))

(ert-deftest py-ert--statement-opens-class-p-functionp-test-yqOTq0 ()
  (should (functionp 'py--statement-opens-class-p)))

(ert-deftest py-ert--statement-opens-def-p-functionp-test-KQJJwO ()
  (should (functionp 'py--statement-opens-def-p)))

(ert-deftest py-ert--statement-opens-def-or-class-p-functionp-test-YZrPzC ()
  (should (functionp 'py--statement-opens-def-or-class-p)))

(ert-deftest py-ert--record-list-error-functionp-test-yqNLAq ()
  (should (functionp 'py--record-list-error)))

(ert-deftest py-ert--message-error-functionp-test-mq9oze ()
  (should (functionp 'py--message-error)))

(ert-deftest py-ert--end-base-functionp-test-Sgddw2 ()
  (should (functionp 'py--end-base)))

(ert-deftest py-ert--look-downward-for-beginning-functionp-test-4GmgpQ ()
  (should (functionp 'py--look-downward-for-beginning)))

(ert-deftest py-ert-look-downward-for-clause-functionp-test-OaxjkE ()
  (should (functionp 'py-look-downward-for-clause)))

(ert-deftest py-ert-current-defun-functionp-test-UfzHbs ()
  (should (functionp 'py-current-defun)))

(ert-deftest py-ert-sort-imports-functionp-test-SgHa4f ()
  (should (functionp 'py-sort-imports)))

(ert-deftest py-ert--in-literal-functionp-test-ozViW3 ()
  (should (functionp 'py--in-literal)))

(ert-deftest py-ert-count-lines-functionp-test-2980KR ()
  (should (functionp 'py-count-lines)))

(ert-deftest py-ert--point-functionp-test-iwvO3I ()
  (should (functionp 'py--point)))

(ert-deftest py-ert-install-local-shells-functionp-test-8S0MnA ()
  (should (functionp 'py-install-local-shells)))

(ert-deftest py-ert--until-found-functionp-test-6zyCEr ()
  (should (functionp 'py--until-found)))

(ert-deftest py-ert-which-def-or-class-functionp-test-IL7FVi ()
  (should (functionp 'py-which-def-or-class)))

(ert-deftest py-ert--fetch-first-python-buffer-functionp-test-uk0A99 ()
  (should (functionp 'py--fetch-first-python-buffer)))

(ert-deftest py-ert-unload-python-el-functionp-test-EhTUl1 ()
  (should (functionp 'py-unload-python-el)))

(ert-deftest py-ert--skip-to-semicolon-backward-functionp-test-MfBMyS ()
  (should (functionp 'py--skip-to-semicolon-backward)))

(ert-deftest py-ert--end-of-comment-intern-functionp-test-euILJJ ()
  (should (functionp 'py--end-of-comment-intern)))

(ert-deftest py-ert--skip-to-comment-or-semicolon-functionp-test-gDzQQA ()
  (should (functionp 'py--skip-to-comment-or-semicolon)))

(ert-deftest py-ert-backward-top-level-functionp-test-2zghVr ()
  (should (functionp 'py-backward-top-level)))

(ert-deftest py-ert-end-of-top-level-functionp-test-MjrkYi ()
  (should (functionp 'py-forward-top-level)))

(ert-deftest py-ert-end-of-top-level-bol-functionp-test-IxiNY9 ()
  (should (functionp 'py-forward-top-level-bol)))

(ert-deftest py-ert--beginning-of-line-form-functionp-test-2536W0 ()
  (should (functionp 'py--beginning-of-line-form)))

(ert-deftest py-ert--mark-base-functionp-test-iiTdTR ()
  (should (functionp 'py--mark-base)))

(ert-deftest py-ert--mark-base-bol-functionp-test-2rV3MI ()
  (should (functionp 'py--mark-base-bol)))

(ert-deftest py-ert-mark-base-functionp-test-C6bGEz ()
  (should (functionp 'py-mark-base)))

(ert-deftest py-ert-backward-same-level-functionp-test-W4OMtq ()
  (should (functionp 'py-backward-same-level)))

(ert-deftest py-ert--end-of-buffer-p-functionp-test-Ss9Pih ()
  (should (functionp 'py--end-of-buffer-p)))

(ert-deftest py-ert-info-lookup-symbol-functionp-test-wZCx87 ()
  (should (functionp 'py-info-lookup-symbol)))

(ert-deftest py-ert-python-after-info-look-functionp-test-aeFLVY ()
  (should (functionp 'python-after-info-look)))

(ert-deftest py-ert--warn-tmp-files-left-functionp-test-K0RdKP ()
  (should (functionp 'py--warn-tmp-files-left)))

(ert-deftest py-ert-fetch-docu-functionp-test-O6yavG ()
  (should (functionp 'py-fetch-docu)))

(ert-deftest py-ert-info-current-defun-functionp-test-g11hoy ()
  (should (functionp 'py-info-current-defun)))

(ert-deftest py-ert-help-at-point-functionp-test-QHd7hq ()
  (should (functionp 'py-help-at-point)))

(ert-deftest py-ert--dump-help-string-functionp-test-yUJrai ()
  (should (functionp 'py--dump-help-string)))

(ert-deftest py-ert-describe-mode-functionp-test-c1rvY9 ()
  (should (functionp 'py-describe-mode)))

(ert-deftest py-ert-find-definition-functionp-test-gVYZN1 ()
  (should (functionp 'py-find-definition)))

(ert-deftest py-ert-find-imports-functionp-test-eqihBT ()
  (should (functionp 'py-find-imports)))

(ert-deftest py-ert-update-imports-functionp-test-aqEHkL ()
  (should (functionp 'py-update-imports)))

(ert-deftest py-ert-pep8-run-functionp-test-Upxj2C ()
  (should (functionp 'py-pep8-run)))

(ert-deftest py-ert-pep8-help-functionp-test-QLavKu ()
  (should (functionp 'py-pep8-help)))

(ert-deftest py-ert-pylint-run-functionp-test-WaT3qm ()
  (should (functionp 'py-pylint-run)))

(ert-deftest py-ert-pylint-help-functionp-test-8iN33d ()
  (should (functionp 'py-pylint-help)))

(ert-deftest py-ert-pylint-doku-functionp-test-uS6sE5 ()
  (should (functionp 'py-pylint-doku)))

(ert-deftest py-ert-pyflakes3-run-functionp-test-Gue4cX ()
  (should (functionp 'py-pyflakes3-run)))

(ert-deftest py-ert-pyflakes3-help-functionp-test-iuxwJO ()
  (should (functionp 'py-pyflakes3-help)))

(ert-deftest py-ert-pyflakespep8-run-functionp-test-oNAJgG ()
  (should (functionp 'py-pyflakespep8-run)))

(ert-deftest py-ert-pyflakespep8-help-functionp-test-2FVOLx ()
  (should (functionp 'py-pyflakespep8-help)))

(ert-deftest py-ert-pychecker-run-functionp-test-IPCzep ()
  (should (functionp 'py-pychecker-run)))

(ert-deftest py-ert-check-command-functionp-test-SWw3Eg ()
  (should (functionp 'py-check-command)))

(ert-deftest py-ert-flake8-run-functionp-test-Kmzi37 ()
  (should (functionp 'py-flake8-run)))

(ert-deftest py-ert-flake8-help-functionp-test-Y1b7oZ ()
  (should (functionp 'py-flake8-help)))

(ert-deftest py-ert-nesting-level-functionp-test-o7PUFF ()
  (should (functionp 'py-nesting-level)))

(ert-deftest py-ert-toggle-flymake-intern-functionp-test-e6m8Lv ()
  (should (functionp 'py-toggle-flymake-intern)))

(ert-deftest py-ert-pylint-flymake-mode-functionp-test-m6Z4Sl ()
  (should (functionp 'pylint-flymake-mode)))

(ert-deftest py-ert-pyflakes-flymake-mode-functionp-test-eOjyWb ()
  (should (functionp 'pyflakes-flymake-mode)))

(ert-deftest py-ert-pychecker-flymake-mode-functionp-test-m21901 ()
  (should (functionp 'pychecker-flymake-mode)))

(ert-deftest py-ert-pep8-flymake-mode-functionp-test-aopq2R ()
  (should (functionp 'pep8-flymake-mode)))

(ert-deftest py-ert-pyflakespep8-flymake-mode-functionp-test-iUif5H ()
  (should (functionp 'pyflakespep8-flymake-mode)))

(ert-deftest py-display-state-of-variables-functionp-test-A9MG3x ()
  (should (functionp 'py-display-state-of-variables)))

(ert-deftest py-ert--quote-syntax-functionp-test-c3gy3n ()
  (should (functionp 'py--quote-syntax)))

(ert-deftest py-ert--python-send-setup-code-intern-functionp-test-iquh1d ()
  (should (functionp 'py--python-send-setup-code-intern)))

(ert-deftest py-ert--python-send-completion-setup-code-functionp-test-2jwfV3 ()
  (should (functionp 'py--python-send-completion-setup-code)))

;; (ert-deftest py-ert--python-send-eldoc-setup-code-functionp-test-AJBxNT ()
;;   (should (functionp 'py--python-send-eldoc-setup-code)))

(ert-deftest py-ert--ipython-import-module-completion-functionp-test-q8JhGJ ()
  (should (functionp 'py--ipython-import-module-completion)))

(ert-deftest py-ert--docstring-p-functionp-test-kXEwxz ()
  (should (functionp 'py--docstring-p)))

(ert-deftest py-ert--font-lock-syntactic-face-function-functionp-test-ysmcmp ()
  (should (functionp 'py--font-lock-syntactic-face-function)))

(ert-deftest py-ert-choose-shell-by-shebang-functionp-test-sdVC8e ()
  (should (functionp 'py-choose-shell-by-shebang)))

(ert-deftest py-ert--choose-shell-by-import-functionp-test-4gCOS4 ()
  (should (functionp 'py--choose-shell-by-import)))

(ert-deftest py-ert-choose-shell-by-path-functionp-test-iuCKAU ()
  (should (functionp 'py-choose-shell-by-path)))

(ert-deftest py-ert-which-python-functionp-test-EFnMgK ()
  (should (functionp 'py-which-python)))

(ert-deftest py-ert-python-current-environment-functionp-test-aQ7CUz ()
  (should (functionp 'py-python-current-environment)))

(ert-deftest py-ert--cleanup-process-name-functionp-test-OUw1vp ()
  (should (functionp 'py--cleanup-process-name)))

(ert-deftest py-ert-choose-shell-functionp-test-6xboeg ()
  (should (functionp 'py-choose-shell)))

(ert-deftest py-ert--normalize-directory-functionp-test-uYn0X6 ()
  (should (functionp 'py--normalize-directory)))

(ert-deftest py-ert-install-directory-check-functionp-test-6jS4DX ()
  (should (functionp 'py-install-directory-check)))

(ert-deftest py-ert-guess-py-install-directory-functionp-test-GKhflO ()
  (should (functionp 'py-guess-py-install-directory)))

(ert-deftest py-ert-load-pymacs-functionp-test-8A05YE ()
  (should (functionp 'py-load-pymacs)))

(ert-deftest py-ert-set-load-path-functionp-test-oPQqEv ()
  (should (functionp 'py-set-load-path)))

;; (ert-deftest py-ert-machine-separator-char-functionp-test-SYYdgm ()
;;   (should (functionp 'py-machine-separator-char)))

(ert-deftest py-ert-in-string-or-comment-p-functionp-test-SyBtr3 ()
  (should (functionp 'py-in-string-or-comment-p)))

(ert-deftest py-ert-electric-colon-functionp-test-UZzFYT ()
  (should (functionp 'py-electric-colon)))

(ert-deftest py-ert-electric-close-functionp-test-wrH2wK ()
  (should (functionp 'py-electric-close)))

(ert-deftest py-ert-electric-comment-functionp-test-WE0F1A ()
  (should (functionp 'py-electric-comment)))

(ert-deftest py-ert-empty-out-list-backward-functionp-test-Y5IBur ()
  (should (functionp 'py-empty-out-list-backward)))

(ert-deftest py-ert-electric-backspace-functionp-test-IDxUXh ()
  (should (functionp 'py-electric-backspace)))

(ert-deftest py-ert-electric-delete-functionp-test-uqFBo8 ()
  (should (functionp 'py-electric-delete)))

(ert-deftest py-ert-electric-yank-functionp-test-k95VNY ()
  (should (functionp 'py-electric-yank)))

(ert-deftest py-ert-backward-comment-functionp-test-uyjHaP ()
  (should (functionp 'py-backward-comment)))

(ert-deftest py-ert-forward-comment-functionp-test-E1mKtF ()
  (should (functionp 'py-forward-comment)))

(ert-deftest py-ert--uncomment-intern-functionp-test-W6YXYl ()
  (should (functionp 'py--uncomment-intern)))

(ert-deftest py-ert-uncomment-functionp-test-qewUac ()
  (should (functionp 'py-uncomment)))

(ert-deftest py-ert-comment-block-functionp-test-6njGk2 ()
  (should (functionp 'py-comment-block)))

(ert-deftest py-ert-comment-minor-block-functionp-test-8owmYV ()
  (should (functionp 'py-comment-minor-block)))

(ert-deftest py-ert-comment-top-level-functionp-test-0M1YCP ()
  (should (functionp 'py-comment-top-level)))

(ert-deftest py-ert-comment-clause-functionp-test-2BW0eJ ()
  (should (functionp 'py-comment-clause)))

(ert-deftest py-ert-comment-block-or-clause-functionp-test-0Y1gSC ()
  (should (functionp 'py-comment-block-or-clause)))

(ert-deftest py-ert-comment-def-functionp-test-M7iNrw ()
  (should (functionp 'py-comment-def)))

(ert-deftest py-ert-comment-class-functionp-test-qcGo2p ()
  (should (functionp 'py-comment-class)))

(ert-deftest py-ert-comment-def-or-class-functionp-test-2jFyzj ()
  (should (functionp 'py-comment-def-or-class)))

(ert-deftest py-ert-comment-statement-functionp-test-6Ngi8c ()
  (should (functionp 'py-comment-statement)))

(ert-deftest py-ert-delete-statement-functionp-test-sjIuD6 ()
  (should (functionp 'py-delete-statement)))

(ert-deftest py-ert-delete-top-level-functionp-test-slec9Z ()
  (should (functionp 'py-delete-top-level)))

(ert-deftest py-ert-delete-block-functionp-test-KW9LBT ()
  (should (functionp 'py-delete-block)))

(ert-deftest py-ert-delete-block-or-clause-functionp-test-0eq91M ()
  (should (functionp 'py-delete-block-or-clause)))

(ert-deftest py-ert-delete-def-functionp-test-ojpetG ()
  (should (functionp 'py-delete-def)))

(ert-deftest py-ert-delete-class-functionp-test-S8C1Qz ()
  (should (functionp 'py-delete-class)))

(ert-deftest py-ert-delete-def-or-class-functionp-test-C4Bnct ()
  (should (functionp 'py-delete-def-or-class)))

(ert-deftest py-ert-delete-expression-functionp-test-8WQUvm ()
  (should (functionp 'py-delete-expression)))

(ert-deftest py-ert-delete-partial-expression-functionp-test-ekIuQf ()
  (should (functionp 'py-delete-partial-expression)))

(ert-deftest py-ert-delete-minor-block-functionp-test-86tz88 ()
  (should (functionp 'py-delete-minor-block)))

(ert-deftest py-ert--imenu-create-index-functionp-test-4Ke0o2 ()
  (should (functionp 'py--imenu-create-index)))

(ert-deftest py-ert--imenu-create-index-engine-functionp-test-GCNUCV ()
  (should (functionp 'py--imenu-create-index-engine)))

(ert-deftest py-ert--imenu-create-index-new-functionp-test-y6DwOO ()
  (should (functionp 'py--imenu-create-index-new)))

(ert-deftest py-ert-execute-file-python-functionp-test-eyY2XH ()
  (should (functionp 'py-execute-file-python)))

(ert-deftest py-ert-execute-file-python-dedicated-functionp-test-Ipd44A ()
  (should (functionp 'py-execute-file-python-dedicated)))

(ert-deftest py-ert-execute-file-ipython-functionp-test-Yf9Zkv ()
  (should (functionp 'py-execute-file-ipython)))

(ert-deftest py-ert-execute-file-python3-functionp-test-Yfc3wp ()
  (should (functionp 'py-execute-file-python3)))

(ert-deftest py-ert-execute-file-python3-dedicated-functionp-test-oRLSIj ()
  (should (functionp 'py-execute-file-python3-dedicated)))

(ert-deftest py-ert-execute-file-python2-functionp-test-cHm0Vd ()
  (should (functionp 'py-execute-file-python2)))

(ert-deftest py-ert-execute-file-python2-dedicated-functionp-test-MhVE57 ()
  (should (functionp 'py-execute-file-python2-dedicated)))

(ert-deftest py-ert-execute-file-jython-functionp-test-iU7og2 ()
  (should (functionp 'py-execute-file-jython)))

(ert-deftest py-ert-execute-file-jython-dedicated-functionp-test-eKgDnW ()
  (should (functionp 'py-execute-file-jython-dedicated)))

(ert-deftest py-ert--shell-completion-get-completions-functionp-test-C2UZsQ ()
  (should (functionp 'py--shell-completion-get-completions)))

(ert-deftest py-ert--after-change-function-functionp-test-iWhMzK ()
  (should (functionp 'py--after-change-function)))

(ert-deftest py-ert--try-completion-intern-functionp-test-szwmCE ()
  (should (functionp 'py--try-completion-intern)))

(ert-deftest py-ert--try-completion-functionp-test-SeGdGy ()
  (should (functionp 'py--try-completion)))

(ert-deftest py--shell-do-completion-at-point-functionp-test-kfiiIs ()
  (should (functionp 'py--shell-do-completion-at-point)))

(ert-deftest py--shell-insert-completion-maybe-functionp-test-UtNoGm ()
  (should (functionp 'py--shell-insert-completion-maybe)))

(ert-deftest py-ert--complete-base-functionp-test-6l56Bg ()
  (should (functionp 'py--complete-base)))

(ert-deftest py-ert-shell-complete-functionp-test-uOc4va ()
  (should (functionp 'py-shell-complete)))

(ert-deftest py-ert-indent-or-complete-functionp-test-ySINq4 ()
  (should (functionp 'py-indent-or-complete)))

(ert-deftest py-ert-shift-left-functionp-test-WC1XiY ()
  (should (functionp 'py-shift-left)))

(ert-deftest py-ert-shift-right-functionp-test-KKvZ8R ()
  (should (functionp 'py-shift-right)))

(ert-deftest py-ert--shift-intern-functionp-test-SGYxXL ()
  (should (functionp 'py--shift-intern)))

(ert-deftest py-ert--shift-forms-base-functionp-test-4W4LJF ()
  (should (functionp 'py--shift-forms-base)))

(ert-deftest py-ert-shift-paragraph-right-functionp-test-MdFJtz ()
  (should (functionp 'py-shift-paragraph-right)))

(ert-deftest py-ert-shift-paragraph-left-functionp-test-gjjMat ()
  (should (functionp 'py-shift-paragraph-left)))

(ert-deftest py-ert-shift-block-right-functionp-test-isk6El ()
  (should (functionp 'py-shift-block-right)))

(ert-deftest py-ert-shift-block-left-functionp-test-0gAu6d ()
  (should (functionp 'py-shift-block-left)))

(ert-deftest py-ert-shift-minor-block-left-functionp-test-k9f4y6 ()
  (should (functionp 'py-shift-minor-block-left)))

(ert-deftest py-ert-shift-minor-block-right-functionp-test-amERXY ()
  (should (functionp 'py-shift-minor-block-right)))

(ert-deftest py-ert-shift-clause-right-functionp-test-OqD0nR ()
  (should (functionp 'py-shift-clause-right)))

(ert-deftest py-ert-shift-clause-left-functionp-test-kppHKJ ()
  (should (functionp 'py-shift-clause-left)))

(ert-deftest py-ert-shift-block-or-clause-right-functionp-test-ABEj8B ()
  (should (functionp 'py-shift-block-or-clause-right)))

(ert-deftest py-ert-shift-block-or-clause-left-functionp-test-U3q3su ()
  (should (functionp 'py-shift-block-or-clause-left)))

(ert-deftest py-ert-shift-def-right-functionp-test-U3v1Nm ()
  (should (functionp 'py-shift-def-right)))

(ert-deftest py-ert-shift-def-left-functionp-test-eGhS5e ()
  (should (functionp 'py-shift-def-left)))

(ert-deftest py-ert-shift-class-right-functionp-test-WCXzl7 ()
  (should (functionp 'py-shift-class-right)))

(ert-deftest py-ert-shift-class-left-functionp-test-WAJ0BZ ()
  (should (functionp 'py-shift-class-left)))

(ert-deftest py-ert-shift-def-or-class-right-functionp-test-88kGQR ()
  (should (functionp 'py-shift-def-or-class-right)))

(ert-deftest py-ert-shift-def-or-class-left-functionp-test-A7Vo1J ()
  (should (functionp 'py-shift-def-or-class-left)))

(ert-deftest py-ert-shift-statement-right-functionp-test-a6kuaC ()
  (should (functionp 'py-shift-statement-right)))

(ert-deftest py-ert-shift-statement-left-functionp-test-wPXmku ()
  (should (functionp 'py-shift-statement-left)))

(ert-deftest py-ert-end-of-block-functionp-test-02Rtsm ()
  (should (functionp 'py-forward-block)))

(ert-deftest py-ert-end-of-clause-functionp-test-0ed3xe ()
  (should (functionp 'py-forward-clause)))

(ert-deftest py-ert-end-of-block-or-clause-functionp-test-KMXnB6 ()
  (should (functionp 'py-forward-block-or-clause)))

(ert-deftest py-ert-end-of-def-functionp-test-2HHWAY ()
  (should (functionp 'py-forward-def)))

(ert-deftest py-ert-end-of-class-functionp-test-84ohyQ ()
  (should (functionp 'py-forward-class)))

(ert-deftest py-ert-py-toggle-smart-indentation-functionp-test ()   (should (functionp 'py-toggle-smart-indentation)))
(ert-deftest py-ert-py-smart-indentation-on-functionp-test ()   (should (functionp 'py-smart-indentation-on)))
(ert-deftest py-ert-py-smart-indentation-off-functionp-test ()   (should (functionp 'py-smart-indentation-off)))
(ert-deftest py-ert-py-toggle-sexp-function-functionp-test ()   (should (functionp 'py-toggle-sexp-function)))
(ert-deftest py-ert-py-toggle-switch-buffers-on-execute-p-functionp-test ()   (should (functionp 'py-toggle-switch-buffers-on-execute-p)))
(ert-deftest py-ert-py-switch-buffers-on-execute-p-on-functionp-test ()   (should (functionp 'py-switch-buffers-on-execute-p-on)))
(ert-deftest py-ert-py-switch-buffers-on-execute-p-off-functionp-test ()   (should (functionp 'py-switch-buffers-on-execute-p-off)))
(ert-deftest py-ert-py-toggle-split-window-on-execute-functionp-test ()   (should (functionp 'py-toggle-split-window-on-execute)))
(ert-deftest py-ert-py-split-window-on-execute-on-functionp-test ()   (should (functionp 'py-split-window-on-execute-on)))
(ert-deftest py-ert-py-split-window-on-execute-off-functionp-test ()   (should (functionp 'py-split-window-on-execute-off)))
(ert-deftest py-ert-py-toggle-fontify-shell-buffer-p-functionp-test ()   (should (functionp 'py-toggle-fontify-shell-buffer-p)))
(ert-deftest py-ert-py-fontify-shell-buffer-p-on-functionp-test ()   (should (functionp 'py-fontify-shell-buffer-p-on)))
(ert-deftest py-ert-py-fontify-shell-buffer-p-off-functionp-test ()   (should (functionp 'py-fontify-shell-buffer-p-off)))
(ert-deftest py-ert-py-toggle-python-mode-v5-behavior-p-functionp-test ()   (should (functionp 'py-toggle-python-mode-v5-behavior-p)))
(ert-deftest py-ert-py-python-mode-v5-behavior-p-on-functionp-test ()   (should (functionp 'py-python-mode-v5-behavior-p-on)))
(ert-deftest py-ert-py-python-mode-v5-behavior-p-off-functionp-test ()   (should (functionp 'py-python-mode-v5-behavior-p-off)))
(ert-deftest py-ert-py-toggle-jump-on-exception-functionp-test ()   (should (functionp 'py-toggle-jump-on-exception)))
(ert-deftest py-ert-py-jump-on-exception-on-functionp-test ()   (should (functionp 'py-jump-on-exception-on)))
(ert-deftest py-ert-py-jump-on-exception-off-functionp-test ()   (should (functionp 'py-jump-on-exception-off)))
(ert-deftest py-ert-py-toggle-use-current-dir-when-execute-p-functionp-test ()   (should (functionp 'py-toggle-use-current-dir-when-execute-p)))
(ert-deftest py-ert-py-use-current-dir-when-execute-p-on-functionp-test ()   (should (functionp 'py-use-current-dir-when-execute-p-on)))
(ert-deftest py-ert-py-use-current-dir-when-execute-p-off-functionp-test ()   (should (functionp 'py-use-current-dir-when-execute-p-off)))
(ert-deftest py-ert-py-toggle-electric-comment-p-functionp-test ()   (should (functionp 'py-toggle-electric-comment-p)))
(ert-deftest py-ert-py-electric-comment-p-on-functionp-test ()   (should (functionp 'py-electric-comment-p-on)))
(ert-deftest py-ert-py-electric-comment-p-off-functionp-test ()   (should (functionp 'py-electric-comment-p-off)))
(ert-deftest py-ert-py-toggle-underscore-word-syntax-p-functionp-test ()   (should (functionp 'py-toggle-underscore-word-syntax-p)))
(ert-deftest py-ert-py-underscore-word-syntax-p-on-functionp-test ()   (should (functionp 'py-underscore-word-syntax-p-on)))
(ert-deftest py-ert-py-underscore-word-syntax-p-off-functionp-test ()   (should (functionp 'py-underscore-word-syntax-p-off)))

(provide 'py-ert-function-tests)
;;; py-ert-function-tests.el ends here
