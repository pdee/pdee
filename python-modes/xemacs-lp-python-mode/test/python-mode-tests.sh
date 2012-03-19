#!/bin/bash
 # --

# Author: Andreas Roehler <andreas.roehler@online.de>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# Commentary:

#  tests Emacs python-mode
#
# Code:

# Edit the vars pointing to the directories/files
# holding your python-mode for test

# assumes python-mode files in current directory

# the path
# needs being in `test' directory
PDIR=`pwd`


# write PATH-TO-EMACS source code directory here
EMACS_SOURCE_DIR=
EMACS_SOURCE_DIR="$HOME/emacs-23.4"

# python-mode file to load
if [ -s "../python-components-mode.el" ];
    then
    PYTHONMODE="../python-components-mode.el"
    else
    PYTHONMODE="../python-mode.el"
fi

if [ $1 ]; then
    EMACS_SOURCE_DIR=$1
fi


if [ $EMACS_SOURCE_DIR ]; then

EMACS="${EMACS_SOURCE_DIR}/src/emacs"

# else
# EMACS=emacs
# when installed Emacs shall be used, CCCMDS must be set
# CCCMDS="${EMACS_SOURCE_DIR}/lisp/progmodes/cc-cmds.el"

# ERG=$(echo $LOGNAME | sed 's/^s\(.*\)/m/')
# if [ $ERG == "m" ]; then

    # EMACS_SOURCE_DIR="$HOME/emacs-20110426"
# else

    # EMACS_SOURCE_DIR="~/emacs-20110426"
# fi

HIGHL="highlight-indentation.el"
CLMACS="${EMACS_SOURCE_DIR}/lisp/emacs-lisp/cl-macs.el"
BYTECOMP="${EMACS_SOURCE_DIR}/lisp/emacs-lisp/bytecomp.el"
CUSTOM="${EMACS_SOURCE_DIR}/lisp/custom.el"
ANSICOLOR="${EMACS_SOURCE_DIR}/lisp/ansi-color.el"
COMINT="${EMACS_SOURCE_DIR}/lisp/comint.el"
CCCMDS="${EMACS_SOURCE_DIR}/lisp/progmodes/cc-cmds.el"
SHELL="${EMACS_SOURCE_DIR}/lisp/shell.el"
SKEL="${EMACS_SOURCE_DIR}/lisp/skeleton.el"
PYMACS="../pymacs.el"
# file holding the tests
TESTFILE="py-bug-numbered-tests.el"
TESTFILE2="python-mode-test.el"
CEXEC="python-extended-executes.el"

echo "\$PYMACS: $PYMACS"
echo "\$PYTHONMODE: $PYTHONMODE"
echo "\$PDIR/\$TESTFILE: $PDIR/$TESTFILE"

$EMACS -Q --batch --eval "(message (emacs-version))" --eval "(when (featurep 'python)(unload-feature 'python t))" --eval "(when (featurep 'python-mode)(unload-feature 'python-mode t))" --eval "(add-to-list 'load-path \"$PDIR/\")" --eval "(add-to-list 'load-path \"$TESTDIR/\")" --eval "(setq py-install-directory \"..\")" -load "$PYMACS" -load $CCCMDS -load $COMINT -load $SHELL -load $ANSICOLOR -load $CLMACS -load $BYTECOMP -load $CUSTOM -load $SKEL -load "../$HIGHL" -load $PYTHONMODE -load "$PDIR/$TESTFILE" -load "$PDIR/$TESTFILE2" --eval "(quietly-read-abbrev-file (expand-file-name \"~/.abbrev_defs\"))" \
--funcall nested-dictionaries-indent-lp:328791-test \
--funcall triple-quoted-string-dq-lp:302834-test \
--funcall dq-in-tqs-string-lp:328813-test \
--funcall py-current-defun-lp:328846-test \
--funcall cls-pseudo-keyword-lp:328849-test \
--funcall mark-decorators-lp:328851-test \
--funcall flexible-indentation-lp:328842-test \
--funcall hungry-delete-backwards-lp:328853-test \
--funcall hungry-delete-forward-lp:328853-test \
--funcall beg-end-of-defun-lp:303622-test \
--funcall bullet-lists-in-comments-lp:328782-test \
--funcall imenu-newline-arglist-lp:328783-test \
--funcall nested-indents-lp:328775-test \
--funcall imenu-matches-in-docstring-lp:436285-test \
--funcall exceptions-not-highlighted-lp:473525-test \
--funcall previous-statement-lp:637955-test \
--funcall inbound-indentation-multiline-assignment-lp:629916-test \
--funcall indentation-of-continuation-lines-lp:691185-test \
--funcall goto-beginning-of-tqs-lp:735328-test \
--funcall class-treated-as-keyword-lp:709478-test \
--funcall backslashed-continuation-line-indent-lp:742993-test \
--funcall py-decorators-face-lp:744335-test \
--funcall indent-after-return-lp:745208-test \
--funcall keep-assignments-column-lp:748198-test \
--funcall indent-triplequoted-to-itself-lp:752252-test \
--funcall multiline-listings-indent-lp:761946-test \
--funcall new-page-char-causes-loop-lp:762498-test \
--funcall nested-dicts-indent-lp:763756-test \
--funcall bad-indent-after-except-lp:771289-test \
--funcall indent-open-paren-not-last-lp:771291-test \
--funcall wrong-indent-after-else-lp:772610-test \
--funcall except-indents-wrong-lp:784432-test \
--funcall indent-explicitly-set-in-multiline-tqs-lp:784225-test \
--funcall unbalanced-parentheses-lp:784645-test \
--funcall explicitly-indent-in-list-lp:785018-test \
--funcall explicit-backslashed-continuation-line-indent-lp:785091-test \
--funcall indentation-error-lp:795773-test \
--funcall class-highlighted-as-keywords-lp:798287-test \
--funcall indent-function-arglist-lp:800088-test \
--funcall python-mode-hangs-lp:801780-test \
--funcall stops-backslashed-line-lp:802504-test \
--funcall stops-backslashed-line-lp:802504-test2 \
--funcall python-mode-slow-lp:803275-test \
--funcall py-variable-name-face-lp:798538-test \
--funcall colon-causes-error-lp:818665-test \
--funcall if-indentation-lp:818720-test \
--funcall closing-parentesis-indent-lp:821820-test \
--funcall py-indent-line-lp:822532-test \
--funcall indent-honor-arglist-whitespaces-lp:822540-test \
--funcall comments-indent-honor-setting-lp:824427-test \
--funcall infinite-loop-after-tqs-lp:826044-test \
--funcall closing-list-lp:826144-test \
--funcall py-electric-comment-add-space-lp:828398-test \
--funcall py-electric-comment-add-space-t-lp:828398-test \
--funcall wrong-indentation-of-function-arguments-lp:840891-test \
--funcall wrong-guess-for-py-indent-offset-lp-852052-test \
--funcall indent-match-import-pkg-lp-852500-test \
--funcall py-hungry-delete-backwards-needs-cc-lp-850595-test \
--funcall py-shift-line-when-no-region-lp-855565-test \
--funcall indentation-of-from-import-continuation-lines-lp-858041-test \
--funcall indentation-after-one-line-suites-lp:858044-test \
--funcall py-compute-indentation-wrong-at-eol-lp-858043-test \
--funcall comment-indentation-level-lp-869854-test \
--funcall indentation-wrong-after-multi-line-parameter-list-lp-871698-test \
--funcall no-indent-after-continue-lp-872676-test \
--funcall indent-after-inline-comment-lp-873372-test \
--funcall else-clause-indentation-lp-874470-test \
--funcall indent-after-multiple-except-statements-lp:883815-test \
--funcall wrongly-highlighted-as-keywords-lp-885144-test \
--funcall glitch-when-indenting-lists-lp-886473-test \
--funcall another-indentation-bug-inside-docstrings-lp:900684-test \
--funcall incorrect-use-of-region-in-py-shift-left-lp:875951-test \
--funcall indentation-keyword-lp-885143-test \
--funcall fore-00007F-breaks-indentation-lp:328788-test \
--funcall indent-offset-not-guessed-when-loading-lp:902890-test \
--funcall from-__future__-import-absolute_import-mishighlighted-lp-907084-test \
--funcall automatic-indentation-is-broken-lp-889643-test \
--funcall chars-uU-preceding-triple-quoted-get-string-face-lp-909517-test \
--funcall py-pychecker-run-missing-lp-910783-test \
--funcall py-forward-into-nomenclature-lp-916818-test \
--funcall py-forward-into-nomenclature-jumps-over-CamelCased-words-lp:919540-test \
--funcall py-backward-into-nomenclature-caps-names-lp:919541-test \
--funcall fourth-level-blocks-indent-incorrectly-lp-939577-test \
--funcall py-mark-expression-marks-too-much-lp-941140-test \
--funcall py-indent-comments-nil-ignored-lp-958721-test \
\
--funcall multiline-list-indent-test \
--funcall py-beginning-of-block-test \
--funcall py-end-of-block-test \
--funcall py-beginning-of-block-or-clause-test \
--funcall py-end-of-block-or-clause-test \
--funcall py-beginning-of-def-test \
--funcall py-end-of-def-test \
--funcall py-beginning-of-def-or-class-test \
--funcall py-end-of-def-or-class-test \
--funcall py-electric-delete-test \
--funcall dict-error-test \
--funcall near-bob-beginning-of-statement-test \
--funcall honor-comments-indent-test \
--funcall bob-beginning-of-statement-test \
--funcall first-line-offset-test \
--funcall assignment-indent-test \
--funcall if-elif-test \
--funcall if-elif-bob-test \
--funcall try-else-clause-test \
--funcall try-except-test \
--funcall assignment-after-block-test \
--funcall py-beginning-of-clause-test \
--funcall py-end-of-clause-test \
--funcall py-beginning-of-expression-test \
--funcall py-end-of-expression-test \
--funcall py-expression-index-test \
--funcall py-indent-after-assigment-test \
--funcall leave-dict-test \
--funcall eofs-attribut-test \
--funcall args-list-first-line-indent-test \
--funcall py-partial-expression-test \
--funcall close-block-test \
--funcall py-shift-block-test \
--funcall nesting-if-test \
--funcall nested-try-test \
--funcall nested-if-test \
--funcall py-guess-pdb-path-commandp-test \
--funcall highlight-indentation-on-commandp-test \
--funcall highlight-indentation-off-commandp-test \
--funcall highlight-indentation-commandp-test \
--funcall py-in-string-or-comment-p-commandp-test \
--funcall py-insert-default-shebang-commandp-test \
--funcall py-electric-comment-commandp-test \
--funcall py-electric-colon-commandp-test \
--funcall py-electric-backspace-commandp-test \
--funcall py-electric-delete-commandp-test \
--funcall py-indent-line-outmost-commandp-test \
--funcall py-indent-line-commandp-test \
--funcall py-newline-and-indent-commandp-test \
--funcall py-newline-and-dedent-commandp-test \
--funcall toggle-indent-tabs-mode-commandp-test \
--funcall indent-tabs-mode-commandp-test \
--funcall indent-tabs-mode-on-commandp-test \
--funcall indent-tabs-mode-off-commandp-test \
--funcall py-guess-indent-offset-commandp-test \
--funcall py-narrow-to-defun-commandp-test \
--funcall py-shift-left-commandp-test \
--funcall py-shift-right-commandp-test \
--funcall py-shift-paragraph-right-commandp-test \
--funcall py-shift-paragraph-left-commandp-test \
--funcall py-shift-block-right-commandp-test \
--funcall py-shift-block-left-commandp-test \
--funcall py-shift-clause-right-commandp-test \
--funcall py-shift-clause-left-commandp-test \
--funcall py-shift-def-right-commandp-test \
--funcall py-shift-def-left-commandp-test \
--funcall py-shift-class-right-commandp-test \
--funcall py-shift-class-left-commandp-test \
--funcall py-shift-line-right-commandp-test \
--funcall py-shift-line-left-commandp-test \
--funcall py-shift-statement-right-commandp-test \
--funcall py-shift-statement-left-commandp-test \
--funcall py-indent-region-commandp-test \
--funcall py-beginning-of-paragraph-position-commandp-test \
--funcall py-end-of-paragraph-position-commandp-test \
--funcall py-beginning-of-block-position-commandp-test \
--funcall py-end-of-block-position-commandp-test \
--funcall py-beginning-of-clause-position-commandp-test \
--funcall py-end-of-clause-position-commandp-test \
--funcall py-beginning-of-block-or-clause-position-commandp-test \
--funcall py-end-of-block-or-clause-position-commandp-test \
--funcall py-beginning-of-def-position-commandp-test \
--funcall py-end-of-def-position-commandp-test \
--funcall py-beginning-of-class-position-commandp-test \
--funcall py-end-of-class-position-commandp-test \
--funcall py-beginning-of-def-or-class-position-commandp-test \
--funcall py-end-of-def-or-class-position-commandp-test \
--funcall py-beginning-of-line-position-commandp-test \
--funcall py-end-of-line-position-commandp-test \
--funcall py-beginning-of-statement-position-commandp-test \
--funcall py-end-of-statement-position-commandp-test \
--funcall py-beginning-of-expression-position-commandp-test \
--funcall py-end-of-expression-position-commandp-test \
--funcall py-beginning-of-partial-expression-position-commandp-test \
--funcall py-end-of-partial-expression-position-commandp-test \
--funcall py-bounds-of-statement-commandp-test \
--funcall py-bounds-of-block-commandp-test \
--funcall py-bounds-of-clause-commandp-test \
--funcall py-bounds-of-block-or-clause-commandp-test \
--funcall py-bounds-of-def-commandp-test \
--funcall py-bounds-of-class-commandp-test \
--funcall py-bounds-of-region-commandp-test \
--funcall py-bounds-of-buffer-commandp-test \
--funcall py-bounds-of-expression-commandp-test \
--funcall py-bounds-of-partial-expression-commandp-test \
--funcall py-bounds-of-declarations-commandp-test \
--funcall py-beginning-of-declarations-commandp-test \
--funcall py-end-of-declarations-commandp-test \
--funcall py-declarations-commandp-test \
--funcall py-kill-declarations-commandp-test \
--funcall py-bounds-of-statements-commandp-test \
--funcall py-beginning-of-statements-commandp-test \
--funcall py-end-of-statements-commandp-test \
--funcall py-statements-commandp-test \
--funcall py-kill-statements-commandp-test \
--funcall py-comment-region-commandp-test \
--funcall py-fill-paragraph-commandp-test \
--funcall py-insert-super-commandp-test \
--funcall py-nesting-level-commandp-test \
--funcall py-compute-indentation-commandp-test \
--funcall py-continuation-offset-commandp-test \
--funcall py-indentation-of-statement-commandp-test \
--funcall py-list-beginning-position-commandp-test \
--funcall py-end-of-list-position-commandp-test \
--funcall py-in-triplequoted-string-p-commandp-test \
--funcall py-in-string-p-commandp-test \
--funcall py-in-statement-p-commandp-test \
--funcall py-beginning-of-paragraph-p-commandp-test \
--funcall py-beginning-of-line-p-commandp-test \
--funcall py-beginning-of-statement-p-commandp-test \
--funcall py-beginning-of-expression-p-commandp-test \
--funcall py-beginning-of-partial-expression-p-commandp-test \
--funcall py-beginning-of-block-p-commandp-test \
--funcall py-beginning-of-clause-p-commandp-test \
--funcall py-beginning-of-block-or-clause-p-commandp-test \
--funcall py-beginning-of-def-p-commandp-test \
--funcall py-beginning-of-class-p-commandp-test \
--funcall py-beginning-of-def-or-class-p-commandp-test \
--funcall py-statement-opens-block-p-commandp-test \
--funcall py-statement-opens-clause-p-commandp-test \
--funcall py-statement-opens-block-or-clause-p-commandp-test \
--funcall py-statement-opens-class-p-commandp-test \
--funcall py-statement-opens-def-p-commandp-test \
--funcall py-statement-opens-def-or-class-p-commandp-test \
--funcall py-current-defun-commandp-test \
--funcall py-sort-imports-commandp-test \
--funcall empty-line-p-commandp-test \
--funcall py-count-lines-commandp-test \
--funcall py-which-function-commandp-test \
--funcall py-beginning-of-block-commandp-test \
--funcall py-beginning-of-if-block-commandp-test \
--funcall py-beginning-of-try-block-commandp-test \
--funcall py-end-of-block-commandp-test \
--funcall py-beginning-of-block-or-clause-commandp-test \
--funcall py-end-of-block-or-clause-commandp-test \
--funcall py-beginning-of-class-commandp-test \
--funcall py-end-of-class-commandp-test \
--funcall py-beginning-of-clause-commandp-test \
--funcall py-end-of-clause-commandp-test \
--funcall py-beginning-of-def-commandp-test \
--funcall py-end-of-def-commandp-test \
--funcall py-beginning-of-def-or-class-commandp-test \
--funcall py-end-of-def-or-class-commandp-test \
--funcall py-beginning-of-expression-commandp-test \
--funcall py-end-of-expression-commandp-test \
--funcall py-beginning-of-partial-expression-commandp-test \
--funcall py-end-of-partial-expression-commandp-test \
--funcall py-beginning-of-statement-commandp-test \
--funcall py-end-of-statement-commandp-test \
--funcall py-goto-statement-below-commandp-test \
--funcall py-mark-paragraph-commandp-test \
--funcall py-mark-block-commandp-test \
--funcall py-mark-clause-commandp-test \
--funcall py-mark-block-or-clause-commandp-test \
--funcall py-mark-def-commandp-test \
--funcall py-mark-class-commandp-test \
--funcall py-mark-def-or-class-commandp-test \
--funcall py-mark-line-commandp-test \
--funcall py-mark-statement-commandp-test \
--funcall py-mark-expression-commandp-test \
--funcall py-mark-partial-expression-commandp-test \
--funcall py-beginning-of-decorator-commandp-test \
--funcall py-end-of-decorator-commandp-test \
--funcall py-copy-expression-commandp-test \
--funcall py-copy-partial-expression-commandp-test \
--funcall py-copy-statement-commandp-test \
--funcall py-copy-block-commandp-test \
--funcall py-copy-block-or-clause-commandp-test \
--funcall py-copy-def-commandp-test \
--funcall py-copy-def-or-class-commandp-test \
--funcall py-copy-class-commandp-test \
--funcall py-copy-clause-commandp-test \
--funcall py-kill-expression-commandp-test \
--funcall py-kill-partial-expression-commandp-test \
--funcall py-kill-statement-commandp-test \
--funcall py-kill-block-commandp-test \
--funcall py-kill-block-or-clause-commandp-test \
--funcall py-kill-def-or-class-commandp-test \
--funcall py-kill-class-commandp-test \
--funcall py-kill-def-commandp-test \
--funcall py-kill-clause-commandp-test \
--funcall py-forward-line-commandp-test \
--funcall py-beginning-of-comment-commandp-test \
--funcall py-leave-comment-or-string-backward-commandp-test \
--funcall py-beginning-of-list-pps-commandp-test \
--funcall py-down-block-lc-commandp-test \
--funcall py-down-clause-lc-commandp-test \
--funcall py-down-def-lc-commandp-test \
--funcall py-down-class-lc-commandp-test \
--funcall py-down-statement-lc-commandp-test \
--funcall py-down-statement-commandp-test \
--funcall py-down-block-commandp-test \
--funcall py-down-clause-commandp-test \
--funcall py-down-block-or-clause-commandp-test \
--funcall py-down-def-commandp-test \
--funcall py-down-class-commandp-test \
--funcall py-down-def-or-class-commandp-test \
--funcall py-forward-into-nomenclature-commandp-test \
--funcall py-backward-into-nomenclature-commandp-test \
--funcall match-paren-commandp-test \
--funcall py-toggle-execute-keep-temporary-file-p-commandp-test \
--funcall py-guess-default-python-commandp-test \
--funcall py-set-shell-completion-environment-commandp-test \
--funcall py-set-ipython-completion-command-string-commandp-test \
--funcall py-shell-dedicated-commandp-test \
--funcall py-shell-commandp-test \
--funcall python-commandp-test \
--funcall ipython-commandp-test \
--funcall python3-commandp-test \
--funcall python2-commandp-test \
--funcall python2.7-commandp-test \
--funcall jython-commandp-test \
--funcall python3.2-commandp-test \
--funcall python-dedicated-commandp-test \
--funcall ipython-dedicated-commandp-test \
--funcall python3-dedicated-commandp-test \
--funcall python2-dedicated-commandp-test \
--funcall python2.7-dedicated-commandp-test \
--funcall jython-dedicated-commandp-test \
--funcall python3.2-dedicated-commandp-test \
--funcall python-switch-commandp-test \
--funcall ipython-switch-commandp-test \
--funcall python3-switch-commandp-test \
--funcall python2-switch-commandp-test \
--funcall python2.7-switch-commandp-test \
--funcall jython-switch-commandp-test \
--funcall python3.2-switch-commandp-test \
--funcall python-no-switch-commandp-test \
--funcall ipython-no-switch-commandp-test \
--funcall python3-no-switch-commandp-test \
--funcall python2-no-switch-commandp-test \
--funcall python2.7-no-switch-commandp-test \
--funcall jython-no-switch-commandp-test \
--funcall python3.2-no-switch-commandp-test \
--funcall python-switch-dedicated-commandp-test \
--funcall ipython-switch-dedicated-commandp-test \
--funcall python3-switch-dedicated-commandp-test \
--funcall python2-switch-dedicated-commandp-test \
--funcall python2.7-switch-dedicated-commandp-test \
--funcall jython-switch-dedicated-commandp-test \
--funcall python3.2-switch-dedicated-commandp-test \
--funcall py-which-execute-file-command-commandp-test \
--funcall py-execute-region-no-switch-commandp-test \
--funcall py-execute-region-switch-commandp-test \
--funcall py-execute-region-commandp-test \
--funcall py-execute-region-default-commandp-test \
--funcall py-execute-region-dedicated-commandp-test \
--funcall py-execute-region-default-dedicated-commandp-test \
--funcall py-execute-string-commandp-test \
--funcall py-execute-string-dedicated-commandp-test \
--funcall py-fetch-py-master-file-commandp-test \
--funcall py-execute-import-or-reload-commandp-test \
--funcall py-execute-buffer-dedicated-commandp-test \
--funcall py-execute-buffer-switch-commandp-test \
--funcall py-execute-buffer-dedicated-switch-commandp-test \
--funcall py-execute-buffer-commandp-test \
--funcall py-execute-buffer-no-switch-commandp-test \
--funcall py-execute-defun-commandp-test \
--funcall py-process-file-commandp-test \
--funcall py-exec-execfile-region-commandp-test \
--funcall py-exec-execfile-commandp-test \
--funcall py-execute-block-commandp-test \
--funcall py-execute-block-or-clause-commandp-test \
--funcall py-execute-class-commandp-test \
--funcall py-execute-clause-commandp-test \
--funcall py-execute-def-commandp-test \
--funcall py-execute-def-or-class-commandp-test \
--funcall py-execute-expression-commandp-test \
--funcall py-execute-partial-expression-commandp-test \
--funcall py-execute-statement-commandp-test \
--funcall py-execute-file-commandp-test \
--funcall py-down-exception-commandp-test \
--funcall py-up-exception-commandp-test \
--funcall py-output-buffer-filter-commandp-test \
--funcall py-send-string-commandp-test \
--funcall py-pdbtrack-toggle-stack-tracking-commandp-test \
--funcall turn-on-pdbtrack-commandp-test \
--funcall turn-off-pdbtrack-commandp-test \
--funcall py-fetch-docu-commandp-test \
--funcall py-find-imports-commandp-test \
--funcall python-find-imports-commandp-test \
--funcall py-describe-symbol-commandp-test \
--funcall py-describe-mode-commandp-test \
--funcall py-find-function-commandp-test \
--funcall py-update-imports-commandp-test \
--funcall py-indent-forward-line-commandp-test \
--funcall py-dedent-forward-line-commandp-test \
--funcall py-dedent-commandp-test \
--funcall py-close-def-commandp-test \
--funcall py-close-class-commandp-test \
--funcall py-close-clause-commandp-test \
--funcall py-close-block-commandp-test \
--funcall py-class-at-point-commandp-test \
--funcall py-match-paren-commandp-test \
--funcall eva-commandp-test \
--funcall pst-here-commandp-test \
--funcall py-printform-insert-commandp-test \
--funcall py-line-to-printform-python2-commandp-test \
--funcall py-switch-imenu-index-function-commandp-test \
--funcall py-completion-at-point-commandp-test \
--funcall py-choose-shell-by-shebang-commandp-test \
--funcall py-which-python-commandp-test \
--funcall py-python-current-environment-commandp-test \
--funcall py-switch-shell-commandp-test \
--funcall py-choose-shell-commandp-test \
--funcall py-toggle-smart-indentation-commandp-test \
--funcall py-smart-indentation-on-commandp-test \
--funcall py-smart-indentation-off-commandp-test \
--funcall py-toggle-split-windows-on-execute-commandp-test \
--funcall py-split-windows-on-execute-on-commandp-test \
--funcall py-split-windows-on-execute-off-commandp-test \
--funcall py-toggle-shell-switch-buffers-on-execute-commandp-test \
--funcall py-shell-switch-buffers-on-execute-on-commandp-test \
--funcall py-shell-switch-buffers-on-execute-off-commandp-test \
--funcall py-normalize-py-install-directory-commandp-test \
--funcall py-install-directory-check-commandp-test \
--funcall py-load-pymacs-commandp-test \
--funcall py-guess-py-install-directory-commandp-test \
--funcall py-set-load-path-commandp-test \
--funcall py-def-or-class-beginning-position-commandp-test \
--funcall py-def-or-class-end-position-commandp-test \
--funcall py-statement-beginning-position-commandp-test \
--funcall py-statement-end-position-commandp-test \
--funcall py-current-indentation-commandp-test \
--funcall py-version-commandp-test \
--funcall run-python-commandp-test \
--funcall py-send-region-commandp-test \
--funcall py-send-buffer-commandp-test \
--funcall py-switch-to-python-commandp-test \
--funcall py-send-region-and-go-commandp-test \
--funcall py-load-file-commandp-test \
--funcall py-set-proc-commandp-test \
--funcall python-send-string-commandp-test \
--funcall py-shell-complete-commandp-test \
--funcall ipython-complete-commandp-test \
--funcall py-pychecker-run-commandp-test \
--funcall virtualenv-current-commandp-test \
--funcall virtualenv-activate-commandp-test \
--funcall virtualenv-deactivate-commandp-test \
--funcall virtualenv-workon-commandp-test \
--funcall py-toggle-local-default-use-commandp-test \
--funcall py-execute-statement-python-commandp-test \
--funcall py-execute-statement-python-switch-commandp-test \
--funcall py-execute-statement-python-noswitch-commandp-test \
--funcall py-execute-statement-python-dedicated-commandp-test \
--funcall py-execute-statement-python-dedicated-switch-commandp-test \
--funcall py-execute-statement-ipython-commandp-test \
--funcall py-execute-statement-ipython-switch-commandp-test \
--funcall py-execute-statement-ipython-noswitch-commandp-test \
--funcall py-execute-statement-ipython-dedicated-commandp-test \
--funcall py-execute-statement-ipython-dedicated-switch-commandp-test \
--funcall py-execute-statement-python3-commandp-test \
--funcall py-execute-statement-python3-switch-commandp-test \
--funcall py-execute-statement-python3-noswitch-commandp-test \
--funcall py-execute-statement-python3-dedicated-commandp-test \
--funcall py-execute-statement-python3-dedicated-switch-commandp-test \
--funcall py-execute-statement-python2-commandp-test \
--funcall py-execute-statement-python2-switch-commandp-test \
--funcall py-execute-statement-python2-noswitch-commandp-test \
--funcall py-execute-statement-python2-dedicated-commandp-test \
--funcall py-execute-statement-python2-dedicated-switch-commandp-test \
--funcall py-execute-statement-python2.7-commandp-test \
--funcall py-execute-statement-python2.7-switch-commandp-test \
--funcall py-execute-statement-python2.7-noswitch-commandp-test \
--funcall py-execute-statement-python2.7-dedicated-commandp-test \
--funcall py-execute-statement-python2.7-dedicated-switch-commandp-test \
--funcall py-execute-statement-jython-commandp-test \
--funcall py-execute-statement-jython-switch-commandp-test \
--funcall py-execute-statement-jython-noswitch-commandp-test \
--funcall py-execute-statement-jython-dedicated-commandp-test \
--funcall py-execute-statement-jython-dedicated-switch-commandp-test \
--funcall py-execute-statement-python3.2-commandp-test \
--funcall py-execute-statement-python3.2-switch-commandp-test \
--funcall py-execute-statement-python3.2-noswitch-commandp-test \
--funcall py-execute-statement-python3.2-dedicated-commandp-test \
--funcall py-execute-statement-python3.2-dedicated-switch-commandp-test \
--funcall py-execute-block-python-commandp-test \
--funcall py-execute-block-python-switch-commandp-test \
--funcall py-execute-block-python-noswitch-commandp-test \
--funcall py-execute-block-python-dedicated-commandp-test \
--funcall py-execute-block-python-dedicated-switch-commandp-test \
--funcall py-execute-block-ipython-commandp-test \
--funcall py-execute-block-ipython-switch-commandp-test \
--funcall py-execute-block-ipython-noswitch-commandp-test \
--funcall py-execute-block-ipython-dedicated-commandp-test \
--funcall py-execute-block-ipython-dedicated-switch-commandp-test \
--funcall py-execute-block-python3-commandp-test \
--funcall py-execute-block-python3-switch-commandp-test \
--funcall py-execute-block-python3-noswitch-commandp-test \
--funcall py-execute-block-python3-dedicated-commandp-test \
--funcall py-execute-block-python3-dedicated-switch-commandp-test \
--funcall py-execute-block-python2-commandp-test \
--funcall py-execute-block-python2-switch-commandp-test \
--funcall py-execute-block-python2-noswitch-commandp-test \
--funcall py-execute-block-python2-dedicated-commandp-test \
--funcall py-execute-block-python2-dedicated-switch-commandp-test \
--funcall py-execute-block-python2.7-commandp-test \
--funcall py-execute-block-python2.7-switch-commandp-test \
--funcall py-execute-block-python2.7-noswitch-commandp-test \
--funcall py-execute-block-python2.7-dedicated-commandp-test \
--funcall py-execute-block-python2.7-dedicated-switch-commandp-test \
--funcall py-execute-block-jython-commandp-test \
--funcall py-execute-block-jython-switch-commandp-test \
--funcall py-execute-block-jython-noswitch-commandp-test \
--funcall py-execute-block-jython-dedicated-commandp-test \
--funcall py-execute-block-jython-dedicated-switch-commandp-test \
--funcall py-execute-block-python3.2-commandp-test \
--funcall py-execute-block-python3.2-switch-commandp-test \
--funcall py-execute-block-python3.2-noswitch-commandp-test \
--funcall py-execute-block-python3.2-dedicated-commandp-test \
--funcall py-execute-block-python3.2-dedicated-switch-commandp-test \
--funcall py-execute-clause-python-commandp-test \
--funcall py-execute-clause-python-switch-commandp-test \
--funcall py-execute-clause-python-noswitch-commandp-test \
--funcall py-execute-clause-python-dedicated-commandp-test \
--funcall py-execute-clause-python-dedicated-switch-commandp-test \
--funcall py-execute-clause-ipython-commandp-test \
--funcall py-execute-clause-ipython-switch-commandp-test \
--funcall py-execute-clause-ipython-noswitch-commandp-test \
--funcall py-execute-clause-ipython-dedicated-commandp-test \
--funcall py-execute-clause-ipython-dedicated-switch-commandp-test \
--funcall py-execute-clause-python3-commandp-test \
--funcall py-execute-clause-python3-switch-commandp-test \
--funcall py-execute-clause-python3-noswitch-commandp-test \
--funcall py-execute-clause-python3-dedicated-commandp-test \
--funcall py-execute-clause-python3-dedicated-switch-commandp-test \
--funcall py-execute-clause-python2-commandp-test \
--funcall py-execute-clause-python2-switch-commandp-test \
--funcall py-execute-clause-python2-noswitch-commandp-test \
--funcall py-execute-clause-python2-dedicated-commandp-test \
--funcall py-execute-clause-python2-dedicated-switch-commandp-test \
--funcall py-execute-clause-python2.7-commandp-test \
--funcall py-execute-clause-python2.7-switch-commandp-test \
--funcall py-execute-clause-python2.7-noswitch-commandp-test \
--funcall py-execute-clause-python2.7-dedicated-commandp-test \
--funcall py-execute-clause-python2.7-dedicated-switch-commandp-test \
--funcall py-execute-clause-jython-commandp-test \
--funcall py-execute-clause-jython-switch-commandp-test \
--funcall py-execute-clause-jython-noswitch-commandp-test \
--funcall py-execute-clause-jython-dedicated-commandp-test \
--funcall py-execute-clause-jython-dedicated-switch-commandp-test \
--funcall py-execute-clause-python3.2-commandp-test \
--funcall py-execute-clause-python3.2-switch-commandp-test \
--funcall py-execute-clause-python3.2-noswitch-commandp-test \
--funcall py-execute-clause-python3.2-dedicated-commandp-test \
--funcall py-execute-clause-python3.2-dedicated-switch-commandp-test \
--funcall py-execute-block-or-clause-python-commandp-test \
--funcall py-execute-block-or-clause-python-switch-commandp-test \
--funcall py-execute-block-or-clause-python-noswitch-commandp-test \
--funcall py-execute-block-or-clause-python-dedicated-commandp-test \
--funcall py-execute-block-or-clause-python-dedicated-switch-commandp-test \
--funcall py-execute-block-or-clause-ipython-commandp-test \
--funcall py-execute-block-or-clause-ipython-switch-commandp-test \
--funcall py-execute-block-or-clause-ipython-noswitch-commandp-test \
--funcall py-execute-block-or-clause-ipython-dedicated-commandp-test \
--funcall py-execute-block-or-clause-ipython-dedicated-switch-commandp-test \
--funcall py-execute-block-or-clause-python3-commandp-test \
--funcall py-execute-block-or-clause-python3-switch-commandp-test \
--funcall py-execute-block-or-clause-python3-noswitch-commandp-test \
--funcall py-execute-block-or-clause-python3-dedicated-commandp-test \
--funcall py-execute-block-or-clause-python3-dedicated-switch-commandp-test \
--funcall py-execute-block-or-clause-python2-commandp-test \
--funcall py-execute-block-or-clause-python2-switch-commandp-test \
--funcall py-execute-block-or-clause-python2-noswitch-commandp-test \
--funcall py-execute-block-or-clause-python2-dedicated-commandp-test \
--funcall py-execute-block-or-clause-python2-dedicated-switch-commandp-test \
--funcall py-execute-block-or-clause-python2.7-commandp-test \
--funcall py-execute-block-or-clause-python2.7-switch-commandp-test \
--funcall py-execute-block-or-clause-python2.7-noswitch-commandp-test \
--funcall py-execute-block-or-clause-python2.7-dedicated-commandp-test \
--funcall py-execute-block-or-clause-python2.7-dedicated-switch-commandp-test \
--funcall py-execute-block-or-clause-jython-commandp-test \
--funcall py-execute-block-or-clause-jython-switch-commandp-test \
--funcall py-execute-block-or-clause-jython-noswitch-commandp-test \
--funcall py-execute-block-or-clause-jython-dedicated-commandp-test \
--funcall py-execute-block-or-clause-jython-dedicated-switch-commandp-test \
--funcall py-execute-block-or-clause-python3.2-commandp-test \
--funcall py-execute-block-or-clause-python3.2-switch-commandp-test \
--funcall py-execute-block-or-clause-python3.2-noswitch-commandp-test \
--funcall py-execute-block-or-clause-python3.2-dedicated-commandp-test \
--funcall py-execute-block-or-clause-python3.2-dedicated-switch-commandp-test \
--funcall py-execute-def-python-commandp-test \
--funcall py-execute-def-python-switch-commandp-test \
--funcall py-execute-def-python-noswitch-commandp-test \
--funcall py-execute-def-python-dedicated-commandp-test \
--funcall py-execute-def-python-dedicated-switch-commandp-test \
--funcall py-execute-def-ipython-commandp-test \
--funcall py-execute-def-ipython-switch-commandp-test \
--funcall py-execute-def-ipython-noswitch-commandp-test \
--funcall py-execute-def-ipython-dedicated-commandp-test \
--funcall py-execute-def-ipython-dedicated-switch-commandp-test \
--funcall py-execute-def-python3-commandp-test \
--funcall py-execute-def-python3-switch-commandp-test \
--funcall py-execute-def-python3-noswitch-commandp-test \
--funcall py-execute-def-python3-dedicated-commandp-test \
--funcall py-execute-def-python3-dedicated-switch-commandp-test \
--funcall py-execute-def-python2-commandp-test \
--funcall py-execute-def-python2-switch-commandp-test \
--funcall py-execute-def-python2-noswitch-commandp-test \
--funcall py-execute-def-python2-dedicated-commandp-test \
--funcall py-execute-def-python2-dedicated-switch-commandp-test \
--funcall py-execute-def-python2.7-commandp-test \
--funcall py-execute-def-python2.7-switch-commandp-test \
--funcall py-execute-def-python2.7-noswitch-commandp-test \
--funcall py-execute-def-python2.7-dedicated-commandp-test \
--funcall py-execute-def-python2.7-dedicated-switch-commandp-test \
--funcall py-execute-def-jython-commandp-test \
--funcall py-execute-def-jython-switch-commandp-test \
--funcall py-execute-def-jython-noswitch-commandp-test \
--funcall py-execute-def-jython-dedicated-commandp-test \
--funcall py-execute-def-jython-dedicated-switch-commandp-test \
--funcall py-execute-def-python3.2-commandp-test \
--funcall py-execute-def-python3.2-switch-commandp-test \
--funcall py-execute-def-python3.2-noswitch-commandp-test \
--funcall py-execute-def-python3.2-dedicated-commandp-test \
--funcall py-execute-def-python3.2-dedicated-switch-commandp-test \
--funcall py-execute-class-python-commandp-test \
--funcall py-execute-class-python-switch-commandp-test \
--funcall py-execute-class-python-noswitch-commandp-test \
--funcall py-execute-class-python-dedicated-commandp-test \
--funcall py-execute-class-python-dedicated-switch-commandp-test \
--funcall py-execute-class-ipython-commandp-test \
--funcall py-execute-class-ipython-switch-commandp-test \
--funcall py-execute-class-ipython-noswitch-commandp-test \
--funcall py-execute-class-ipython-dedicated-commandp-test \
--funcall py-execute-class-ipython-dedicated-switch-commandp-test \
--funcall py-execute-class-python3-commandp-test \
--funcall py-execute-class-python3-switch-commandp-test \
--funcall py-execute-class-python3-noswitch-commandp-test \
--funcall py-execute-class-python3-dedicated-commandp-test \
--funcall py-execute-class-python3-dedicated-switch-commandp-test \
--funcall py-execute-class-python2-commandp-test \
--funcall py-execute-class-python2-switch-commandp-test \
--funcall py-execute-class-python2-noswitch-commandp-test \
--funcall py-execute-class-python2-dedicated-commandp-test \
--funcall py-execute-class-python2-dedicated-switch-commandp-test \
--funcall py-execute-class-python2.7-commandp-test \
--funcall py-execute-class-python2.7-switch-commandp-test \
--funcall py-execute-class-python2.7-noswitch-commandp-test \
--funcall py-execute-class-python2.7-dedicated-commandp-test \
--funcall py-execute-class-python2.7-dedicated-switch-commandp-test \
--funcall py-execute-class-jython-commandp-test \
--funcall py-execute-class-jython-switch-commandp-test \
--funcall py-execute-class-jython-noswitch-commandp-test \
--funcall py-execute-class-jython-dedicated-commandp-test \
--funcall py-execute-class-jython-dedicated-switch-commandp-test \
--funcall py-execute-class-python3.2-commandp-test \
--funcall py-execute-class-python3.2-switch-commandp-test \
--funcall py-execute-class-python3.2-noswitch-commandp-test \
--funcall py-execute-class-python3.2-dedicated-commandp-test \
--funcall py-execute-class-python3.2-dedicated-switch-commandp-test \
--funcall py-execute-region-python-commandp-test \
--funcall py-execute-region-python-switch-commandp-test \
--funcall py-execute-region-python-noswitch-commandp-test \
--funcall py-execute-region-python-dedicated-commandp-test \
--funcall py-execute-region-python-dedicated-switch-commandp-test \
--funcall py-execute-region-ipython-commandp-test \
--funcall py-execute-region-ipython-switch-commandp-test \
--funcall py-execute-region-ipython-noswitch-commandp-test \
--funcall py-execute-region-ipython-dedicated-commandp-test \
--funcall py-execute-region-ipython-dedicated-switch-commandp-test \
--funcall py-execute-region-python3-commandp-test \
--funcall py-execute-region-python3-switch-commandp-test \
--funcall py-execute-region-python3-noswitch-commandp-test \
--funcall py-execute-region-python3-dedicated-commandp-test \
--funcall py-execute-region-python3-dedicated-switch-commandp-test \
--funcall py-execute-region-python2-commandp-test \
--funcall py-execute-region-python2-switch-commandp-test \
--funcall py-execute-region-python2-noswitch-commandp-test \
--funcall py-execute-region-python2-dedicated-commandp-test \
--funcall py-execute-region-python2-dedicated-switch-commandp-test \
--funcall py-execute-region-python2.7-commandp-test \
--funcall py-execute-region-python2.7-switch-commandp-test \
--funcall py-execute-region-python2.7-noswitch-commandp-test \
--funcall py-execute-region-python2.7-dedicated-commandp-test \
--funcall py-execute-region-python2.7-dedicated-switch-commandp-test \
--funcall py-execute-region-jython-commandp-test \
--funcall py-execute-region-jython-switch-commandp-test \
--funcall py-execute-region-jython-noswitch-commandp-test \
--funcall py-execute-region-jython-dedicated-commandp-test \
--funcall py-execute-region-jython-dedicated-switch-commandp-test \
--funcall py-execute-region-python3.2-commandp-test \
--funcall py-execute-region-python3.2-switch-commandp-test \
--funcall py-execute-region-python3.2-noswitch-commandp-test \
--funcall py-execute-region-python3.2-dedicated-commandp-test \
--funcall py-execute-region-python3.2-dedicated-switch-commandp-test \
--funcall py-execute-buffer-python-commandp-test \
--funcall py-execute-buffer-python-switch-commandp-test \
--funcall py-execute-buffer-python-noswitch-commandp-test \
--funcall py-execute-buffer-python-dedicated-commandp-test \
--funcall py-execute-buffer-python-dedicated-switch-commandp-test \
--funcall py-execute-buffer-ipython-commandp-test \
--funcall py-execute-buffer-ipython-switch-commandp-test \
--funcall py-execute-buffer-ipython-noswitch-commandp-test \
--funcall py-execute-buffer-ipython-dedicated-commandp-test \
--funcall py-execute-buffer-ipython-dedicated-switch-commandp-test \
--funcall py-execute-buffer-python3-commandp-test \
--funcall py-execute-buffer-python3-switch-commandp-test \
--funcall py-execute-buffer-python3-noswitch-commandp-test \
--funcall py-execute-buffer-python3-dedicated-commandp-test \
--funcall py-execute-buffer-python3-dedicated-switch-commandp-test \
--funcall py-execute-buffer-python2-commandp-test \
--funcall py-execute-buffer-python2-switch-commandp-test \
--funcall py-execute-buffer-python2-noswitch-commandp-test \
--funcall py-execute-buffer-python2-dedicated-commandp-test \
--funcall py-execute-buffer-python2-dedicated-switch-commandp-test \
--funcall py-execute-buffer-python2.7-commandp-test \
--funcall py-execute-buffer-python2.7-switch-commandp-test \
--funcall py-execute-buffer-python2.7-noswitch-commandp-test \
--funcall py-execute-buffer-python2.7-dedicated-commandp-test \
--funcall py-execute-buffer-python2.7-dedicated-switch-commandp-test \
--funcall py-execute-buffer-jython-commandp-test \
--funcall py-execute-buffer-jython-switch-commandp-test \
--funcall py-execute-buffer-jython-noswitch-commandp-test \
--funcall py-execute-buffer-jython-dedicated-commandp-test \
--funcall py-execute-buffer-jython-dedicated-switch-commandp-test \
--funcall py-execute-buffer-python3.2-commandp-test \
--funcall py-execute-buffer-python3.2-switch-commandp-test \
--funcall py-execute-buffer-python3.2-noswitch-commandp-test \
--funcall py-execute-buffer-python3.2-dedicated-commandp-test \
--funcall py-execute-buffer-python3.2-dedicated-switch-commandp-test \
--funcall py-execute-expression-python-commandp-test \
--funcall py-execute-expression-python-switch-commandp-test \
--funcall py-execute-expression-python-noswitch-commandp-test \
--funcall py-execute-expression-python-dedicated-commandp-test \
--funcall py-execute-expression-python-dedicated-switch-commandp-test \
--funcall py-execute-expression-ipython-commandp-test \
--funcall py-execute-expression-ipython-switch-commandp-test \
--funcall py-execute-expression-ipython-noswitch-commandp-test \
--funcall py-execute-expression-ipython-dedicated-commandp-test \
--funcall py-execute-expression-ipython-dedicated-switch-commandp-test \
--funcall py-execute-expression-python3-commandp-test \
--funcall py-execute-expression-python3-switch-commandp-test \
--funcall py-execute-expression-python3-noswitch-commandp-test \
--funcall py-execute-expression-python3-dedicated-commandp-test \
--funcall py-execute-expression-python3-dedicated-switch-commandp-test \
--funcall py-execute-expression-python2-commandp-test \
--funcall py-execute-expression-python2-switch-commandp-test \
--funcall py-execute-expression-python2-noswitch-commandp-test \
--funcall py-execute-expression-python2-dedicated-commandp-test \
--funcall py-execute-expression-python2-dedicated-switch-commandp-test \
--funcall py-execute-expression-python2.7-commandp-test \
--funcall py-execute-expression-python2.7-switch-commandp-test \
--funcall py-execute-expression-python2.7-noswitch-commandp-test \
--funcall py-execute-expression-python2.7-dedicated-commandp-test \
--funcall py-execute-expression-python2.7-dedicated-switch-commandp-test \
--funcall py-execute-expression-jython-commandp-test \
--funcall py-execute-expression-jython-switch-commandp-test \
--funcall py-execute-expression-jython-noswitch-commandp-test \
--funcall py-execute-expression-jython-dedicated-commandp-test \
--funcall py-execute-expression-jython-dedicated-switch-commandp-test \
--funcall py-execute-expression-python3.2-commandp-test \
--funcall py-execute-expression-python3.2-switch-commandp-test \
--funcall py-execute-expression-python3.2-noswitch-commandp-test \
--funcall py-execute-expression-python3.2-dedicated-commandp-test \
--funcall py-execute-expression-python3.2-dedicated-switch-commandp-test \
--funcall py-execute-partial-expression-python-switch-commandp-test \
--funcall py-execute-partial-expression-python-commandp-test \
--funcall py-execute-partial-expression-python-noswitch-commandp-test \
--funcall py-execute-partial-expression-python-dedicated-commandp-test \
--funcall py-execute-partial-expression-python-dedicated-switch-commandp-test \
--funcall py-execute-partial-expression-ipython-commandp-test \
--funcall py-execute-partial-expression-ipython-switch-commandp-test \
--funcall py-execute-partial-expression-ipython-noswitch-commandp-test \
--funcall py-execute-partial-expression-ipython-dedicated-commandp-test \
--funcall py-execute-partial-expression-ipython-dedicated-switch-commandp-test \
--funcall py-execute-partial-expression-python3-commandp-test \
--funcall py-execute-partial-expression-python3-switch-commandp-test \
--funcall py-execute-partial-expression-python3-noswitch-commandp-test \
--funcall py-execute-partial-expression-python3-dedicated-commandp-test \
--funcall py-execute-partial-expression-python3-dedicated-switch-commandp-test \
--funcall py-execute-partial-expression-python2-commandp-test \
--funcall py-execute-partial-expression-python2-switch-commandp-test \
--funcall py-execute-partial-expression-python2-noswitch-commandp-test \
--funcall py-execute-partial-expression-python2-dedicated-commandp-test \
--funcall py-execute-partial-expression-python2-dedicated-switch-commandp-test \
--funcall py-execute-partial-expression-python2.7-commandp-test \
--funcall py-execute-partial-expression-python2.7-switch-commandp-test \
--funcall py-execute-partial-expression-python2.7-noswitch-commandp-test \
--funcall py-execute-partial-expression-python2.7-dedicated-commandp-test \
--funcall py-execute-partial-expression-python2.7-dedicated-switch-commandp-test \
--funcall py-execute-partial-expression-jython-commandp-test \
--funcall py-execute-partial-expression-jython-switch-commandp-test \
--funcall py-execute-partial-expression-jython-noswitch-commandp-test \
--funcall py-execute-partial-expression-jython-dedicated-commandp-test \
--funcall py-execute-partial-expression-jython-dedicated-switch-commandp-test \
--funcall py-execute-partial-expression-python3.2-commandp-test \
--funcall py-execute-partial-expression-python3.2-switch-commandp-test \
--funcall py-execute-partial-expression-python3.2-noswitch-commandp-test \
--funcall py-execute-partial-expression-python3.2-dedicated-commandp-test \
--funcall py-execute-partial-expression-python3.2-dedicated-switch-commandp-test \
--funcall py-mark-block-clause-misbehave-lp-949310-test \
--funcall py-mark-clause-misbehave-lp-949310-test \
--funcall py-mark-block-misbehave-lp-949310-test \
--funcall py-mark-partial-expression-commandp-test \
--funcall py-shebang-consider-ipython-lp-849293-test \
--funcall py-insert-super-python2-test \
--funcall py-shell-invoking-python-lp:835151-test \
--funcall py-shell-invoking-ipython-lp:835151-test \
--funcall py-shell-invoking-python3-lp:835151-test \
--funcall py-shell-invoking-python2-lp:835151-test \
--funcall py-shell-invoking-python2.7-lp:835151-test \
--funcall py-shell-invoking-jython-lp:835151-test \
--funcall py-smart-indent-eight-test \
--funcall wrong-type-argument-lp-901541-test \
--funcall indentation-bug-inside-docstrings-lp-899455-test \
--funcall py-shebang-ipython-env-lp-849293-test \
--funcall py-insert-super-python2-test \
--funcall nested-try-finally-test \
--funcall py-install-directory-path-test \
--funcall py-separator-char-test \
--funcall execute-indented-code-lp:828314-test \
--funcall switch-windows-on-execute-p-test \
--funcall py-execute-block-test \
--funcall py-shell-complete-test \
--funcall py-completion-at-point-test \
--funcall split-windows-on-execute-p-test \
--funcall tqs-list-error-test \
--funcall py-ipython-complete-lp:927136-test \
--funcall master-file-not-honored-lp:794850-test \
--funcall execute-buffer-ipython-fails-lp:928087-test \
--funcall py-electric-backspace-test \
--funcall UnicodeEncodeError-lp-550661-test \
--funcall UnicodeEncodeError-python3-test \
--funcall py-end-of-print-statement-test \
--funcall py-insert-super-python3-test \
--funcall py-menu-pyshell-test \

else

cat    <<EOF
usage: ${0##*/} EMACS_SOURCE_DIR

This script tests python-mode with non-installed Emacsen in a Bash.

It assumes being in directory "test" below python-mode.el and relies on source-code directories as delivered by bzr branch.

Edit \$EMACS_SOURCE_DIR to specify an Emacs or put "PATH-TO-EMACS-SOURCES" as shell argument.

To run tests with installed Emacs, load available test-files like "py-bug-numbered-tests.el" and do "M-x py-run-bug-numbered-tests". Alternatively you may edit variables making it point according to you installation.

EOF

fi

