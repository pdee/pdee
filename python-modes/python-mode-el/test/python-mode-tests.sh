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
EMACS_SOURCE_DIR="$HOME/emacs-20110426"

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

$EMACS -Q --batch --eval "(message (emacs-version))" --eval "(when (featurep 'python)(unload-feature 'python t))" --eval "(when (featurep 'python-mode)(unload-feature 'python-mode t))" --eval "(add-to-list 'load-path \"$PDIR/\")" --eval "(add-to-list 'load-path \"$TESTDIR/\")" --eval "(setq py-install-directory \"..\")" -load "$PYMACS" -load $CCCMDS -load $COMINT -load $SHELL -load $ANSICOLOR -load $CLMACS -load $BYTECOMP -load $CUSTOM -load $SKEL -load "../$HIGHL" -load "../$CEXEC" -load $PYTHONMODE -load "$PDIR/$TESTFILE" -load "$PDIR/$TESTFILE2" --eval "(quietly-read-abbrev-file (expand-file-name \"~/.abbrev_defs\"))" \
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
--funcall execute-indented-code-lp:828314-test \
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
--funcall indentation-bug-inside-docstrings-lp-899455-test \
--funcall another-indentation-bug-inside-docstrings-lp:900684-test \
--funcall incorrect-use-of-region-in-py-shift-left-lp:875951-test \
--funcall indentation-keyword-lp-885143-test \
--funcall fore-00007F-breaks-indentation-lp:328788-test \
--funcall indent-offset-not-guessed-when-loading-lp:902890-test \
--funcall from-__future__-import-absolute_import-mishighlighted-lp-907084-test \
--funcall automatic-indentation-is-broken-lp-889643-test \
--funcall chars-uU-preceding-triple-quoted-get-string-face-lp-909517-test \
--funcall wrong-type-argument-lp-901541-test \
--funcall py-pychecker-run-missing-lp-910783-test \
--funcall py-forward-into-nomenclature-lp-916818-test \
--funcall py-forward-into-nomenclature-jumps-over-CamelCased-words-lp:919540-test \
--funcall py-backward-into-nomenclature-caps-names-lp:919541-test \
--funcall execute-buffer-ipython-fails-lp:928087-test \
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
--funcall py-shebang-consider-ipython-lp-849293-test \
--funcall py-insert-super-python3-test \
--funcall UnicodeEncodeError-lp:550661-test \
--funcall py-shell-complete-test \
--funcall py-completion-at-point-test \
--funcall py-shebang-ipython-env-lp-849293-test \
--funcall master-file-not-honored-lp:794850-test \
--funcall py-insert-super-python2-test \
--funcall nested-try-finally-test \
--funcall py-electric-backspace-test \
--funcall UnicodeEncodeError-python3-test \
--funcall py-ipython-complete-lp:927136-test \
--funcall py-execute-block-test \
--funcall tqs-list-error-test \
--funcall py-end-of-print-statement-test \


else

cat    <<EOF
usage: ${0##*/} EMACS_SOURCE_DIR

This script tests python-mode with non-installed Emacsen in a Bash.

It assumes being in directory "test" below python-mode.el and relies on source-code directories as delivered by bzr branch.

Edit \$EMACS_SOURCE_DIR to specify an Emacs or put "PATH-TO-EMACS-SOURCES" as shell argument.

To run tests with installed Emacs, load available test-files like "py-bug-numbered-tests.el" and do "M-x py-run-bug-numbered-tests". Alternatively you may edit variables making it point according to you installation.

EOF

fi

