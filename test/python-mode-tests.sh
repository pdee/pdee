#!/bin/bash

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

# This script tests Emacs python-mode.

# Caveats:
#
# needs being started in `test' directory 
# optional shell argument PATH/TO/EMACS-SOURCE-DIRECTORY might be given
#
# If testing with emacs-24 please be aware of bug 11984 [0], for the
# time being the patch will need to be added manually.
#
# IPython 0.12 due to a bug in argparse requires a patch [1] to work.
#
# 0. http://debbugs.gnu.org/cgi/bugreport.cgi?bug=11984
# 1. http://bugs.python.org/issue13720

# Code:


# needs being in `test' directory
# PCOT=`pwd`
PCOT="."
# PDIR=".."
PDIR=$(cd ..; pwd)/
# the directory that this file is in.
# TESTDIR="$(dirname "$0")"
# PDIR="$TESTDIR/.."

# write PATH-TO-EMACS source code default directory here
EMACS_SOURCE_DIR=
if [ $1 ]; then
    echo "\$1: $1"
    EMACS_SOURCE_DIR=$1
elif [ -s "$HOME/emacs-23.4" ]; then
    EMACS_SOURCE_DIR="$HOME/emacs-23.4"
    else
cat    <<EOF
usage: ${0##*/} EMACS_SOURCE_DIR

This script tests python-mode with non-installed Emacsen in a Bash.

It assumes being in directory "test" below python-mode.el and relies on source-code directories as delivered by bzr branch.

Edit \$EMACS_SOURCE_DIR to specify an Emacs or put "PATH-TO-EMACS-SOURCES" as shell argument.

To run tests with installed Emacs, load available test-files like "py-bug-numbered-tests.el" and do "M-x py-run-bug-numbered-tests". Alternatively you may edit variables making it point according to you installation.

EOF
    
fi

echo "\$EMACS_SOURCE_DIR: $EMACS_SOURCE_DIR"

EMACS="$EMACS_SOURCE_DIR/src/emacs"

# python-mode file to load
if [ -s "../python-components-mode.el" ];
then
    PYTHONMODE="../python-components-mode.el"
elif
    [ -s "../python-mode.el" ];
then
    PYTHONMODE="../python-mode.el"
else
    cat    <<EOF
usage: ${0##*/} EMACS_SOURCE_DIR

This script tests python-mode with non-installed Emacsen in a Bash.

It assumes being in directory "test" below python-mode.el and relies on source-code directories as delivered by bzr branch.

Edit \$EMACS_SOURCE_DIR to specify an Emacs or put "PATH-TO-EMACS-SOURCES" as shell argument.

To run tests with installed Emacs, load available test-files like "py-bug-numbered-tests.el" and do "M-x py-run-bug-numbered-tests". Alternatively you may edit variables making it point according to you installation.

EOF

fi


HIGHL="highlight-indentation.el"
CLMACS="${EMACS_SOURCE_DIR}/lisp/emacs-lisp/cl-macs.el"
BYTECOMP="${EMACS_SOURCE_DIR}/lisp/emacs-lisp/bytecomp.el"
CUSTOM="${EMACS_SOURCE_DIR}/lisp/custom.el"
ANSICOLOR="${EMACS_SOURCE_DIR}/lisp/ansi-color.el"
COMINT="${EMACS_SOURCE_DIR}/lisp/comint.el"
CCCMDS="${EMACS_SOURCE_DIR}/lisp/progmodes/cc-cmds.el"
SKEL="${EMACS_SOURCE_DIR}/lisp/skeleton.el"
PYCO="$PDIR/completion/pycomplete.el"
SO="$PDIR/extensions/py-smart-operator.el"

# file holding the tests
TESTFILE="py-bug-numbered-tests.el"
TESTFILE2="python-mode-test.el"
TESTFILE3="python-extended-executes-test.el"
TESTFILE4="python-executes-test.el"
TESTFILE5="py-shell-completion-tests.el"
CEXEC="python-extended-executes.el"
PCOT="$TESTDIR"

# export PYTHONPATH="$PDIR/completion/:~/tmp/"

# python-mode file to load
if [ -s "$PDIR/python-components-mode.el" ];
then
    PYTHONMODE="$PDIR/python-components-mode.el"
else
    PYTHONMODE="$PDIR/python-mode.el"
fi

echo "\$PYMACS: $PYMACS"
echo "\$PYTHONMODE: $PYTHONMODE"
echo "\$PDIR/\$TESTFILE: $PDIR/$TESTFILE"
$EMACS -Q --batch --eval "(message (emacs-version))" --eval "(when (featurep 'python)(unload-feature 'python t))" --eval "(when (featurep 'python-mode)(unload-feature 'python-mode t))" --eval "(add-to-list 'load-path \"$PDIR/\")" --eval "(add-to-list 'load-path \"$TESTDIR/\")" --eval "(setq py-install-directory \"$PDIR\"))" --eval "(message \"py-install-directory: %s\" py-install-directory)" --eval "(setq py-load-pymacs-p t)" -load $CCCMDS -load $COMINT -load $ANSICOLOR -load $CLMACS -load $BYTECOMP -load $CUSTOM -load $SKEL -load $PYCO -load $SO -load $PYTHONMODE  --eval "(message \"py-temp-directory: %s\" py-temp-directory)" -load $PCOT$TESTFILE -load $PCOT$TESTFILE2 -load $PCOT$TESTFILE3 -load $PCOT$TESTFILE4 -load $PCOT$TESTFILE5 --eval "(when (file-exists-p \"~/.abbrev_defs\") (quietly-read-abbrev-file (expand-file-name \"~/.abbrev_defs\")))" \
\
-eval "(assert (functionp 'word-at-point) nil \"new completion bug, lp:1034656, word-at-point not known\")" \
\
-eval "(assert (commandp 'pylint-flymake-mode) nil \"pylint-flymake-mode not detected as command\")" \
-eval "(assert (commandp 'pyflakes-flymake-mode) nil \"pyflakes-flymake-mode not detected as command\")" \
-eval "(assert (commandp 'pychecker-flymake-mode) nil \"pychecker-flymake-mode not detected as command\")" \
-eval "(assert (commandp 'pep8-flymake-mode) nil \"pep8-flymake-mode not detected as command\")" \
-eval "(assert (commandp 'pyflakespep8-flymake-mode) nil \"pyflakespep8-flymake-mode not detected as command\")" \
-eval "(assert (commandp 'py-pylint-doku) nil \"py-pylint-doku not detected as command\")" \
-eval "(assert (commandp 'py-pyflakes-run) nil \"py-pyflakes-run not detected as command\")" \
-eval "(assert (commandp 'py-pyflakespep8-run) nil \"py-pyflakespep8-run not detected as command\")" \
-eval "(assert (commandp 'py-pyflakespep8-help) nil \"py-pyflakespep8-help not detected as command\")" \
-eval "(assert (commandp 'py-guess-pdb-path) nil \"py-guess-pdb-path not detected as command\")" \
-eval "(assert (commandp 'highlight-indentation-on) nil \"highlight-indentation-on not detected as command\")" \
-eval "(assert (commandp 'highlight-indentation-off) nil \"highlight-indentation-off not detected as command\")" \
-eval "(assert (commandp 'highlight-indentation) nil \"highlight-indentation not detected as command\")" \
-eval "(assert (commandp 'py-insert-default-shebang) nil \"py-insert-default-shebang not detected as command\")" \
-eval "(assert (commandp 'py-electric-comment) nil \"py-electric-comment not detected as command\")" \
-eval "(assert (commandp 'py-electric-colon) nil \"py-electric-colon not detected as command\")" \
-eval "(assert (commandp 'py-electric-backspace) nil \"py-electric-backspace not detected as command\")" \
-eval "(assert (commandp 'py-electric-delete) nil \"py-electric-delete not detected as command\")" \
-eval "(assert (commandp 'py-indent-line-outmost) nil \"py-indent-line-outmost not detected as command\")" \
-eval "(assert (commandp 'py-indent-line) nil \"py-indent-line not detected as command\")" \
-eval "(assert (commandp 'py-newline-and-indent) nil \"py-newline-and-indent not detected as command\")" \
-eval "(assert (commandp 'py-newline-and-dedent) nil \"py-newline-and-dedent not detected as command\")" \
-eval "(assert (commandp 'py-toggle-indent-tabs-mode) nil \"py-toggle-indent-tabs-mode not detected as command\")" \
-eval "(assert (commandp 'py-indent-tabs-mode) nil \"indent-tabs-mode not detected as command\")" \
-eval "(assert (commandp 'py-indent-tabs-mode-on) nil \"indent-tabs-mode-on not detected as command\")" \
-eval "(assert (commandp 'py-indent-tabs-mode-off) nil \"indent-tabs-mode-off not detected as command\")" \
-eval "(assert (commandp 'py-guess-indent-offset) nil \"py-guess-indent-offset not detected as command\")" \
-eval "(assert (commandp 'py-narrow-to-defun) nil \"py-narrow-to-defun not detected as command\")" \
-eval "(assert (commandp 'py-shift-left) nil \"py-shift-left not detected as command\")" \
-eval "(assert (commandp 'py-shift-right) nil \"py-shift-right not detected as command\")" \
-eval "(assert (commandp 'py-shift-paragraph-right) nil \"py-shift-paragraph-right not detected as command\")" \
-eval "(assert (commandp 'py-shift-paragraph-left) nil \"py-shift-paragraph-left not detected as command\")" \
-eval "(assert (commandp 'py-shift-block-right) nil \"py-shift-block-right not detected as command\")" \
-eval "(assert (commandp 'py-shift-block-left) nil \"py-shift-block-left not detected as command\")" \
-eval "(assert (commandp 'py-shift-clause-right) nil \"py-shift-clause-right not detected as command\")" \
-eval "(assert (commandp 'py-shift-clause-left) nil \"py-shift-clause-left not detected as command\")" \
-eval "(assert (commandp 'py-shift-def-right) nil \"py-shift-def-right not detected as command\")" \
-eval "(assert (commandp 'py-shift-def-left) nil \"py-shift-def-left not detected as command\")" \
-eval "(assert (commandp 'py-shift-class-right) nil \"py-shift-class-right not detected as command\")" \
-eval "(assert (commandp 'py-shift-class-left) nil \"py-shift-class-left not detected as command\")" \
-eval "(assert (commandp 'py-shift-line-right) nil \"py-shift-line-right not detected as command\")" \
-eval "(assert (commandp 'py-shift-line-left) nil \"py-shift-line-left not detected as command\")" \
-eval "(assert (commandp 'py-shift-statement-right) nil \"py-shift-statement-right not detected as command\")" \
-eval "(assert (commandp 'py-shift-statement-left) nil \"py-shift-statement-left not detected as command\")" \
-eval "(assert (commandp 'py-indent-region) nil \"py-indent-region not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-paragraph-position) nil \"py-beginning-of-paragraph-position not detected as command\")" \
-eval "(assert (commandp 'py-end-of-paragraph-position) nil \"py-end-of-paragraph-position not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-block-position) nil \"py-beginning-of-block-position not detected as command\")" \
-eval "(assert (commandp 'py-end-of-block-position) nil \"py-end-of-block-position not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-clause-position) nil \"py-beginning-of-clause-position not detected as command\")" \
-eval "(assert (commandp 'py-end-of-clause-position) nil \"py-end-of-clause-position not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-block-or-clause-position) nil \"py-beginning-of-block-or-clause-position not detected as command\")" \
-eval "(assert (commandp 'py-end-of-block-or-clause-position) nil \"py-end-of-block-or-clause-position not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-def-position) nil \"py-beginning-of-def-position not detected as command\")" \
-eval "(assert (commandp 'py-end-of-def-position) nil \"py-end-of-def-position not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-class-position) nil \"py-beginning-of-class-position not detected as command\")" \
-eval "(assert (commandp 'py-end-of-class-position) nil \"py-end-of-class-position not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-def-or-class-position) nil \"py-beginning-of-def-or-class-position not detected as command\")" \
-eval "(assert (commandp 'py-end-of-def-or-class-position) nil \"py-end-of-def-or-class-position not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-line-position) nil \"py-beginning-of-line-position not detected as command\")" \
-eval "(assert (commandp 'py-end-of-line-position) nil \"py-end-of-line-position not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-statement-position) nil \"py-beginning-of-statement-position not detected as command\")" \
-eval "(assert (commandp 'py-end-of-statement-position) nil \"py-end-of-statement-position not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-expression-position) nil \"py-beginning-of-expression-position not detected as command\")" \
-eval "(assert (commandp 'py-end-of-expression-position) nil \"py-end-of-expression-position not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-partial-expression-position) nil \"py-beginning-of-partial-expression-position not detected as command\")" \
-eval "(assert (commandp 'py-end-of-partial-expression-position) nil \"py-end-of-partial-expression-position not detected as command\")" \
-eval "(assert (commandp 'py-bounds-of-statement) nil \"py-bounds-of-statement not detected as command\")" \
-eval "(assert (commandp 'py-bounds-of-block) nil \"py-bounds-of-block not detected as command\")" \
-eval "(assert (commandp 'py-bounds-of-clause) nil \"py-bounds-of-clause not detected as command\")" \
-eval "(assert (commandp 'py-bounds-of-block-or-clause) nil \"py-bounds-of-block-or-clause not detected as command\")" \
-eval "(assert (commandp 'py-bounds-of-def) nil \"py-bounds-of-def not detected as command\")" \
-eval "(assert (commandp 'py-bounds-of-class) nil \"py-bounds-of-class not detected as command\")" \
-eval "(assert (commandp 'py-bounds-of-region) nil \"py-bounds-of-region not detected as command\")" \
-eval "(assert (commandp 'py-bounds-of-buffer) nil \"py-bounds-of-buffer not detected as command\")" \
-eval "(assert (commandp 'py-bounds-of-expression) nil \"py-bounds-of-expression not detected as command\")" \
-eval "(assert (commandp 'py-bounds-of-partial-expression) nil \"py-bounds-of-partial-expression not detected as command\")" \
-eval "(assert (commandp 'py-bounds-of-declarations) nil \"py-bounds-of-declarations not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-declarations) nil \"py-beginning-of-declarations not detected as command\")" \
-eval "(assert (commandp 'py-end-of-declarations) nil \"py-end-of-declarations not detected as command\")" \
-eval "(assert (commandp 'py-declarations) nil \"py-declarations not detected as command\")" \
-eval "(assert (commandp 'py-kill-declarations) nil \"py-kill-declarations not detected as command\")" \
-eval "(assert (commandp 'py-bounds-of-statements) nil \"py-bounds-of-statements not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-statements) nil \"py-beginning-of-statements not detected as command\")" \
-eval "(assert (commandp 'py-end-of-statements) nil \"py-end-of-statements not detected as command\")" \
-eval "(assert (commandp 'py-statements) nil \"py-statements not detected as command\")" \
-eval "(assert (commandp 'py-kill-statements) nil \"py-kill-statements not detected as command\")" \
-eval "(assert (commandp 'py-comment-region) nil \"py-comment-region not detected as command\")" \
-eval "(assert (commandp 'py-fill-paragraph) nil \"py-fill-paragraph not detected as command\")" \
-eval "(assert (commandp 'py-insert-super) nil \"py-insert-super not detected as command\")" \
-eval "(assert (commandp 'py-compute-indentation) nil \"py-compute-indentation not detected as command\")" \
-eval "(assert (commandp 'py-continuation-offset) nil \"py-continuation-offset not detected as command\")" \
-eval "(assert (commandp 'py-indentation-of-statement) nil \"py-indentation-of-statement not detected as command\")" \
-eval "(assert (commandp 'py-list-beginning-position) nil \"py-list-beginning-position not detected as command\")" \
-eval "(assert (commandp 'py-end-of-list-position) nil \"py-end-of-list-position not detected as command\")" \
-eval "(assert (commandp 'py-in-triplequoted-string-p) nil \"py-in-triplequoted-string-p not detected as command\")" \
-eval "(assert (commandp 'py-in-string-p) nil \"py-in-string-p not detected as command\")" \
-eval "(assert (commandp 'py-in-statement-p) nil \"py-in-statement-p not detected as command\")" \
-eval "(assert (commandp 'py-statement-opens-block-p) nil \"py-statement-opens-block-p not detected as command\")" \
-eval "(assert (commandp 'py-statement-opens-clause-p) nil \"py-statement-opens-clause-p not detected as command\")" \
-eval "(assert (commandp 'py-statement-opens-block-or-clause-p) nil \"py-statement-opens-block-or-clause-p not detected as command\")" \
-eval "(assert (commandp 'py-statement-opens-class-p) nil \"py-statement-opens-class-p not detected as command\")" \
-eval "(assert (commandp 'py-statement-opens-def-p) nil \"py-statement-opens-def-p not detected as command\")" \
-eval "(assert (commandp 'py-statement-opens-def-or-class-p) nil \"py-statement-opens-def-or-class-p not detected as command\")" \
-eval "(assert (commandp 'py-current-defun) nil \"py-current-defun not detected as command\")" \
-eval "(assert (commandp 'py-sort-imports) nil \"py-sort-imports not detected as command\")" \
-eval "(assert (commandp 'py-which-function) nil \"py-which-function not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-block) nil \"py-beginning-of-block not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-if-block) nil \"py-beginning-of-if-block not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-try-block) nil \"py-beginning-of-try-block not detected as command\")" \
-eval "(assert (commandp 'py-end-of-block) nil \"py-end-of-block not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-block-or-clause) nil \"py-beginning-of-block-or-clause not detected as command\")" \
-eval "(assert (commandp 'py-end-of-block-or-clause) nil \"py-end-of-block-or-clause not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-class) nil \"py-beginning-of-class not detected as command\")" \
-eval "(assert (commandp 'py-end-of-class) nil \"py-end-of-class not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-clause) nil \"py-beginning-of-clause not detected as command\")" \
-eval "(assert (commandp 'py-end-of-clause) nil \"py-end-of-clause not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-def) nil \"py-beginning-of-def not detected as command\")" \
-eval "(assert (commandp 'py-end-of-def) nil \"py-end-of-def not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-def-or-class) nil \"py-beginning-of-def-or-class not detected as command\")" \
-eval "(assert (commandp 'py-end-of-def-or-class) nil \"py-end-of-def-or-class not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-expression) nil \"py-beginning-of-expression not detected as command\")" \
-eval "(assert (commandp 'py-end-of-expression) nil \"py-end-of-expression not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-partial-expression) nil \"py-beginning-of-partial-expression not detected as command\")" \
-eval "(assert (commandp 'py-end-of-partial-expression) nil \"py-end-of-partial-expression not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-statement) nil \"py-beginning-of-statement not detected as command\")" \
-eval "(assert (commandp 'py-end-of-statement) nil \"py-end-of-statement not detected as command\")" \
-eval "(assert (commandp 'py-goto-statement-below) nil \"py-goto-statement-below not detected as command\")" \
-eval "(assert (commandp 'py-mark-paragraph) nil \"py-mark-paragraph not detected as command\")" \
-eval "(assert (commandp 'py-mark-block) nil \"py-mark-block not detected as command\")" \
-eval "(assert (commandp 'py-mark-clause) nil \"py-mark-clause not detected as command\")" \
-eval "(assert (commandp 'py-mark-block-or-clause) nil \"py-mark-block-or-clause not detected as command\")" \
-eval "(assert (commandp 'py-mark-def) nil \"py-mark-def not detected as command\")" \
-eval "(assert (commandp 'py-mark-class) nil \"py-mark-class not detected as command\")" \
-eval "(assert (commandp 'py-mark-def-or-class) nil \"py-mark-def-or-class not detected as command\")" \
-eval "(assert (commandp 'py-mark-line) nil \"py-mark-line not detected as command\")" \
-eval "(assert (commandp 'py-mark-statement) nil \"py-mark-statement not detected as command\")" \
-eval "(assert (commandp 'py-mark-expression) nil \"py-mark-expression not detected as command\")" \
-eval "(assert (commandp 'py-mark-partial-expression) nil \"py-mark-partial-expression not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-decorator) nil \"py-beginning-of-decorator not detected as command\")" \
-eval "(assert (commandp 'py-end-of-decorator) nil \"py-end-of-decorator not detected as command\")" \
-eval "(assert (commandp 'py-copy-expression) nil \"py-copy-expression not detected as command\")" \
-eval "(assert (commandp 'py-copy-partial-expression) nil \"py-copy-partial-expression not detected as command\")" \
-eval "(assert (commandp 'py-copy-statement) nil \"py-copy-statement not detected as command\")" \
-eval "(assert (commandp 'py-copy-block) nil \"py-copy-block not detected as command\")" \
-eval "(assert (commandp 'py-copy-block-or-clause) nil \"py-copy-block-or-clause not detected as command\")" \
-eval "(assert (commandp 'py-copy-def) nil \"py-copy-def not detected as command\")" \
-eval "(assert (commandp 'py-copy-def-or-class) nil \"py-copy-def-or-class not detected as command\")" \
-eval "(assert (commandp 'py-copy-class) nil \"py-copy-class not detected as command\")" \
-eval "(assert (commandp 'py-copy-clause) nil \"py-copy-clause not detected as command\")" \
-eval "(assert (commandp 'py-kill-expression) nil \"py-kill-expression not detected as command\")" \
-eval "(assert (commandp 'py-kill-partial-expression) nil \"py-kill-partial-expression not detected as command\")" \
-eval "(assert (commandp 'py-kill-statement) nil \"py-kill-statement not detected as command\")" \
-eval "(assert (commandp 'py-kill-block) nil \"py-kill-block not detected as command\")" \
-eval "(assert (commandp 'py-kill-block-or-clause) nil \"py-kill-block-or-clause not detected as command\")" \
-eval "(assert (commandp 'py-kill-def-or-class) nil \"py-kill-def-or-class not detected as command\")" \
-eval "(assert (commandp 'py-kill-class) nil \"py-kill-class not detected as command\")" \
-eval "(assert (commandp 'py-kill-def) nil \"py-kill-def not detected as command\")" \
-eval "(assert (commandp 'py-kill-clause) nil \"py-kill-clause not detected as command\")" \
-eval "(assert (commandp 'py-forward-line) nil \"py-forward-line not detected as command\")" \
-eval "(assert (commandp 'py-leave-comment-or-string-backward) nil \"py-leave-comment-or-string-backward not detected as command\")" \
-eval "(assert (commandp 'py-beginning-of-list-pps) nil \"py-beginning-of-list-pps not detected as command\")" \
-eval "(assert (commandp 'py-down-block-lc) nil \"py-down-block-lc not detected as command\")" \
-eval "(assert (commandp 'py-down-clause-lc) nil \"py-down-clause-lc not detected as command\")" \
-eval "(assert (commandp 'py-down-def-lc) nil \"py-down-def-lc not detected as command\")" \
-eval "(assert (commandp 'py-down-class-lc) nil \"py-down-class-lc not detected as command\")" \
-eval "(assert (commandp 'py-down-statement-lc) nil \"py-down-statement-lc not detected as command\")" \
-eval "(assert (commandp 'py-down-statement) nil \"py-down-statement not detected as command\")" \
-eval "(assert (commandp 'py-down-block) nil \"py-down-block not detected as command\")" \
-eval "(assert (commandp 'py-down-clause) nil \"py-down-clause not detected as command\")" \
-eval "(assert (commandp 'py-down-block-or-clause) nil \"py-down-block-or-clause not detected as command\")" \
-eval "(assert (commandp 'py-down-def) nil \"py-down-def not detected as command\")" \
-eval "(assert (commandp 'py-down-class) nil \"py-down-class not detected as command\")" \
-eval "(assert (commandp 'py-down-def-or-class) nil \"py-down-def-or-class not detected as command\")" \
-eval "(assert (commandp 'py-forward-into-nomenclature) nil \"py-forward-into-nomenclature not detected as command\")" \
-eval "(assert (commandp 'py-backward-into-nomenclature) nil \"py-backward-into-nomenclature not detected as command\")" \
-eval "(assert (commandp 'match-paren) nil \"match-paren not detected as command\")" \
-eval "(assert (commandp 'py-toggle-execute-keep-temporary-file-p) nil \"py-toggle-execute-keep-temporary-file-p not detected as command\")" \
-eval "(assert (commandp 'py-guess-default-python) nil \"py-guess-default-python not detected as command\")" \
-eval "(assert (commandp 'py-set-ipython-completion-command-string) nil \"py-set-ipython-completion-command-string not detected as command\")" \
-eval "(assert (commandp 'py-shell-dedicated) nil \"py-shell-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-shell) nil \"py-shell not detected as command\")" \
-eval "(assert (commandp 'python) nil \"python not detected as command\")" \
-eval "(assert (commandp 'ipython) nil \"ipython not detected as command\")" \
-eval "(assert (commandp 'python3) nil \"python3 not detected as command\")" \
-eval "(assert (commandp 'python2) nil \"python2 not detected as command\")" \
-eval "(assert (commandp 'python2.7) nil \"python2.7 not detected as command\")" \
-eval "(assert (commandp 'python-dedicated) nil \"python-dedicated not detected as command\")" \
-eval "(assert (commandp 'ipython-dedicated) nil \"ipython-dedicated not detected as command\")" \
-eval "(assert (commandp 'python3-dedicated) nil \"python3-dedicated not detected as command\")" \
-eval "(assert (commandp 'python2-dedicated) nil \"python2-dedicated not detected as command\")" \
-eval "(assert (commandp 'python2.7-dedicated) nil \"python2.7-dedicated not detected as command\")" \
-eval "(assert (commandp 'python-switch) nil \"python-switch not detected as command\")" \
-eval "(assert (commandp 'ipython-switch) nil \"ipython-switch not detected as command\")" \
-eval "(assert (commandp 'python3-switch) nil \"python3-switch not detected as command\")" \
-eval "(assert (commandp 'python2-switch) nil \"python2-switch not detected as command\")" \
-eval "(assert (commandp 'python2.7-switch) nil \"python2.7-switch not detected as command\")" \
-eval "(assert (commandp 'jython-switch) nil \"jython-switch not detected as command\")" \
-eval "(assert (commandp 'python-no-switch) nil \"python-no-switch not detected as command\")" \
-eval "(assert (commandp 'ipython-no-switch) nil \"ipython-no-switch not detected as command\")" \
-eval "(assert (commandp 'python3-no-switch) nil \"python3-no-switch not detected as command\")" \
-eval "(assert (commandp 'python2-no-switch) nil \"python2-no-switch not detected as command\")" \
-eval "(assert (commandp 'python2.7-no-switch) nil \"python2.7-no-switch not detected as command\")" \
-eval "(assert (commandp 'jython-no-switch) nil \"jython-no-switch not detected as command\")" \
-eval "(assert (commandp 'python-switch-dedicated) nil \"python-switch-dedicated not detected as command\")" \
-eval "(assert (commandp 'ipython-switch-dedicated) nil \"ipython-switch-dedicated not detected as command\")" \
-eval "(assert (commandp 'python3-switch-dedicated) nil \"python3-switch-dedicated not detected as command\")" \
-eval "(assert (commandp 'python2-switch-dedicated) nil \"python2-switch-dedicated not detected as command\")" \
-eval "(assert (commandp 'python2.7-switch-dedicated) nil \"python2.7-switch-dedicated not detected as command\")" \
-eval "(assert (commandp 'jython-switch-dedicated) nil \"jython-switch-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-which-execute-file-command) nil \"py-which-execute-file-command not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-no-switch) nil \"py-execute-region-no-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-switch) nil \"py-execute-region-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-region) nil \"py-execute-region not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-default) nil \"py-execute-region-default not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-dedicated) nil \"py-execute-region-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-default-dedicated) nil \"py-execute-region-default-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-string) nil \"py-execute-string not detected as command\")" \
-eval "(assert (commandp 'py-execute-string-dedicated) nil \"py-execute-string-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-fetch-py-master-file) nil \"py-fetch-py-master-file not detected as command\")" \
-eval "(assert (commandp 'py-execute-import-or-reload) nil \"py-execute-import-or-reload not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-dedicated) nil \"py-execute-buffer-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-switch) nil \"py-execute-buffer-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-dedicated-switch) nil \"py-execute-buffer-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer) nil \"py-execute-buffer not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-no-switch) nil \"py-execute-buffer-no-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-defun) nil \"py-execute-defun not detected as command\")" \
-eval "(assert (commandp 'py-process-file) nil \"py-process-file not detected as command\")" \
-eval "(assert (commandp 'py-exec-execfile-region) nil \"py-exec-execfile-region not detected as command\")" \
-eval "(assert (commandp 'py-exec-execfile) nil \"py-exec-execfile not detected as command\")" \
-eval "(assert (commandp 'py-execute-block) nil \"py-execute-block not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause) nil \"py-execute-block-or-clause not detected as command\")" \
-eval "(assert (commandp 'py-execute-class) nil \"py-execute-class not detected as command\")" \
-eval "(assert (commandp 'py-execute-def) nil \"py-execute-def not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-or-class) nil \"py-execute-def-or-class not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression) nil \"py-execute-expression not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression) nil \"py-execute-partial-expression not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement) nil \"py-execute-statement not detected as command\")" \
-eval "(assert (commandp 'py-execute-file) nil \"py-execute-file not detected as command\")" \
-eval "(assert (commandp 'py-down-exception) nil \"py-down-exception not detected as command\")" \
-eval "(assert (commandp 'py-up-exception) nil \"py-up-exception not detected as command\")" \
-eval "(assert (commandp 'py-output-buffer-filter) nil \"py-output-buffer-filter not detected as command\")" \
-eval "(assert (commandp 'py-send-string) nil \"py-send-string not detected as command\")" \
-eval "(assert (commandp 'py-pdbtrack-toggle-stack-tracking) nil \"py-pdbtrack-toggle-stack-tracking not detected as command\")" \
-eval "(assert (commandp 'turn-on-pdbtrack) nil \"turn-on-pdbtrack not detected as command\")" \
-eval "(assert (commandp 'turn-off-pdbtrack) nil \"turn-off-pdbtrack not detected as command\")" \
-eval "(assert (commandp 'py-fetch-docu) nil \"py-fetch-docu not detected as command\")" \
-eval "(assert (commandp 'py-find-imports) nil \"py-find-imports not detected as command\")" \
-eval "(assert (commandp 'python-find-imports) nil \"python-find-imports not detected as command\")" \
-eval "(assert (commandp 'py-describe-symbol) nil \"py-describe-symbol not detected as command\")" \
-eval "(assert (commandp 'py-describe-mode) nil \"py-describe-mode not detected as command\")" \
-eval "(assert (commandp 'py-find-function) nil \"py-find-function not detected as command\")" \
-eval "(assert (commandp 'py-update-imports) nil \"py-update-imports not detected as command\")" \
-eval "(assert (commandp 'py-indent-forward-line) nil \"py-indent-forward-line not detected as command\")" \
-eval "(assert (commandp 'py-dedent-forward-line) nil \"py-dedent-forward-line not detected as command\")" \
-eval "(assert (commandp 'py-dedent) nil \"py-dedent not detected as command\")" \
-eval "(assert (commandp 'py-close-def) nil \"py-close-def not detected as command\")" \
-eval "(assert (commandp 'py-close-class) nil \"py-close-class not detected as command\")" \
-eval "(assert (commandp 'py-close-clause) nil \"py-close-clause not detected as command\")" \
-eval "(assert (commandp 'py-close-block) nil \"py-close-block not detected as command\")" \
-eval "(assert (commandp 'py-class-at-point) nil \"py-class-at-point not detected as command\")" \
-eval "(assert (commandp 'py-match-paren) nil \"py-match-paren not detected as command\")" \
-eval "(assert (commandp 'eva) nil \"eva not detected as command\")" \
-eval "(assert (commandp 'pst-here) nil \"pst-here not detected as command\")" \
-eval "(assert (commandp 'py-printform-insert) nil \"py-printform-insert not detected as command\")" \
-eval "(assert (commandp 'py-line-to-printform-python2) nil \"py-line-to-printform-python2 not detected as command\")" \
-eval "(assert (commandp 'py-switch-imenu-index-function) nil \"py-switch-imenu-index-function not detected as command\")" \
-eval "(assert (commandp 'py-completion-at-point) nil \"py-completion-at-point not detected as command\")" \
-eval "(assert (commandp 'py-choose-shell-by-shebang) nil \"py-choose-shell-by-shebang not detected as command\")" \
-eval "(assert (commandp 'py-which-python) nil \"py-which-python not detected as command\")" \
-eval "(assert (commandp 'py-python-current-environment) nil \"py-python-current-environment not detected as command\")" \
-eval "(assert (commandp 'py-switch-shell) nil \"py-switch-shell not detected as command\")" \
-eval "(assert (commandp 'py-choose-shell) nil \"py-choose-shell not detected as command\")" \
-eval "(assert (commandp 'py-toggle-smart-indentation) nil \"py-toggle-smart-indentation not detected as command\")" \
-eval "(assert (commandp 'py-smart-indentation-on) nil \"py-smart-indentation-on not detected as command\")" \
-eval "(assert (commandp 'py-smart-indentation-off) nil \"py-smart-indentation-off not detected as command\")" \
-eval "(assert (commandp 'py-toggle-split-windows-on-execute) nil \"py-toggle-split-windows-on-execute not detected as command\")" \
-eval "(assert (commandp 'py-split-windows-on-execute-on) nil \"py-split-windows-on-execute-on not detected as command\")" \
-eval "(assert (commandp 'py-split-windows-on-execute-off) nil \"py-split-windows-on-execute-off not detected as command\")" \
-eval "(assert (commandp 'py-toggle-shell-switch-buffers-on-execute) nil \"py-toggle-shell-switch-buffers-on-execute not detected as command\")" \
-eval "(assert (commandp 'py-shell-switch-buffers-on-execute-on) nil \"py-shell-switch-buffers-on-execute-on not detected as command\")" \
-eval "(assert (commandp 'py-shell-switch-buffers-on-execute-off) nil \"py-shell-switch-buffers-on-execute-off not detected as command\")" \
-eval "(assert (commandp 'py-install-directory-check) nil \"py-install-directory-check not detected as command\")" \
-eval "(assert (commandp 'py-guess-py-install-directory) nil \"py-guess-py-install-directory not detected as command\")" \
-eval "(assert (commandp 'py-set-load-path) nil \"py-set-load-path not detected as command\")" \
-eval "(assert (commandp 'py-def-or-class-beginning-position) nil \"py-def-or-class-beginning-position not detected as command\")" \
-eval "(assert (commandp 'py-def-or-class-end-position) nil \"py-def-or-class-end-position not detected as command\")" \
-eval "(assert (commandp 'py-statement-beginning-position) nil \"py-statement-beginning-position not detected as command\")" \
-eval "(assert (commandp 'py-statement-end-position) nil \"py-statement-end-position not detected as command\")" \
-eval "(assert (commandp 'py-current-indentation) nil \"py-current-indentation not detected as command\")" \
-eval "(assert (commandp 'py-version) nil \"py-version not detected as command\")" \
-eval "(assert (commandp 'run-python) nil \"run-python not detected as command\")" \
-eval "(assert (commandp 'py-send-region) nil \"py-send-region not detected as command\")" \
-eval "(assert (commandp 'py-send-buffer) nil \"py-send-buffer not detected as command\")" \
-eval "(assert (commandp 'py-switch-to-python) nil \"py-switch-to-python not detected as command\")" \
-eval "(assert (commandp 'py-send-region-and-go) nil \"py-send-region-and-go not detected as command\")" \
-eval "(assert (commandp 'py-load-file) nil \"py-load-file not detected as command\")" \
-eval "(assert (commandp 'py-shell-complete) nil \"py-shell-complete not detected as command\")" \
-eval "(assert (commandp 'ipython-complete) nil \"ipython-complete not detected as command\")" \
-eval "(assert (commandp 'py-pychecker-run) nil \"py-pychecker-run not detected as command\")" \
-eval "(assert (commandp 'virtualenv-current) nil \"virtualenv-current not detected as command\")" \
-eval "(assert (commandp 'virtualenv-activate) nil \"virtualenv-activate not detected as command\")" \
-eval "(assert (commandp 'virtualenv-deactivate) nil \"virtualenv-deactivate not detected as command\")" \
-eval "(assert (commandp 'virtualenv-workon) nil \"virtualenv-workon not detected as command\")" \
-eval "(assert (commandp 'py-toggle-local-default-use) nil \"py-toggle-local-default-use not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-python) nil \"py-execute-statement-python not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-python-switch) nil \"py-execute-statement-python-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-python-noswitch) nil \"py-execute-statement-python-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-python-dedicated) nil \"py-execute-statement-python-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-python-dedicated-switch) nil \"py-execute-statement-python-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-ipython) nil \"py-execute-statement-ipython not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-ipython-switch) nil \"py-execute-statement-ipython-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-ipython-noswitch) nil \"py-execute-statement-ipython-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-ipython-dedicated) nil \"py-execute-statement-ipython-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-ipython-dedicated-switch) nil \"py-execute-statement-ipython-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-python3) nil \"py-execute-statement-python3 not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-python3-switch) nil \"py-execute-statement-python3-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-python3-noswitch) nil \"py-execute-statement-python3-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-python3-dedicated) nil \"py-execute-statement-python3-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-python3-dedicated-switch) nil \"py-execute-statement-python3-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-python2) nil \"py-execute-statement-python2 not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-python2-switch) nil \"py-execute-statement-python2-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-python2-noswitch) nil \"py-execute-statement-python2-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-python2-dedicated) nil \"py-execute-statement-python2-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-python2-dedicated-switch) nil \"py-execute-statement-python2-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-python2.7) nil \"py-execute-statement-python2.7 not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-python2.7-switch) nil \"py-execute-statement-python2.7-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-python2.7-noswitch) nil \"py-execute-statement-python2.7-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-python2.7-dedicated) nil \"py-execute-statement-python2.7-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-python2.7-dedicated-switch) nil \"py-execute-statement-python2.7-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-jython) nil \"py-execute-statement-jython not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-jython-switch) nil \"py-execute-statement-jython-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-jython-noswitch) nil \"py-execute-statement-jython-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-jython-dedicated) nil \"py-execute-statement-jython-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-statement-jython-dedicated-switch) nil \"py-execute-statement-jython-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-python) nil \"py-execute-block-python not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-python-switch) nil \"py-execute-block-python-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-python-noswitch) nil \"py-execute-block-python-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-python-dedicated) nil \"py-execute-block-python-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-python-dedicated-switch) nil \"py-execute-block-python-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-ipython) nil \"py-execute-block-ipython not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-ipython-switch) nil \"py-execute-block-ipython-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-ipython-noswitch) nil \"py-execute-block-ipython-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-ipython-dedicated) nil \"py-execute-block-ipython-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-ipython-dedicated-switch) nil \"py-execute-block-ipython-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-python3) nil \"py-execute-block-python3 not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-python3-switch) nil \"py-execute-block-python3-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-python3-noswitch) nil \"py-execute-block-python3-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-python3-dedicated) nil \"py-execute-block-python3-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-python3-dedicated-switch) nil \"py-execute-block-python3-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-python2) nil \"py-execute-block-python2 not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-python2-switch) nil \"py-execute-block-python2-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-python2-noswitch) nil \"py-execute-block-python2-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-python2-dedicated) nil \"py-execute-block-python2-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-python2-dedicated-switch) nil \"py-execute-block-python2-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-python2.7) nil \"py-execute-block-python2.7 not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-python2.7-switch) nil \"py-execute-block-python2.7-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-python2.7-noswitch) nil \"py-execute-block-python2.7-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-python2.7-dedicated) nil \"py-execute-block-python2.7-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-python2.7-dedicated-switch) nil \"py-execute-block-python2.7-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-jython) nil \"py-execute-block-jython not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-jython-switch) nil \"py-execute-block-jython-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-jython-noswitch) nil \"py-execute-block-jython-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-jython-dedicated) nil \"py-execute-block-jython-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-jython-dedicated-switch) nil \"py-execute-block-jython-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-python) nil \"py-execute-block-or-clause-python not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-python-switch) nil \"py-execute-block-or-clause-python-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-python-noswitch) nil \"py-execute-block-or-clause-python-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-python-dedicated) nil \"py-execute-block-or-clause-python-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-python-dedicated-switch) nil \"py-execute-block-or-clause-python-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-ipython) nil \"py-execute-block-or-clause-ipython not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-ipython-switch) nil \"py-execute-block-or-clause-ipython-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-ipython-noswitch) nil \"py-execute-block-or-clause-ipython-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-ipython-dedicated) nil \"py-execute-block-or-clause-ipython-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-ipython-dedicated-switch) nil \"py-execute-block-or-clause-ipython-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-python3) nil \"py-execute-block-or-clause-python3 not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-python3-switch) nil \"py-execute-block-or-clause-python3-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-python3-noswitch) nil \"py-execute-block-or-clause-python3-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-python3-dedicated) nil \"py-execute-block-or-clause-python3-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-python3-dedicated-switch) nil \"py-execute-block-or-clause-python3-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-python2) nil \"py-execute-block-or-clause-python2 not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-python2-switch) nil \"py-execute-block-or-clause-python2-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-python2-noswitch) nil \"py-execute-block-or-clause-python2-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-python2-dedicated) nil \"py-execute-block-or-clause-python2-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-python2-dedicated-switch) nil \"py-execute-block-or-clause-python2-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-python2.7) nil \"py-execute-block-or-clause-python2.7 not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-python2.7-switch) nil \"py-execute-block-or-clause-python2.7-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-python2.7-noswitch) nil \"py-execute-block-or-clause-python2.7-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-python2.7-dedicated) nil \"py-execute-block-or-clause-python2.7-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-python2.7-dedicated-switch) nil \"py-execute-block-or-clause-python2.7-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-jython) nil \"py-execute-block-or-clause-jython not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-jython-switch) nil \"py-execute-block-or-clause-jython-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-jython-noswitch) nil \"py-execute-block-or-clause-jython-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-jython-dedicated) nil \"py-execute-block-or-clause-jython-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-block-or-clause-jython-dedicated-switch) nil \"py-execute-block-or-clause-jython-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-python) nil \"py-execute-def-python not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-python-switch) nil \"py-execute-def-python-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-python-noswitch) nil \"py-execute-def-python-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-python-dedicated) nil \"py-execute-def-python-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-python-dedicated-switch) nil \"py-execute-def-python-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-ipython) nil \"py-execute-def-ipython not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-ipython-switch) nil \"py-execute-def-ipython-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-ipython-noswitch) nil \"py-execute-def-ipython-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-ipython-dedicated) nil \"py-execute-def-ipython-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-ipython-dedicated-switch) nil \"py-execute-def-ipython-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-python3) nil \"py-execute-def-python3 not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-python3-switch) nil \"py-execute-def-python3-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-python3-noswitch) nil \"py-execute-def-python3-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-python3-dedicated) nil \"py-execute-def-python3-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-python3-dedicated-switch) nil \"py-execute-def-python3-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-python2) nil \"py-execute-def-python2 not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-python2-switch) nil \"py-execute-def-python2-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-python2-noswitch) nil \"py-execute-def-python2-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-python2-dedicated) nil \"py-execute-def-python2-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-python2-dedicated-switch) nil \"py-execute-def-python2-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-python2.7) nil \"py-execute-def-python2.7 not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-python2.7-switch) nil \"py-execute-def-python2.7-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-python2.7-noswitch) nil \"py-execute-def-python2.7-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-python2.7-dedicated) nil \"py-execute-def-python2.7-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-python2.7-dedicated-switch) nil \"py-execute-def-python2.7-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-jython) nil \"py-execute-def-jython not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-jython-switch) nil \"py-execute-def-jython-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-jython-noswitch) nil \"py-execute-def-jython-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-jython-dedicated) nil \"py-execute-def-jython-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-def-jython-dedicated-switch) nil \"py-execute-def-jython-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-python) nil \"py-execute-class-python not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-python-switch) nil \"py-execute-class-python-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-python-noswitch) nil \"py-execute-class-python-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-python-dedicated) nil \"py-execute-class-python-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-python-dedicated-switch) nil \"py-execute-class-python-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-ipython) nil \"py-execute-class-ipython not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-ipython-switch) nil \"py-execute-class-ipython-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-ipython-noswitch) nil \"py-execute-class-ipython-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-ipython-dedicated) nil \"py-execute-class-ipython-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-ipython-dedicated-switch) nil \"py-execute-class-ipython-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-python3) nil \"py-execute-class-python3 not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-python3-switch) nil \"py-execute-class-python3-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-python3-noswitch) nil \"py-execute-class-python3-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-python3-dedicated) nil \"py-execute-class-python3-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-python3-dedicated-switch) nil \"py-execute-class-python3-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-python2) nil \"py-execute-class-python2 not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-python2-switch) nil \"py-execute-class-python2-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-python2-noswitch) nil \"py-execute-class-python2-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-python2-dedicated) nil \"py-execute-class-python2-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-python2-dedicated-switch) nil \"py-execute-class-python2-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-python2.7) nil \"py-execute-class-python2.7 not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-python2.7-switch) nil \"py-execute-class-python2.7-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-python2.7-noswitch) nil \"py-execute-class-python2.7-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-python2.7-dedicated) nil \"py-execute-class-python2.7-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-python2.7-dedicated-switch) nil \"py-execute-class-python2.7-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-jython) nil \"py-execute-class-jython not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-jython-switch) nil \"py-execute-class-jython-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-jython-noswitch) nil \"py-execute-class-jython-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-jython-dedicated) nil \"py-execute-class-jython-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-class-jython-dedicated-switch) nil \"py-execute-class-jython-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-python) nil \"py-execute-region-python not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-python-switch) nil \"py-execute-region-python-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-python-noswitch) nil \"py-execute-region-python-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-python-dedicated) nil \"py-execute-region-python-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-python-dedicated-switch) nil \"py-execute-region-python-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-ipython) nil \"py-execute-region-ipython not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-ipython-switch) nil \"py-execute-region-ipython-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-ipython-noswitch) nil \"py-execute-region-ipython-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-ipython-dedicated) nil \"py-execute-region-ipython-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-ipython-dedicated-switch) nil \"py-execute-region-ipython-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-python3) nil \"py-execute-region-python3 not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-python3-switch) nil \"py-execute-region-python3-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-python3-noswitch) nil \"py-execute-region-python3-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-python3-dedicated) nil \"py-execute-region-python3-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-python3-dedicated-switch) nil \"py-execute-region-python3-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-python2) nil \"py-execute-region-python2 not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-python2-switch) nil \"py-execute-region-python2-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-python2-noswitch) nil \"py-execute-region-python2-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-python2-dedicated) nil \"py-execute-region-python2-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-python2-dedicated-switch) nil \"py-execute-region-python2-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-python2.7) nil \"py-execute-region-python2.7 not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-python2.7-switch) nil \"py-execute-region-python2.7-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-python2.7-noswitch) nil \"py-execute-region-python2.7-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-python2.7-dedicated) nil \"py-execute-region-python2.7-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-python2.7-dedicated-switch) nil \"py-execute-region-python2.7-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-jython) nil \"py-execute-region-jython not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-jython-switch) nil \"py-execute-region-jython-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-jython-noswitch) nil \"py-execute-region-jython-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-jython-dedicated) nil \"py-execute-region-jython-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-region-jython-dedicated-switch) nil \"py-execute-region-jython-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-python) nil \"py-execute-buffer-python not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-python-switch) nil \"py-execute-buffer-python-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-python-noswitch) nil \"py-execute-buffer-python-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-python-dedicated) nil \"py-execute-buffer-python-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-python-dedicated-switch) nil \"py-execute-buffer-python-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-ipython) nil \"py-execute-buffer-ipython not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-ipython-switch) nil \"py-execute-buffer-ipython-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-ipython-noswitch) nil \"py-execute-buffer-ipython-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-ipython-dedicated) nil \"py-execute-buffer-ipython-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-ipython-dedicated-switch) nil \"py-execute-buffer-ipython-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-python3) nil \"py-execute-buffer-python3 not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-python3-switch) nil \"py-execute-buffer-python3-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-python3-noswitch) nil \"py-execute-buffer-python3-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-python3-dedicated) nil \"py-execute-buffer-python3-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-python3-dedicated-switch) nil \"py-execute-buffer-python3-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-python2) nil \"py-execute-buffer-python2 not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-python2-switch) nil \"py-execute-buffer-python2-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-python2-noswitch) nil \"py-execute-buffer-python2-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-python2-dedicated) nil \"py-execute-buffer-python2-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-python2-dedicated-switch) nil \"py-execute-buffer-python2-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-python2.7) nil \"py-execute-buffer-python2.7 not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-python2.7-switch) nil \"py-execute-buffer-python2.7-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-python2.7-noswitch) nil \"py-execute-buffer-python2.7-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-python2.7-dedicated) nil \"py-execute-buffer-python2.7-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-python2.7-dedicated-switch) nil \"py-execute-buffer-python2.7-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-jython) nil \"py-execute-buffer-jython not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-jython-switch) nil \"py-execute-buffer-jython-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-jython-noswitch) nil \"py-execute-buffer-jython-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-jython-dedicated) nil \"py-execute-buffer-jython-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-buffer-jython-dedicated-switch) nil \"py-execute-buffer-jython-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-python) nil \"py-execute-expression-python not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-python-switch) nil \"py-execute-expression-python-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-python-noswitch) nil \"py-execute-expression-python-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-python-dedicated) nil \"py-execute-expression-python-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-python-dedicated-switch) nil \"py-execute-expression-python-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-ipython) nil \"py-execute-expression-ipython not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-ipython-switch) nil \"py-execute-expression-ipython-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-ipython-noswitch) nil \"py-execute-expression-ipython-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-ipython-dedicated) nil \"py-execute-expression-ipython-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-ipython-dedicated-switch) nil \"py-execute-expression-ipython-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-python3) nil \"py-execute-expression-python3 not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-python3-switch) nil \"py-execute-expression-python3-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-python3-noswitch) nil \"py-execute-expression-python3-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-python3-dedicated) nil \"py-execute-expression-python3-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-python3-dedicated-switch) nil \"py-execute-expression-python3-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-python2) nil \"py-execute-expression-python2 not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-python2-switch) nil \"py-execute-expression-python2-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-python2-noswitch) nil \"py-execute-expression-python2-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-python2-dedicated) nil \"py-execute-expression-python2-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-python2-dedicated-switch) nil \"py-execute-expression-python2-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-python2.7) nil \"py-execute-expression-python2.7 not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-python2.7-switch) nil \"py-execute-expression-python2.7-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-python2.7-noswitch) nil \"py-execute-expression-python2.7-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-python2.7-dedicated) nil \"py-execute-expression-python2.7-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-python2.7-dedicated-switch) nil \"py-execute-expression-python2.7-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-jython) nil \"py-execute-expression-jython not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-jython-switch) nil \"py-execute-expression-jython-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-jython-noswitch) nil \"py-execute-expression-jython-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-jython-dedicated) nil \"py-execute-expression-jython-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-expression-jython-dedicated-switch) nil \"py-execute-expression-jython-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-python-switch) nil \"py-execute-partial-expression-python-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-python) nil \"py-execute-partial-expression-python not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-python-noswitch) nil \"py-execute-partial-expression-python-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-python-dedicated) nil \"py-execute-partial-expression-python-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-python-dedicated-switch) nil \"py-execute-partial-expression-python-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-ipython) nil \"py-execute-partial-expression-ipython not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-ipython-switch) nil \"py-execute-partial-expression-ipython-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-ipython-noswitch) nil \"py-execute-partial-expression-ipython-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-ipython-dedicated) nil \"py-execute-partial-expression-ipython-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-ipython-dedicated-switch) nil \"py-execute-partial-expression-ipython-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-python3) nil \"py-execute-partial-expression-python3 not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-python3-switch) nil \"py-execute-partial-expression-python3-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-python3-noswitch) nil \"py-execute-partial-expression-python3-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-python3-dedicated) nil \"py-execute-partial-expression-python3-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-python3-dedicated-switch) nil \"py-execute-partial-expression-python3-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-python2) nil \"py-execute-partial-expression-python2 not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-python2-switch) nil \"py-execute-partial-expression-python2-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-python2-noswitch) nil \"py-execute-partial-expression-python2-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-python2-dedicated) nil \"py-execute-partial-expression-python2-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-python2-dedicated-switch) nil \"py-execute-partial-expression-python2-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-python2.7) nil \"py-execute-partial-expression-python2.7 not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-python2.7-switch) nil \"py-execute-partial-expression-python2.7-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-python2.7-noswitch) nil \"py-execute-partial-expression-python2.7-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-python2.7-dedicated) nil \"py-execute-partial-expression-python2.7-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-python2.7-dedicated-switch) nil \"py-execute-partial-expression-python2.7-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-jython) nil \"py-execute-partial-expression-jython not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-jython-switch) nil \"py-execute-partial-expression-jython-switch not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-jython-noswitch) nil \"py-execute-partial-expression-jython-noswitch not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-jython-dedicated) nil \"py-execute-partial-expression-jython-dedicated not detected as command\")" \
-eval "(assert (commandp 'py-execute-partial-expression-jython-dedicated-switch) nil \"py-execute-partial-expression-jython-dedicated-switch not detected as command\")" \
-eval "(assert (commandp 'py-mark-block-clause-misbehave-lp:949310-test) nil \"py-mark-block-clause-misbehave-lp:949310-test not detected as command\")" \
-eval "(assert (commandp 'broken-font-locking-lp:961231-test) nil \"broken-font-locking-lp:961231-test not detected as command\")" \
-eval "(assert (commandp 'py-mark-clause-misbehave-lp:949310-test) nil \"py-mark-clause-misbehave-lp:949310-test not detected as command\")" \
-eval "(assert (commandp 'py-mark-block-misbehave-lp:949310-test) nil \"py-mark-block-misbehave-lp:949310-test not detected as command\")" \
-eval "(assert (commandp 'py-mark-partial-expression) nil \"py-mark-partial-expression not detected as command\")" \
-eval "(message \"commandp tests done\")" \
--funcall py-execute-buffer-python3-switch-test \
--funcall py-execute-buffer-python2-switch-test \
--funcall py-guess-indent-offset-dont-detect-indent-of-2-lp-1027389-test \
--funcall indent-triplequoted-to-itself-lp:752252-test \
--funcall split-windows-on-execute-p-test \
--funcall key-binding-tests \
--funcall py-narrow-to-defun-lp-1020531-test \
--funcall pycomplete-imports-not-found-error-when-no-symbol-lp:1019791-test \
--funcall tqs-lp:302834-lp:1018994-test \
--funcall return-statement-indented-incorrectly-lp-1019601.py-test \
--funcall converts-tabs-to-spaces-in-indent-tabs-mode-t-lp-1019128.py-test \
--funcall empty-triple-quote-lp:1009318-test \
--funcall spurious-trailing-whitespace-lp-1008679-test \
--funcall py-end-of-statement-test \
--funcall no-completion-at-all-lp:1001328-test \
--funcall shebang-interpreter-not-detected-lp:1001327-test \
--funcall pycomplete-same-folder-def-lp:889052-test \
--funcall new-problem-with-py-temp-directory-lp:965762-test \
--funcall nested-dictionaries-indent-lp:328791-test \
--funcall new-problem-with-py-temp-directory-lp:965762-test \
--funcall nested-dictionaries-indent-lp:328791-test \
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
--funcall py-variable-name-face-lp:798538-test \
--funcall colon-causes-error-lp:818665-test \
--funcall if-indentation-lp:818720-test \
--funcall closing-parentesis-indent-lp:821820-test \
--funcall py-indent-line-lp:822532-test \
--funcall indent-honor-arglist-whitespaces-lp:822540-test \
--funcall comments-indent-honor-setting-lp:824427-test \
--funcall closing-list-lp:826144-test \
--funcall py-electric-comment-add-space-lp:828398-test \
--funcall py-electric-comment-add-space-t-lp:828398-test \
--funcall wrong-indentation-of-function-arguments-lp:840891-test \
--funcall wrong-guess-for-py-indent-offset-lp:852052-test \
--funcall indent-match-import-pkg-lp:852500-test \
--funcall py-hungry-delete-backwards-needs-cc-lp:850595-test \
--funcall py-shift-line-when-no-region-lp:855565-test \
--funcall indentation-of-from-import-continuation-lines-lp:858041-test \
--funcall indentation-after-one-line-suites-lp:858044-test \
--funcall py-compute-indentation-wrong-at-eol-lp:858043-test \
--funcall comment-indentation-level-lp:869854-test \
--funcall indentation-wrong-after-multi-line-parameter-list-lp:871698-test \
--funcall no-indent-after-continue-lp:872676-test \
--funcall indent-after-inline-comment-lp:873372-test \
--funcall else-clause-indentation-lp:874470-test \
--funcall indent-after-multiple-except-statements-lp:883815-test \
--funcall wrongly-highlighted-as-keywords-lp:885144-test \
--funcall glitch-when-indenting-lists-lp:886473-test \
--funcall another-indentation-bug-inside-docstrings-lp:900684-test \
--funcall incorrect-use-of-region-in-py-shift-left-lp:875951-test \
--funcall indentation-keyword-lp:885143-test \
--funcall fore-00007F-breaks-indentation-lp:328788-test \
--funcall indent-offset-not-guessed-when-loading-lp:902890-test \
--funcall from-__future__-import-absolute_import-mishighlighted-lp:907084-test \
--funcall automatic-indentation-is-broken-lp:889643-test \
--funcall chars-uU-preceding-triple-quoted-get-string-face-lp:909517-test \
--funcall py-pychecker-run-missing-lp:910783-test \
--funcall py-forward-into-nomenclature-lp:916818-test \
--funcall py-forward-into-nomenclature-jumps-over-CamelCased-words-lp:919540-test \
--funcall py-backward-into-nomenclature-caps-names-lp:919541-test \
--funcall fourth-level-blocks-indent-incorrectly-lp:939577-test \
--funcall py-mark-expression-marks-too-much-lp:941140-test \
--funcall py-indent-comments-nil-ignored-lp:958721-test \
--funcall tuple-unpacking-highlighted-incorrectly-lp:961496-test \
\
--funcall py-compute-indentation-with-test \
--funcall py-end-of-def-inline-comment-test \
--funcall before-inline-comment-test \
--funcall toggle-force-py-shell-name-p-test \
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
--funcall py-insert-super-python2-test \
--funcall py-smart-indent-eight-test \
--funcall wrong-type-argument-lp:901541-test \
--funcall indentation-bug-inside-docstrings-lp:899455-test \
--funcall py-insert-super-python2-test \
--funcall nested-try-finally-test \
--funcall py-separator-char-test \
--funcall switch-windows-on-execute-p-test \
--funcall python-dedicated-test \
\
--funcall py-execute-statement-test \
--funcall py-execute-block-test \
--funcall py-execute-block-or-clause-test \
--funcall py-execute-def-test \
--funcall py-execute-class-test \
--funcall py-execute-region-test \
--funcall py-execute-buffer-test \
--funcall py-execute-expression-test \
--funcall py-execute-partial-expression-test \
--funcall py-execute-line-test \
\
--funcall py-execute-statement-python-test \
--funcall py-execute-statement-ipython-test \
--funcall py-execute-statement-python3-test \
--funcall py-execute-statement-python2-test \
--funcall py-execute-statement-python2.7-switch-test \
--funcall py-execute-statement-python2.7-noswitch-test \
--funcall py-execute-statement-python2.7-dedicated-test \
--funcall py-execute-statement-python2.7-dedicated-switch-test \
\
--funcall py-shell-complete-test \
--funcall another-broken-font-locking-lp:961231-test \
--funcall py-execute-def-python-test \
--funcall py-execute-def-ipython-test \
--funcall py-execute-def-python3-test \
--funcall py-execute-def-python2-test \
--funcall py-execute-def-python2.7-test \
--funcall py-execute-class-python-test \
--funcall py-execute-class-ipython-test \
--funcall py-execute-class-python3-test \
--funcall py-execute-class-python2-test \
--funcall py-execute-class-python2.7-test \
--funcall py-execute-region-python-test \
--funcall py-execute-region-ipython-test \
--funcall py-execute-region-python3-test \
--funcall py-execute-region-python2-switch-test \
--funcall py-execute-expression-python-test \
--funcall py-execute-expression-ipython-test \
--funcall py-execute-expression-python3-test \
--funcall py-execute-expression-python2-test \
--funcall py-execute-expression-python2.7-test \
--funcall py-execute-partial-expression-python-test \
--funcall py-execute-partial-expression-python-switch-test \
--funcall py-execute-partial-expression-python-noswitch-test \
--funcall py-execute-partial-expression-python-dedicated-test \
--funcall py-execute-partial-expression-python-dedicated-switch-test \
--funcall py-execute-block-python-test \
--funcall py-execute-block-ipython-test \
--funcall py-execute-block-python3-test \
--funcall py-execute-block-python2-test \
--funcall py-execute-block-python2.7-test \
--funcall py-execute-block-or-clause-python-test \
--funcall py-execute-block-or-clause-ipython-test \
--funcall py-execute-block-or-clause-python3-test \
--funcall py-execute-block-or-clause-python2-test \
--funcall py-execute-block-or-clause-python2.7-test \
--funcall py-execute-block-or-clause-python2.7-switch-test \
--funcall py-execute-block-or-clause-python2.7-noswitch-test \
--funcall py-execute-block-or-clause-python2.7-dedicated-test \
--funcall py-execute-block-or-clause-python2.7-dedicated-switch-test \
--funcall py-execute-partial-expression-ipython-test \
t \
--funcall py-execute-partial-expression-python3-test \
t \
--funcall py-execute-partial-expression-python2-test \
t \
--funcall py-execute-partial-expression-python2.7-test \
est \
--funcall py-execute-line-python-test \
--funcall py-execute-line-ipython-test \
--funcall py-execute-line-python3-test \
--funcall py-execute-line-python2-test \
--funcall py-execute-line-python2.7-test \
--funcall py-execute-line-python2.7-switch-test \
--funcall py-execute-line-python2.7-noswitch-test \
--funcall py-execute-line-python2.7-dedicated-test \
--funcall py-execute-line-python2.7-dedicated-switch-test \
\
--funcall master-file-not-honored-lp:794850-test \
--funcall execute-buffer-ipython-fails-lp:928087-test \
--funcall py-shell-invoking-ipython-lp:835151-test \
--funcall py-shell-invoking-python3-lp:835151-test \
--funcall py-shell-invoking-python2-lp:835151-test \
--funcall py-shell-invoking-python2.7-lp:835151-test \
--funcall py-shell-invoking-jython-lp:835151-test \
\
--funcall py-electric-backspace-test \
--funcall UnicodeEncodeError-lp:550661-test \
--funcall py-insert-super-python3-test \
\
--funcall python-shell-complete-test \
--funcall usr-bin-python-shell-complete-test \
--funcall usr-bin-python2.7-shell-complete-test \
--funcall ipython-shell-complete-test \
--funcall usr-bin-ipython-shell-complete-test \
\
--funcall indent-region-lp:997958-test \
--funcall py-shell-invoking-python-lp:835151-test \
--funcall script-buffer-appears-instead-of-python-shell-buffer-lp:957561-test \
--funcall py-install-directory-path-test \
--funcall dict-error-test \
--funcall py-execute-region-python3-noswitch-test \
--funcall usr-bin-python3-shell-complete-test \
--funcall usr-bin-python3.1-shell-complete-test \
--funcall py-end-of-print-statement-test \
--funcall py-describe-symbol-fails-on-modules-lp:919719-test \
--funcall py-menu-pyshell-test \
--funcall completion-fails-in-python-script-r989-lp:1004613-test \
--funcall py-completion-at-point-test \
--funcall tqs-list-error-test \
--funcall UnicodeEncodeError-python3-test \
--funcall python-mode-slow-lp:803275-test \
--funcall infinite-loop-after-tqs-lp:826044-test \
--funcall not-that-useful-completion-lp:1003580-test \
--funcall completion-at-gentoo-lp-1008842-test \
--funcall py-find-imports-lp-1023236-test \
--funcall execute-indented-code-lp:828314-test \
--funcall py-execute-region-python2.7-switch-test \
--funcall py-execute-buffer-python-switch-test \
--funcall py-execute-buffer-ipython-switch-test \
--funcall pycomplete-same-folder-class-lp:889052-test \
--funcall py-ipython-complete-lp:927136-test \
--funcall augmented-assigment-test \
--funcall py-smart-operator-test \
