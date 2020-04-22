#!/bin/sh

# Author: Andreas Röhler <andreas.roehler@online.de>

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

# This script tests functions from python-mode.el.

# Code:

emacs27=~$HOME/emacs-20180529/src/emacs-27.0.50.1
ARSUBR=~/werkstatt/thingatpt-utils-core/ar-subr.el
PYTHON=~/emacs-20180529/lisp/progmodes/python.el
PDIR=$PWD
echo "\$PWD: $PWD"
# WERKSTATT set in .bashrc, thus unset remotly
WERKSTATT=${WERKSTATT:=1}
echo "\$WERKSTATT: $WERKSTATT"

TESTDIR=$PDIR/test
export TESTDIR

# echo "\$1: $1"

# if $1; then
# PYTHONMODE=$PDIR/$1
if [ -s $PDIR/python-mode.el ]; then
    PYTHONMODE=$PDIR/python-mode.el
elif [ -s $PDIR/python-components-mode.el ]; then
    PYTHONMODE=$PDIR/python-components-mode.el
fi

echo "\$PYTHONMODE: $PYTHONMODE"

SETUP=$TESTDIR/setup-ert-tests.el

TEST1=$TESTDIR/py-ert-tests-1.el
TEST2=$TESTDIR/py-ert-tests-2.el
TEST3=$TESTDIR/py-ert-always-split-lp-1361531-tests.el
TEST4=$TESTDIR/py-ert-just-two-split-lp-1361531-tests.el
TEST5=$TESTDIR/py-ert-beginning-tests.el
TEST6=$TESTDIR/py-ert-forward-tests.el
TEST7=$TESTDIR/py-ert-function-tests.el
TEST8=$TESTDIR/py-ert-variablen-tests.el
TEST9=$TESTDIR/py-shell-arg-ert-tests.el
TEST10=$TESTDIR/py-ert-execute-block-test.el
TEST11=$TESTDIR/py-ert-execute-region-test.el
TEST12=$TESTDIR/py-execute-region-commandp-test.el
TEST13=$TESTDIR/py-ert-tests-3.el
TEST14=$TESTDIR/py-ert-forward-tests.el
TEST15=$TESTDIR/py-ert-tests-4.el
TEST16=$TESTDIR/py-extra-tests.el
TEST17=$HOME/emacs/test/lisp/progmodes/python-tests.el
TEST18=$TESTDIR/translated-python-tests.el

if [ -s emacs27 ]; then
    EMACS=emacs27
else
    EMACS=emacs
fi

echo "\$EMACS: $EMACS"

PYCO="$PDIR/completion/pycomplete.el"

h1() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $PYTHONMODE \
-l $TEST1 \
-f ert-run-tests-batch-and-exit
}

h2() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $PYTHONMODE \
-l $TEST2 \
-f ert-run-tests-batch-and-exit
}

h3() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $PYTHONMODE \
-l $TEST3 \
-f ert-run-tests-batch-and-exit
}

h4() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $PYTHONMODE \
-l $TEST4 \
-f ert-run-tests-batch-and-exit
}

h5() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $PYTHONMODE \
-l $TEST5 \
-f ert-run-tests-batch-and-exit
}

h6() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $PYTHONMODE \
-l $TEST6 \
-f ert-run-tests-batch-and-exit
}

h7() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $PYTHONMODE \
-l $TEST7 \
-f ert-run-tests-batch-and-exit
}

h8() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $PYTHONMODE \
-l $TEST8 \
-f ert-run-tests-batch-and-exit
}

h9() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $PYTHONMODE \
-l $TEST9 \
-f ert-run-tests-batch-and-exit
}

h10() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $PYTHONMODE \
-l $TEST10 \
-f ert-run-tests-batch-and-exit
}

h11() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $PYTHONMODE \
-l $TEST11 \
-f ert-run-tests-batch-and-exit
}

h12() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $PYTHONMODE \
-l $TEST12 \
-f ert-run-tests-batch-and-exit
}

h13() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $PYTHONMODE \
-l $TEST13 \
-f ert-run-tests-batch-and-exit
}

h14() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $PYTHONMODE \
-l $TEST14 \
-f ert-run-tests-batch-and-exit
}

h15() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $PYTHONMODE \
-l $TEST15 \
-f ert-run-tests-batch-and-exit
}

h16() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $PYTHONMODE \
-l $TEST16 \
-f ert-run-tests-batch-and-exit
}

h17() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
-l $TEST17 \
-f ert-run-tests-batch-and-exit
}

hierv5() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $PYTHONMODE \
-l $TEST1 \
-l $TEST2 \
-l $TEST4 \
-l $TEST5 \
-l $TEST6 \
-l $TEST7 \
-l $TEST8 \
-l $TEST11 \
-l $TEST12 \
-l $TEST13 \
-l $TEST14 \
-l $TEST15 \
-l $TEST16 \
-f ert-run-tests-batch-and-exit
}


hier() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p nil)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST1 \
-l $TEST2 \
-l $TEST4 \
-l $TEST5 \
-l $TEST6 \
-l $TEST7 \
-l $TEST8 \
-l $TEST11 \
-l $TEST12 \
-l $TEST13 \
-l $TEST14 \
-l $TEST15 \
-l $TEST16 \
-f ert-run-tests-batch-and-exit
}

erst() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p nil)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST1 \
-l $TEST2 \
-l $TEST4 \
-l $TEST5 \
-l $TEST6 \
-l $TEST7 \
-f ert-run-tests-batch-and-exit
}

zweit() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p nil)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST8 \
-l $TEST11 \
-l $TEST12 \
-l $TEST13 \
-l $TEST14 \
-l $TEST15 \
-l $TEST16 \
-f ert-run-tests-batch-and-exit
}

extrav5() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST16 \
-f ert-run-tests-batch-and-exit
}

extra() {
    date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p nil)" \
--eval "(setq py-verbose-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST16 \
-f ert-run-tests-batch-and-exit
}

entfernt() {
$EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST1 \
-l $TEST2 \
-l $TEST4 \
-l $TEST5 \
-l $TEST6 \
-l $TEST7 \
-l $TEST8 \
-l $TEST12 \
-l $TEST13 \
-l $TEST14 \
-l $TEST15 \
--eval "(setq py-debug-p nil)" \
-f ert-run-tests-batch-and-exit
}

if [ $WERKSTATT -eq 0 ]; then
    while getopts 123456789abcehijkpqrstuvx option
    do
        case $option in
	    1) echo "Lade \$TEST1: \"$TEST1\"";h1;;
	    2) echo "Lade \$TEST2: \"$TEST2\"";h2;;
	    3) echo "Lade \$TEST3: \"$TEST3\"";h3;;
	    4) echo "Lade \$TEST4: \"$TEST4\"";h4;;
	    5) echo "Lade \$TEST5: \"$TEST5\"";h5;;
	    6) echo "Lade \$TEST6: \"$TEST6\"";h6;;
	    7) echo "Lade \$TEST7: \"$TEST7\"";h7;;
	    8) echo "Lade \$TEST8: \"$TEST8\"";h8;;
	    9) echo "Lade \$TEST9: \"$TEST9\"";h9;;
	    a) echo "Lade erst"; erst;;
	    b) echo "Lade zweit"; zweit;;
 	    c) echo "Running python-tests.el";h17;;
	    e) echo "Lade testumgebung \"ENTFERNT\""; entfernt;;
	    i) echo "Lade \$TEST10: \"$TEST10\"";h10;;
	    j) echo "Lade \$TEST11: \"$TEST11\"";h11;;
	    k) echo "Lade \$TEST12: \"$TEST12\"";h12;;
	    p) echo "Lade \$TEST13: \"$TEST13\"";h13;;
	    q) echo "Lade \$TEST14: \"$TEST14\"";h14;;
	    r) echo "Lade \$TEST15: \"$TEST15\"";h15;;
	    s) echo "Lade \$TEST16: \"$TEST16\"";h16;;
	    u) echo "Lade testumgebung \"EXTRA\"";hierv5;;
	    v) echo "Lade testumgebung \"EXTRA\"";extrav5;;
	    x) echo "Lade testumgebung \"EXTRA\"";extra;;
            h) echo "Lade testumgebung \"HIER1\"";hier;;
	esac
    done

    # hier1
    # echo "Lade testumgebung \"HIER1\""
    # hier2
    # echo "Lade testumgebung \"HIER1\""

else
    echo "entfernt"
    echo "\$WERKSTATT: $WERKSTATT"
    echo "Lade testumgebung \"ENTFERNT\""
    entfernt
fi


# -l $TEST1 \
# -l $TEST2 \
# -l $TEST4 \
# -l $TEST5 \
# -l $TEST6 \
# -l $TEST7 \
# -l $TEST8 \
# -l $TEST11 \
# -l $TEST12 \
# -l $TEST13 \
# -l $TEST14 \
# -l $TEST15 \
# -l $TEST16 \
