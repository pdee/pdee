#!/bin/sh

# Author: https://gitlab.com/groups/python-mode-devs

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

# run-tests.sh -[OPTION]
# selecting single test-files

# This script tests functions from python-mode.el.

# Code:

if [ $1 == en ]; then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif [ $1 == e25 ]; then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e26 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e27 ];then
    #  export EMACS="$HOME/emacs-20220306/src/emacs -Q"
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e28 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e29 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e30 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
else
    EMACS=emacs
fi

echo "
before shift \$EMACS: $EMACS"
shift
echo "after shift \$EMACS: $EMACS"
echo "\$*: $*"
echo "\$_: $_"
PDIR=$PWD
echo "\$PWD: $PWD"
# IFLOCAL set in .bashrc, thus unset remotly
IFLOCAL=${IFLOCAL:=1}
echo "\$IFLOCAL: $IFLOCAL"
TESTDIR=$PWD/test
export TESTDIR

# EMACS_TEST_VERBOSE

if [ -s $PWD/python-mode.el ]; then
    PYTHONMODE=$PWD/python-mode.el
elif [ -s $PWD/python-components-mode.el ]; then
    PYTHONMODE=$PWD/python-components-mode.el
fi

echo "\$PYTHONMODE: $PYTHONMODE"
SETUP=${PWD}/py-setup-ert-tests.el

TEST1=$TESTDIR/py-ert-indent-tests.el
TEST2=$TESTDIR/py-ert-beginning-tests.el
TEST3=$TESTDIR/py-ert-forward-tests.el
TEST4=$TESTDIR/py-ert-misc-tests.el
TEST5=$TESTDIR/py-ert-scope-tests.el
TEST6=$TESTDIR/py-ert-function-tests.el
TEST7=$TESTDIR/py-ert-variablen-tests.el
TEST8=$TESTDIR/py-ert-navigation-tests.el
TEST9=$TESTDIR/py-ert-delete-tests.el
TEST10=$TESTDIR/py-ert-execute-region-test.el
TEST11=$TESTDIR/py-execute-region-commandp-test.el
TEST12=$TESTDIR/py-ert-fill-tests.el
TEST13=$TESTDIR/py-extra-tests.el
TEST14=$TESTDIR/py-ert-hide-tests.el
TEST15=$TESTDIR/py-ert-font-lock-test.el
TEST16=$TESTDIR/py-executable-python-tests.el
TEST17=$TESTDIR/py-split-window-on-execute-test
TEST18=$TESTDIR/py-split-just-two-window-on-execute-test.el
TEST19=$TESTDIR/py-ert-interactive-tests.el
TEST20=$HOME/emacs/test/lisp/progmodes/python-tests.el
TEST21=$TESTDIR/py-ert-ipython-tests.el
TEST22=$TESTDIR/py-interactive-tests.el
echo "\$EMACS: $EMACS"

PYCO="$PWD/completion/pycomplete.el"

h1() { 
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq py-verbose-p nil)" \
--eval "(setq py-install-dir \"$PWD\")" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
--eval "(add-to-list 'load-path \"$PWD/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST1 \
-f ert-run-tests-batch-and-exit
}

h2() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST2 \
-f ert-run-tests-batch-and-exit
}

h3() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST3 \
-f ert-run-tests-batch-and-exit
}

h4() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST4 \
-f ert-run-tests-batch-and-exit
}

h5() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST5 \
-f ert-run-tests-batch-and-exit
}

h6() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST6 \
-f ert-run-tests-batch-and-exit
}

h7() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST7 \
-f ert-run-tests-batch-and-exit
}

h8() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST8 \
-f ert-run-tests-batch-and-exit
}

h9() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST9 \
-f ert-run-tests-batch-and-exit
}

h10() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST10 \
-f ert-run-tests-batch-and-exit
}

h11() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST11 \
-f ert-run-tests-batch-and-exit
}

h12() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST12 \
-f ert-run-tests-batch-and-exit
}

h13() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST13 \
-f ert-run-tests-batch-and-exit
}

h14() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST14 \
-f ert-run-tests-batch-and-exit
}

h15() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p t)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST15 \
-f ert-run-tests-batch-and-exit
}

h16() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST16 \
-f ert-run-tests-batch-and-exit
}

h17() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
-l $HOME/emacs/lisp/progmodes/python.el \
-l $TEST17 \
-f ert-run-tests-batch-and-exit
}

h18() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
-l $HOME/emacs/lisp/progmodes/python.el \
-l $TEST18 \
-f ert-run-tests-batch-and-exit
}

h19() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST19 \
-f ert-run-tests-batch-and-exit
}

h20() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST20 \
-f ert-run-tests-batch-and-exit
}

h21() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST21 \
-f ert-run-tests-batch-and-exit
}

h22() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST22 \
-f ert-run-tests-batch-and-exit
}

h23() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST23 \
--eval "(py-in-list-indent-test-XnIq9d1)"
}

hierv5() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p t)" \
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
-l $TEST16 \
-f ert-run-tests-batch-and-exit
}

extrav5() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p t)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST16 \
-f ert-run-tests-batch-and-exit
}

extra() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p nil)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-l $TEST1 \
-f ert-run-tests-batch-and-exit
}

entfernt() {
$EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
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
-l $TEST20 \
--eval "(setq py-debug-p nil)" \
-f ert-run-tests-batch-and-exit
}

hier() {
    date; $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq py-debug-p nil)" \
--eval "(setq python-indent-guess-indent-offset nil)" \
--eval "(setq python-indend-offset 4)" \
--eval "(setq python-mode-v5-behavior-p nil)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $PYTHONMODE \
-l $TEST1 \
-l $TEST2 \
-l $TEST3 \
-l $TEST4 \
-l $TEST5 \
-l $TEST6 \
-l $TEST7 \
-l $TEST8 \
-l $TEST9 \
-l $TEST10 \
-l $TEST11 \
-l $TEST13 \
-l $TEST16 \
-l $TEST21 \
-f ert-run-tests-batch-and-exit
}

if [ $IFLOCAL -eq 0 ]; then

    # sleep 1

    while getopts 123456789abcdefghijklmnopqrstuvx option
    do
        case $option in
	    1) echo "h1: Lade \$TEST1: \"$TEST1\"";h1;;
	    2) echo "h2: Lade \$TEST2: \"$TEST2\"";h2;;
	    3) echo "h3: Lade \$TEST3: \"$TEST3\"";h3;;
	    4) echo "h4: Lade \$TEST4: \"$TEST4\"";h4;;
	    5) echo "h5: Lade \$TEST5: \"$TEST5\"";h5;;
	    6) echo "h6: Lade \$TEST6: \"$TEST6\"";h6;;
	    7) echo "h7: Lade \$TEST7: \"$TEST7\"";h7;;
	    8) echo "h8: Lade \$TEST8: \"$TEST8\"";h8;;
	    9) echo "h9: Lade \$TEST9: \"$TEST9\"";h9;;
	    a) echo "h10: Lade \$TEST10: \"$TEST10\"";h10;;
	    b) echo "h11: Lade \$TEST11: \"$TEST11\"";h11;;
	    c) echo "h12: Lade \$TEST12: \"$TEST12\"";h12;;
	    d) echo "h13: Lade \$TEST13: \"$TEST13\"";h13;;
	    e) echo "h14: Lade \$TEST14: \"$TEST14\"";h14;;
	    f) echo "h15: Lade \$TEST15: \"$TEST15\"";h15;;
	    g) echo "h16: Lade \$TEST16: \"$TEST16\"";h16;;
            h) echo "h17: Running python-tests.el";h17;;
	    i) echo "h18: Lade \$TEST18: \"$TEST18\"";h18;;
	    j) echo "h19: Lade \$TEST19: \"$TEST19\"";h19;;
	    k) echo "h20: Lade \$TEST20: \"$TEST20\"";h20;;
	    l) echo "h21: Lade neu: \"neu\"";neu;;
	    m) echo "h22: Lade \$TEST22: \"$TEST22\"";h22;;
	    n) echo "Lade Testumgebung ‘hier’";hier

	esac
	echo "\$*: $*"

    done

    else
    echo "entfernt"
    echo "\$IFLOCAL: $IFLOCAL"
    echo "Lade testumgebung \"ENTFERNT\""
    entfernt
fi
