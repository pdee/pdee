#!/bin/sh

# URL: https://gitlab.com/python-mode-devs

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

# This script compiles files where python-mode.el is composed off.

# Code:

PDIR=$PWD
echo "\$PWD: $PWD"

EMACS=emacs

if [ -s $PDIR/python-mode.el ]; then
    PYTHONMODE=$PDIR/python-mode.el
elif [ -s $PDIR/python-components-mode.el ]; then
    PYTHONMODE=$PDIR/python-components-mode.el
fi

echo "\$PYTHONMODE: $PYTHONMODE"

date; time -p $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq py--debug-p nil)" \
--eval "(setq python-mode-v5-behavior-p nil)" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
 \
--load "python-components-start1.el" --eval "(byte-compile-file \"python-components-start1.el\")" \
--load "python-components-extra.el" --eval "(byte-compile-file \"python-components-extra.el\")" \
--load "python-components-shift-forms.el" --eval "(byte-compile-file \"python-components-shift-forms.el\")" \
--load "python-components-down.el" --eval "(byte-compile-file \"python-components-down.el\")" \
--load "python-components-start-Zf98zM.el" --eval "(byte-compile-file \"python-components-start-Zf98zM.el\")" \
--load "python-components-backward-forms.el" --eval "(byte-compile-file \"python-components-backward-forms.el\")" \
--load "python-components-forward-forms.el" --eval "(byte-compile-file \"python-components-forward-forms.el\")" \
--load "python-components-start2.el" --eval "(byte-compile-file \"python-components-start2.el\")" \
--load "python-components-start3.el" --eval "(byte-compile-file \"python-components-start3.el\")" \
--load "python-components-execute-file.el" --eval "(byte-compile-file \"python-components-execute-file.el\")" \
--load "python-components-up.el" --eval "(byte-compile-file \"python-components-up.el\")" \
--load "python-components-booleans-beginning-forms.el" --eval "(byte-compile-file \"python-components-booleans-beginning-forms.el\")" \
--load "python-components-move.el" --eval "(byte-compile-file \"python-components-move.el\")" \
--load "python-components-end-position-forms.el" --eval "(byte-compile-file \"python-components-end-position-forms.el\")" \
--load "python-components-beginning-position-forms.el" --eval "(byte-compile-file \"python-components-beginning-position-forms.el\")" \
--load "python-components-extended-executes.el" --eval "(byte-compile-file \"python-components-extended-executes.el\")" \
--load "python-components-execute.el" --eval "(byte-compile-file \"python-components-execute.el\")" \
--load "python-components-intern.el" --eval "(byte-compile-file \"python-components-intern.el\")" \
--load "python-components-copy-forms.el" --eval "(byte-compile-file \"python-components-copy-forms.el\")" \
--load "python-components-delete-forms.el" --eval "(byte-compile-file \"python-components-delete-forms.el\")" \
--load "python-components-mark-forms.el" --eval "(byte-compile-file \"python-components-mark-forms.el\")" \
--load "python-components-close-forms.el" --eval "(byte-compile-file \"python-components-close-forms.el\")" \
--load "python-components-kill-forms.el" --eval "(byte-compile-file \"python-components-kill-forms.el\")" \
--load "python-components-forms-code.el" --eval "(byte-compile-file \"python-components-forms-code.el\")" \
--load "python-components-booleans-end-forms.el" --eval "(byte-compile-file \"python-components-booleans-end-forms.el\")" \
--load "python-components-exec-forms.el" --eval "(byte-compile-file \"python-components-exec-forms.el\")" \
--load "python-components-switches.el" --eval "(byte-compile-file \"python-components-switches.el\")" \
--load "python-components-edit.el" --eval "(byte-compile-file \"python-components-edit.el\")" \
--load "python-components-named-shells.el" --eval "(byte-compile-file \"python-components-named-shells.el\")" \
--load "python-components-font-lock.el" --eval "(byte-compile-file \"python-components-font-lock.el\")" \
--load "python-components-menu.el" --eval "(byte-compile-file \"python-components-menu.el\")" \
--load "python-components-map.el" --eval "(byte-compile-file \"python-components-map.el\")" \
--load "python-components-shell-menu.el" --eval "(byte-compile-file \"python-components-shell-menu.el\")" \
--load "python-components-complete.el" --eval "(byte-compile-file \"python-components-complete.el\")" \
--load "python-components-pdb.el" --eval "(byte-compile-file \"python-components-pdb.el\")" \
--load "python-components-pdbtrack.el" --eval "(byte-compile-file \"python-components-pdbtrack.el\")" \
--load "python-components-help.el" --eval "(byte-compile-file \"python-components-help.el\")" \
--load "python-components-extensions.el" --eval "(byte-compile-file \"python-components-extensions.el\")" \
--load "python-components-imenu.el" --eval "(byte-compile-file \"python-components-imenu.el\")" \
--load "python-components-electric.el" --eval "(byte-compile-file \"python-components-electric.el\")" \
--load "python-components-virtualenv.el" --eval "(byte-compile-file \"python-components-virtualenv.el\")" \
--load "python-abbrev-propose.el" --eval "(byte-compile-file \"python-abbrev-propose.el\")" \
--load "python-components-paragraph.el" --eval "(byte-compile-file \"python-components-paragraph.el\")" \
--load "python-components-section-forms.el" --eval "(byte-compile-file \"python-components-section-forms.el\")" \
--load "python-components-comment.el" --eval "(byte-compile-file \"python-components-comment.el\")" \
--load "python-components-fast-forms.el" --eval "(byte-compile-file \"python-components-fast-forms.el\")" \
--load "python-components-narrow.el" --eval "(byte-compile-file \"python-components-narrow.el\")" \
--load "python-components-hide-show.el" --eval "(byte-compile-file \"python-components-hide-show.el\")" \
--load "python-components-ffap.el" --eval "(byte-compile-file \"python-components-ffap.el\")" \
--load "python-components-foot.el" --eval "(byte-compile-file \"python-components-foot.el\")"
