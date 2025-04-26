;;; python-components-mode.el --- Edit, debug, develop, run Python programs. -*- lexical-binding: t; -*-

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

;; Includes a minor mode for handling a Python/IPython shell, and can
;; take advantage of Pymacs when installed.

;; See documentation in README.org, README.DEVEL.org

;; Please report bugs at
;; https://gitlab.com/python-mode-devs/python-mode/issues

;; available commands are documented in directory "doc" as
;; commands-python-mode.org

;; As for ‘py-add-abbrev’:
;; Similar to ‘add-mode-abbrev’, but uses
;; ‘py-partial-expression’ before point for expansion to
;; store, not ‘word’.  Also provides a proposal for new
;; abbrevs.

;; Proposal for an abbrev is composed from the downcased
;; initials of expansion - provided they are of char-class
;; [:alpha:]
;;
;; For example code below would be recognised as a
;; ‘py-expression’ composed by three
;; py-partial-expressions.
;;
;; OrderedDict.popitem(last=True)
;;
;; Putting the curser at the EOL, M-3 M-x py-add-abbrev
;;
;; would prompt "op" for an abbrev to store, as first
;; ‘py-partial-expression’ beginns with a "(", which is
;; not taken as proposal.

;;; Code:

;; used by py-hungry-delete-backwards
(require (quote cc-cmds))
(require (quote python-components-vars))
(require (quote python-components-rx))
(require (quote python-components-statement))
(require (quote python-components-foot))
(require (quote python-components-start1))
(require (quote python-components-extra))
(require (quote python-components-shift-forms))
(require (quote python-components-down))
(require (quote python-components-start-Zf98zM))
(require (quote python-components-backward-forms))
(require (quote python-components-forward-forms))
(require (quote python-components-start2))
(require (quote python-components-start3))
(require (quote python-components-execute-file))
(require (quote python-components-up))
(require (quote python-components-booleans-beginning-forms))
(require (quote python-components-move))
(require (quote python-components-end-position-forms))
(require (quote python-components-beginning-position-forms))
(require (quote python-components-extended-executes))
(require (quote python-components-execute))
(require (quote python-components-compute-indentation))
(require (quote python-components-intern))
(require (quote python-components-copy-forms))
(require (quote python-components-delete-forms))
(require (quote python-components-mark-forms))
(require (quote python-components-copy-forms))
(require (quote python-components-delete-forms))
(require (quote python-components-close-forms))
(require (quote python-components-kill-forms))
(require (quote python-components-forms-code))
(require (quote python-components-booleans-end-forms))
(require (quote python-components-exec-forms))
(require (quote python-components-switches))
(require (quote python-components-edit))
(require (quote python-components-named-shells))
(require (quote python-components-font-lock))
(require (quote python-components-menu))
(require (quote python-components-map))
(require (quote python-components-shell-menu))
(require (quote python-components-complete))
(require (quote python-components-pdb))
(require (quote python-components-pdbtrack))
(require (quote python-components-help))
(require (quote python-components-extensions))
(require (quote python-components-imenu))
;; now installed by py-load-named-shells
;; (require (quote python-components-named-shells))
(require (quote python-components-electric))
(require (quote python-components-virtualenv))
(require (quote python-abbrev-propose))
(require (quote python-components-paragraph))
(require (quote python-components-section-forms))
(require (quote python-components-comment))
(require (quote python-components-fast-forms))
(require (quote python-components-narrow))
(require (quote python-components-hide-show))
(require (quote python-components-ffap))

;; (require (quote py-setup-ert-tests))
;; (require (quote python-components-eldoc))

;; (provide (quote python-components-mode))
(provide (quote python-mode))
;;; python-components-mode.el ends her
