;;; python-components-mode.el --- Edit, debug, develop, run Python programs. -*- lexical-binding: t; -*-

;; Version: 6.2.3

;; Keywords: languages, processes, python, oop

;; URL: https://gitlab.com/groups/python-mode-devs

;; Package-Requires: ((emacs "24"))

;; Author: 2015-2021 https://gitlab.com/groups/python-mode-devs
;;         2003-2014 https://launchpad.net/python-mode
;;         1995-2002 Barry A. Warsaw
;;         1992-1994 Tim Peters
;; Maintainer: python-mode@python.org
;; Created:    Feb 1992
;; Keywords:   python languages oop

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

;; As for `py-add-abbrev':
;; Similar to `add-mode-abbrev', but uses
;; `py-partial-expression' before point for expansion to
;; store, not `word'.  Also provides a proposal for new
;; abbrevs.

;; Proposal for an abbrev is composed from the downcased
;; initials of expansion - provided they are of char-class
;; [:alpha:]
;;
;; For example code below would be recognised as a
;; `py-expression' composed by three
;; py-partial-expressions.
;;
;; OrderedDict.popitem(last=True)
;;
;; Putting the curser at the EOL, M-3 M-x py-add-abbrev
;;
;; would prompt "op" for an abbrev to store, as first
;; `py-partial-expression' beginns with a "(", which is
;; not taken as proposal.

;;; Code:

(require 'python-components-foot)
(require 'python-components-start1)
(require 'python-components-extra)
(require 'python-components-shift-forms)
(require 'python-components-down)
(require 'python-components-start-Zf98zM)
(require 'python-components-backward-forms)
(require 'python-components-forward-forms)
(require 'python-components-start2)
(require 'python-components-start3)
(require 'python-components-execute-file)
(require 'python-components-up)
(require 'python-components-booleans-beginning-forms)
(require 'python-components-move)
(require 'python-components-end-position-forms)
(require 'python-components-beginning-position-forms)
(require 'python-components-extended-executes)
(require 'python-components-execute)
(require 'python-components-intern)
(require 'python-components-copy-forms)
(require 'python-components-delete-forms)
(require 'python-components-mark-forms)
(require 'python-components-copy-forms)
(require 'python-components-delete-forms)
(require 'python-components-close-forms)
(require 'python-components-kill-forms)
(require 'python-components-forms-code)
(require 'python-components-booleans-end-forms)
(require 'python-components-exec-forms)
(require 'python-components-switches)
(require 'python-components-edit)
(require 'python-components-named-shells)
(require 'python-components-font-lock)
(require 'python-components-menu)
(require 'python-components-map)
(require 'python-components-shell-menu)
(require 'python-components-complete)
(require 'python-components-pdb)
(require 'python-components-pdbtrack)
(require 'python-components-help)
(require 'python-components-extensions)
(require 'python-components-imenu)
;; now installed by py-load-named-shells
;; (require 'python-components-named-shells)
(require 'python-components-electric)
(require 'python-components-virtualenv)
(require 'python-abbrev-propose)
(require 'python-components-paragraph)
(require 'python-components-section-forms)
(require 'python-components-comment)
(require 'python-components-fast-forms)
(require 'python-components-narrow)
(require 'python-components-hide-show)
(require 'python-components-ffap)

;; (require 'python-components-eldoc)


;; (provide 'python-components-mode)
(provide 'python-mode)
;;; python-components-mode.el ends her
