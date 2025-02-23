;;; python-components-map.el --- Install a python-mode-map -*- lexical-binding: t; -*-

;; URL: https://gitlab.com/python-mode-devs

;; Keywords: languages

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

(defvar py-use-menu-p t
  "If the menu should be loaded.

Default is t")

(defvar py-menu nil
  "Make a dynamically bound variable ‘py-menu’.")

(setq python-mode-map
      (let ((map (make-sparse-keymap)))
        ;; electric keys
        (define-key map [(:)] (quote py-electric-colon))
        (define-key map [(\#)] (quote py-electric-comment))
        (define-key map [(delete)] (quote py-electric-delete))
        (define-key map [(control backspace)] (quote py-hungry-delete-backwards))
        (define-key map [(control c) (delete)] (quote py-hungry-delete-forward))
        ;; (define-key map [(control y)] (quote py-electric-yank))
        ;; moving point
        (define-key map [(control c) (control p)] (quote py-backward-statement))
        (define-key map [(control c) (control n)] (quote py-forward-statement))
        (define-key map [(control c) (control u)] (quote py-backward-block))
        (define-key map [(control c) (control q)] (quote py-forward-block))
        (define-key map [(control meta a)] (quote py-backward-def-or-class))
        (define-key map [(control meta e)] (quote py-forward-def-or-class))
        ;; (define-key map [(meta i)] (quote py-indent-forward-line))
        ;; (define-key map [(control j)] (quote py-newline-and-indent))
	(define-key map (kbd "C-j") 'newline)
        ;; Most Pythoneers expect RET ‘py-newline-and-indent’
	;; which is default of var py-return-key’
        (define-key map (kbd "RET") py-return-key)
        ;; (define-key map (kbd "RET") 'newline)
        ;; (define-key map (kbd "RET") (quote py-newline-and-dedent))
        (define-key map [(super backspace)] (quote py-dedent))
        ;; (define-key map [(control return)] (quote py-newline-and-dedent))
        ;; indentation level modifiers
        (define-key map [(control c) (control l)] (quote py-shift-left))
        (define-key map [(control c) (control r)] (quote py-shift-right))
        (define-key map [(control c) (<)] (quote py-shift-left))
        (define-key map [(control c) (>)] (quote py-shift-right))
        ;; (define-key map [(control c) (tab)] (quote py-indent-region))
	(define-key map (kbd "C-c TAB") (quote py-indent-region))
        (define-key map [(control c) (:)] (quote py-guess-indent-offset))
        ;; subprocess commands
        (define-key map [(control c) (control c)] (quote py-execute-buffer))
        (define-key map [(control c) (control m)] (quote py-execute-import-or-reload))
        (define-key map [(control c) (control s)] (quote py-execute-string))
        (define-key map [(control c) (|)] (quote py-execute-region))
        (define-key map [(control meta x)] (quote py-execute-def-or-class))
        (define-key map [(control c) (!)] (quote py-shell))
        (define-key map [(control c) (control t)] (quote py-toggle-shell))
        (define-key map [(control meta h)] (quote py-mark-def-or-class))
        (define-key map [(control c) (control k)] (quote py-mark-block-or-clause))
        (define-key map [(control c) (.)] (quote py-expression))
        (define-key map [(control c) (?,)] (quote py-partial-expression))
        ;; Miscellaneous
        ;; (define-key map [(super q)] (quote py-copy-statement))
        (define-key map [(control c) (control d)] (quote py-pdbtrack-toggle-stack-tracking))
        (define-key map [(control c) (control f)] (quote py-sort-imports))
        (define-key map [(control c) (\#)] (quote py-comment-region))
        (define-key map [(control c) (\?)] (quote py-describe-mode))
        (define-key map [(control c) (control e)] (quote py-help-at-point))
        (define-key map [(control c) (-)] (quote py-up-exception))
        (define-key map [(control c) (=)] (quote py-down-exception))
        (define-key map [(control x) (n) (d)] (quote py-narrow-to-def-or-class))
        ;; information
        (define-key map [(control c) (control b)] (quote py-submit-bug-report))
        (define-key map [(control c) (control v)] (quote py-version))
        (define-key map [(control c) (control w)] (quote py-pychecker-run))
        ;; (define-key map (kbd "TAB") (quote py-indent-line))
        (define-key map (kbd "TAB") (quote py-indent-line))
	;; (if py-complete-function
        ;;     (progn
        ;;       (define-key map [(meta tab)] py-complete-function)
        ;;       (define-key map [(esc) (tab)] py-complete-function))
        ;;   (define-key map [(meta tab)] (quote py-shell-complete))
        ;;   (define-key map [(esc) (tab)] (quote py-shell-complete)))
        (substitute-key-definition 'complete-symbol 'completion-at-point
                                   map global-map)
        (substitute-key-definition 'backward-up-list (quote py-up)
                                   map global-map)
        (substitute-key-definition 'down-list (quote py-down)
                                   map global-map)
	(when py-use-menu-p
	  (setq map (py-define-menu map)))
        map))

(defvar py-shell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-r" 	  (quote py-nav-last-prompt))
    (define-key map (kbd "RET") 'comint-send-input)
    (define-key map (kbd "TAB") (quote py-indent-line))
    (define-key map [(control c) (!)] (quote py-shell))
    (define-key map [(control c) (-)] (quote py-up-exception))
    (define-key map [(control c) (.)] (quote py-expression))
    (define-key map [(control c) (:)] (quote py-guess-indent-offset))
    (define-key map [(control c) (<)] (quote py-shift-left))
    (define-key map [(control c) (=)] (quote py-down-exception))
    (define-key map [(control c) (>)] (quote py-shift-right))
    (define-key map [(control c) (\#)] (quote py-comment-region))
    (define-key map [(control c) (\?)] (quote py-describe-mode))
    (define-key map [(control c) (control b)] (quote py-submit-bug-report))
    (define-key map [(control c) (control d)] (quote py-pdbtrack-toggle-stack-tracking))
    (define-key map [(control c) (control e)] (quote py-help-at-point))
    (define-key map [(control c) (control k)] (quote py-mark-block-or-clause))
    (define-key map [(control c) (control l)] 'comint-dynamic-list-input-ring)
    (define-key map [(control c) (control n)] (quote py-forward-statement))
    (define-key map [(control c) (control p)] (quote py-backward-statement))
    (define-key map [(control c) (control q)] (quote py-forward-block))
    (define-key map [(control c) (control t)] (quote py-toggle-shell))
    (define-key map [(control c) (control u)] (quote py-backward-block))
    (define-key map [(control c) (control v)] (quote py-version))
    (define-key map [(control c) (control w)] (quote py-pychecker-run))
    (define-key map [(control c) (tab)] (quote py-indent-region))
    (define-key map [(control j)] (quote py-newline-and-indent))
    (define-key map [(control meta a)] (quote py-backward-def-or-class))
    (define-key map [(control meta e)] (quote py-forward-def-or-class))
    (define-key map [(control meta h)] (quote py-mark-def-or-class))
    (define-key map [(control x) (n) (d)] (quote py-narrow-to-def-or-class))
    (define-key map [(meta tab)] (quote py-shell-complete))
    (define-key map [(super backspace)] (quote py-dedent))
    ;; (define-key map "\C-c\C-r" 	  'comint-show-output)
    ;; (define-key map [(control c)(control r)] (quote py-nav-last-prompt))
    (substitute-key-definition 'complete-symbol 'completion-at-point
			       map global-map)
    (substitute-key-definition 'backward-up-list (quote py-up)
			       map global-map)
    (substitute-key-definition 'down-list (quote py-down)
			       map global-map)
    map)
  "Used inside a Python-shell.")

(defvar py-ipython-shell-mode-map py-shell-mode-map
  "Copy ‘py-shell-mode-map’ here.")

(provide 'python-components-map)

;;; python-components-map.el ends here
