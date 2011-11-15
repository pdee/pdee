;; This file is a great big shortcut for all the features contained in emacs-for-python

;; Trick to get the filename of the installation directory

(add-to-list 'load-path pdee-install-dir)
(require 'pdee-setup)
(require 'pdee-python)
(require 'pdee-completion)
(require 'pdee-editing)
(require 'pdee-bindings)

(provide 'pdee-init)
