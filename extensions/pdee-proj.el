(define-derived-mode unittest-mode 
  fundamental-mode
  "Unittest"
  "Mode to display and analyze unittest output"
  
  )


(defun unittest-discover ()
  "Discover and run tests from the current directory and down"
  (interactive)
  
  ()
  )

(defun testdiscover ()
  ""
  (interactive)
  (pymacs-load "unitdiscover" "unitdiscover-")
  (unitdiscover-discover "~/workspace/aiccm"))

(global-set-key (kbd "<f9>") 'testdiscover)
(defun make-link (text cmd)
  "Return a TEXT propertized as a link that calls CMD when clicked"
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] cmd)
    (define-key map (kbd "RET") cmd)
    (propertize text
		'keymap map
		'face 'underline
		'mouse-face 'highlight
		'rear-nonsticky t
		'read-only t)))