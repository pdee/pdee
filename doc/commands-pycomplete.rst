Python-mode commands

====================

py-complete-which-class
-----------------------
Return current class name based on point.
If no class name is found, return nil.

py-complete
-----------
Complete symbol before point using Pymacs. 

py-complete-help
----------------
get help on a python expression

py-complete-help-thing-at-point
-------------------------------


py-complete-signature-expr
--------------------------


py-complete-electric-lparen
---------------------------
electricly insert '(', and try to get a signature for the stuff to the left

py-complete-electric-comma
--------------------------
electricly insert ',', and redisplay latest signature

py-complete-goto-definition
---------------------------
Got to definition of Python function.

py-complete-set-keymap
----------------------
Define key map with pycomplete functions.

py-complete-initialize
----------------------
Initialize pycomplete hooks and key map.
Should be called from python-mode-hook.

