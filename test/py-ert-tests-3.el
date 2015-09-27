;; py-ert-tests-3.el --- Some more Tests

;; Copyright (C) 2014 Andreas Roehler, <andreas.roehler@online.de>

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; tests are expected to run from directory test

(add-to-list 'load-path default-directory)
(load "py-ert-tests-1.el" nil t)


;; py-if-name-main-permission-p
(ert-deftest py-ert-if-name-main-permission-lp-326620-test ()
  (py-test-with-temp-buffer-point-min
      "#! /usr/bin/env python
# -*- coding: utf-8 -*-
def py_if_name_main_permission_test():
    if __name__ == \"__main__\" :
        print(\"__name__ == '__main__' run\")
        return True

    else:
        print(\"__name__ == '__main__' supressed\")
        return False

py_if_name_main_permission_test()
"
    (let ((py-if-name-main-permission-p t))
      (py-execute-buffer)
      (set-buffer "*Python*")
      (goto-char (point-max))
      (forward-line -1)
      (end-of-line)
      (sit-for 0.2)
      (assert (looking-back "run") nil "py-if-name-main-permission-lp-326620-test #1 failed"))))


(ert-deftest py-ert-intend-try-test ()
  (py-test-with-temp-buffer-point-min
      "#! /usr/bin/env python

import sys
import os

        try:"
    (search-forward "try")
    (should (eq 0 (py-compute-indentation)))))

(ert-deftest py-ert-find-definition-test-1 ()
  (py-test-with-temp-buffer-point-min
      "#! /usr/bin/env python

import sys
import os

a = sys.argv"
    (search-forward "a = sy")
    (py-find-definition)))

(ert-deftest py-ert-multiple-decorators-test-1 ()
  (py-test-with-temp-buffer
      "@blah
@blub
def foo():
    pass
"
    (let ((py-mark-decorators t))
      (py-beginning-of-def-or-class)
      (should (bobp)))))

(ert-deftest py-ert-multiple-decorators-test-2 ()
  (py-test-with-temp-buffer
      "@blah
@blub
def foo():
    pass
"
    (let* (py-mark-decorators
	   (erg (py-beginning-of-def-or-class)))
      (should (eq 13 erg)))))

;; (ert-deftest py-ert-find-definition-test-2 ()
;;   (py-test-with-temp-buffer
;;       "#! /usr/bin/env python

;; import sys
;; import os

;; def foo ()
;;     pass

;; foo()"
;;     (beginning-of-line)
;;     (sit-for 0.1)
;;     (should (string= "def foo" (py-find-definition)))))

(ert-deftest py-ert-async-backward-block-test ()
  (py-test-with-temp-buffer
      "async def coro(name, lock):
    print('coro {}: waiting for lock'.format(name))
    async with lock:
        print('coro {}: holding the lock'.format(name))
        await asyncio.sleep(1)
        print('coro {}: releasing the lock'.format(name))"
    (py-backward-block)
    (should (looking-at "async with"))))

(ert-deftest py-ert-async-backward-def-test ()
  (py-test-with-temp-buffer
      "async def coro(name, lock):
    print('coro {}: waiting for lock'.format(name))
    async with lock:
        print('coro {}: holding the lock'.format(name))
        await asyncio.sleep(1)
        print('coro {}: releasing the lock'.format(name))"
    (py-backward-def)
    (should (looking-at "async def"))))

(provide 'py-ert-tests-3)
;;; py-ert-tests-3.el ends here
