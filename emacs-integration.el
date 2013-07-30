;; Load this file into emacs for proper indentation of the project's macros
;; within emacs' `scheme-mode'.
;;
;; This file is part of `scm-test-tap'.

(mapc (lambda (thing)
        (put (car thing)
             'scheme-indent-function
             (cdr thing)))
      '((with-test-bundle . 1)
        (define-test . 1)))
