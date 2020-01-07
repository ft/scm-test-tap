;; Load this file into emacs for proper indentation of the project's macros
;; within emacs' `scheme-mode'.
;;
;; This file is part of `scm-test-tap'.

(mapc (lambda (thing)
        (put (car thing)
             'scheme-indent-function
             (cdr thing)))
      '((for-each-test . 1)
        (with-test-bundle . 1)
        (with-fs-test-bundle . 1)
        (with-ellipsis . 1)
        (set-record-type-printer! . 1)
        (define-test . 1)))
