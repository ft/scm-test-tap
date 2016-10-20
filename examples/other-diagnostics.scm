;; Copyright (c) 2016 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap))

(with-test-bundle (example diagnostics other)
  (no-plan)

  (define-test "pass-if-=: Wrong number of arguments"
    (pass-if-= 1 1 23))

  (define-test "Show diagnostics: Wrong types"
    (pass-if-= "1" 1))

  (define-test "Show diagnostics: unbound variable"
    (pass-if-= does-not-exist 1))

  (define-test "Show diagnostics: Wrong use of a procedure"
    (pass-if-true (cons 1 2 3)))

  (define-test "Show diagnostics: More than one exception"
    (pass-if-= (+ "2" 2) (- "4" 2)))

  (define-test "Show diagnostics: Unexpected exception"
    (pass-if-true (throw 'crap)))

  (define-test "Show diagnostics: Late exception"
    (pass-if-= "2" 2)))
