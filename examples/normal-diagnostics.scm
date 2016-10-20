;; Copyright (c) 2016 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap))

(with-test-bundle (example diagnostics normal)
  (plan 2)

  (define-test "pass-if-=: Show diagnostics, part 1 (Wrong no. of args)"
    (pass-if-= 1 1 23))

  (define-test "pass-if-=: Show diagnostics, part 2"
    (pass-if-= 1 0))

  (define-test "pass-if-=: Show diagnostics, part 3"
    (pass-if-= (/ (+ 1 3) 2) 0))

  (define-test "pass-if-~=: Show diagnostics, part 1"
    (pass-if-~= 1 0 0.5))

  (define-test "pass-if-~=: Show diagnostics, part 2"
    (pass-if-~= (/ (+ 1 3) 2) 0 0.5)))
