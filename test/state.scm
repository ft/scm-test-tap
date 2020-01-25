;; Copyright (c) 2016-2020 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap))

(define counter #f)

(let ((state 0))
  (set! counter
    (lambda ()
      (let ((old state)
            (new (+ state 1)))
        (set! state new)
        old))))

(with-test-bundle (test tap state)
  (plan 3)

  (define-test "Use a counter with internal state works"
    (pass-if-= (counter) 0))

  (define-test "Use the next value from said counter is correct"
    (pass-if-= (counter) 1))

  (define-test "...and a third time for good measure"
    (pass-if-= (counter) 2)))
