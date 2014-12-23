;; Copyright (c) 2014 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap))

(with-test-bundle (test tap skip individual)
  (plan 3)
  (define-test skip "This test will be skipped"
    (pass-if-true (format #t "not ok 0 - This should not be printed.~%")))
  (define-test skip "This test will be skipped as well"
    (pass-if-true (format #t "not ok 0 - Neither should this...~%")))
  (define-test "This one will NOT be skipped"
    (pass-if-= 1 1)))
