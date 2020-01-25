;; Copyright (c) 2014-2020 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap))

(with-test-bundle (test tap todo)
  (plan 3)
  (todo (define-test "This is a TODO test"
          (pass-if-= 0 1))
        (define-test "This is another TODO test"
          (pass-if-= 1 0))
        )
  (define-test "This one is NOT a TODO test"
    (pass-if-= 1 1)))
