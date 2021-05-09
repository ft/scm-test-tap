;; Copyright (c) 2016-2020 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap))

(define-tap-test (pass-if-string? datum) (string? datum))
(define-tap-test (pass-if-valid-type? datum) (or (string? datum)
                                                 (number? datum)))

(with-test-bundle (example diagnostics custom-primitives)
  (plan 3)
  (define-test "Custom macro pass-if-string? works"
    (pass-if-string? "123"))
  (define-test "Custom macro pass-if-valid-type? likes strings"
    (pass-if-valid-type? "123"))
  (define-test "Custom macro pass-if-valid-type? likes numbers"
    (pass-if-valid-type? 4+2i)))
