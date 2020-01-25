;; Copyright (c) 2016-2020 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap))

(define-tap-test (pass-if-string? datum) (string? datum))
(define-tap-test (pass-if-valid-type? datum) (or (string? datum)
                                                 (number? datum)))

(with-test-bundle (example diagnostics custom-primitives)
  (no-plan)
  (define-test "We can define new primitives: pass-if-string?"
    (pass-if-string? 123))
  (define-test "Costum macro pass-if-valid-type? dislikes lists"
    (pass-if-valid-type? (list 'a 'b 'c (+ 1 2)))))
