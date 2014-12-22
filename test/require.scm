;; Copyright (c) 2014 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap))

(with-test-bundle (test tap todo)
  (plan 2)
  ;; Make the ‘test’ argument fail, so this bundle gets skipped.
  (require (eq? 'this 'fails))
  (define-test "First test"
    (pass-if-= 1 1))
  (define-test "Second test"
    (pass-if-not-= 2 1)))
