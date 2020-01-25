;; Copyright (c) 2016-2020 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap))

(with-test-bundle (example diagnostics exceptions)
  (no-plan)

  (define-test "pass-if-any-exception: Show diagnostics"
    (pass-if-any-exception (+ 1 1)))

  (define-test "pass-if-no-exception: Show diagnostics"
    (pass-if-no-exception (throw 'too-bad)))

  (define-test "pass-if-exception: Show diagnostics"
    (pass-if-exception 'too-bad! (+ 1 2 3 (/ (throw 'too-bad) 2.0)))))
