;; Copyright (c) 2016 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap))

;; This is used to get a diagnose for a test, that is marked TODO because it is
;; known to fail, to let the whole suite pass.
(tap/set-option 'tap-todo-prints-diagnose #t)

(with-test-bundle (test tap exceptions)
  (plan 5)
  (define-test "Can catch any exception"
    (pass-if-any-exception (throw 'fack)))
  (define-test "Can also catch standard exceptions"
    (pass-if-any-exception (= "Haha. Sure." 23)))
  (define-test "Can also catch standard exceptions by name"
    (pass-if-exception 'wrong-type-arg (= "Haha. Sure." 23)))
  (todo (define-test "Expecting the wrong exception is informative"
          (pass-if-exception 'unbound-variable (= "Haha. Sure." 23))))
  (define-test "Can test for no exception, too"
    (pass-if-no-exception (+ 1 2))))
