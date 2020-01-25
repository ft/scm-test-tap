;; Copyright (c) 2016-2020 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap))

(with-test-bundle (test tap graceful)
  (plan 4)
  (tap/comment "This set of tests, checks if the framework handles unexpected")
  (tap/comment "exceptions in a graceful - maybe even useful - manner.")
  (tap/comment "")
  (tap/comment "All these tests fail for that reason and are marked as TODO,")
  (tap/comment "to make sure the suite doesn't count them as actual errors.")
  (tap/comment "")
  (todo (define-test "Wrong argument type"
          (pass-if-= "Haha. Sure." 23)))
  (todo (define-test "Throwing an expection, unexpectedly."
          (pass-if-true (throw 'fack))))
  (todo (define-test "Unbound variable: access"
          (pass-if-false (access "DOES-NOT-EXIST"))))
  (todo (define-test "Wrong number of arguments to access?"
          (pass-if-true (access? "DOES-NOT-EXIST")))))
