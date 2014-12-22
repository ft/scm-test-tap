;; Copyright (c) 2014 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap))

(with-test-bundle (test tap simple)
  (no-plan)

  (define-test "test: ="
    (pass-if-= (+ 1 2) 3))

  (define-test "test: not ="
    (pass-if-not-= (+ 1 2) 2))

  (define-test "test: eq?"
    (pass-if-eq? 'same 'same))

  (define-test "test: not eq?"
    (pass-if-not-eq? 'not 'same))

  (define-test "test: eqv?"
    (pass-if-eqv? #\A #\A))

  (define-test "test: not eqv?"
    (pass-if-not-eqv? 1.0 1))

  (define-test "test: equal?"
    (pass-if-equal? (list 1 2 3) (list 1 2 3)))

  (define-test "test: not equal?"
    (pass-if-not-equal? (list 1 2 3) (list 1 3 2)))

  (define-test "test: string=?"
    (pass-if-string=? "same" "same"))

  (define-test "test: not string=?"
    (pass-if-not-string=? "not" "same"))

  (define-test "test: string-ci=?"
    (pass-if-string-ci=? "SamE" "sAMe"))

  (define-test "test: not string-ci=?"
    (pass-if-not-string-ci=? "nOt" "same"))

  (define-test "test: re-match"
    (pass-if-re-match "^Does.*match" "Does this match?"))

  (define-test "test: not re-match"
    (pass-if-not-re-match "^Does not.*match" "Does this match?"))

  (define-test "test: true"
    (pass-if-true #t))

  (define-test "test: not true"
    (pass-if-false #f)))
