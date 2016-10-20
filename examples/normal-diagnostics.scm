;; Copyright (c) 2016 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap))

(with-test-bundle (example diagnostics normal)
  (no-plan)

  (define-test "pass-if-=: Show diagnostics, part 1"
    (pass-if-= 1 0))

  (define-test "pass-if-=: Show diagnostics, part 2"
    (pass-if-= (/ (+ 1 3) 2) 0))

  (define-test "pass-if-~=: Show diagnostics, part 1"
    (pass-if-~= 1 0 0.5))

  (define-test "pass-if-~=: Show diagnostics, part 2"
    (pass-if-~= (/ (+ 1 3) 2) 0 0.5))

  (define-test "pass-if-eq?: Show diagnostics, part 1"
    (pass-if-eq? 'certainly 'not))

  (define-test "pass-if-eq?: Show diagnostics, part 2"
    (pass-if-eq? (symbol-append 'cert 'ainly) 'not))

  (define-test "pass-if-eq?: Show diagnostics, part 1"
    (pass-if-eq? 'certainly 'not))

  (define-test "pass-if-eq?: Show diagnostics, part 2"
    (pass-if-eq? (symbol-append 'cert 'ainly) 'not))

  (define-test "pass-if-eqv?: Show diagnostics, part 1"
    (pass-if-eqv? 1 1.0))

  (define-test "pass-if-eqv?: Show diagnostics, part 2"
    (pass-if-eqv? 1 (/ 2.0 2)))

  (define-test "pass-if-equal?: Show diagnostics, part 1"
    (pass-if-equal? (list 'a 'b 'c) '(a b c (e f) g)))

  (define-test "pass-if-equal?: Show diagnostics, part 2"
    (pass-if-equal? '(what (ev (er (the hell) goes) on) around here)
                    '(sure thing)))

  (define-test "pass-if-string=?: Show diagnostics, part 1"
    (pass-if-string=? "Not" "the same"))

  (define-test "pass-if-string=?: Show diagnostics, part 2"
    (pass-if-string=? "different strings" "Entirely."))

  (define-test "pass-if-string-ci=?: Show diagnostics, part 1"
    (pass-if-string-ci=? "the Same?" "the same!"))

  (define-test "pass-if-string-ci=?: Show diagnostics, part 2"
    (pass-if-string-ci=? "different strings" "still."))

  (define-test "pass-if-re-match: Show diagnostics"
    (pass-if-re-match "The.*!" "the same!"))

  (define-test "pass-if-true: Show diagnostics"
    (pass-if-true #f))

  (define-test "pass-if-false: Show diagnostics"
    (pass-if-false (member 'ever '(I wonder if anyone ever sees this.)))))
