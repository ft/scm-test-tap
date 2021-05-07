;; Copyright (c) 2021 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test tap-harness))

(define input-tests
  '( ;; Test lines: (ok, not ok... stuff like that)
    ("ok" .
     (test (result . #t)
           (number . #f)
           (description . #f)
           (directive . #f)))
    ("not ok" .
     (test (result . #f)
           (number . #f)
           (description . #f)
           (directive . #f)))
    ("ok 23" .
     (test (result . #t)
           (number . 23)
           (description . #f)
           (directive . #f)))
    ("not ok 23" .
     (test (result . #f)
           (number . 23)
           (description . #f)
           (directive . #f)))
    ("ok 23    " .
     (test (result . #t)
           (number . 23)
           (description . #f)
           (directive . #f)))
    ("ok 23 foo bar" .
     (test (result . #t)
           (number . 23)
           (description . "foo bar")
           (directive . #f)))
    ("ok 23 - foo bar" .
     (test (result . #t)
           (number . 23)
           (description . "foo bar")
           (directive . #f)))
    ("ok 23 - foo bar # baz" .
     (test (result . #t)
           (number . 23)
           (description . "foo bar # baz")
           (directive . #f)))
    ("ok 23 - foo bar # baz # SKIP" .
     (test (result . #t)
           (number . 23)
           (description . "foo bar # baz")
           (directive skip (reason . #f))))
    ("ok 23 - foo bar # baz # SKIP   Some Reason" .
     (test (result . #t)
           (number . 23)
           (description . "foo bar # baz")
           (directive skip (reason . "Some Reason"))))
    ("not ok 23 - foo bar # baz # TODO" .
     (test (result . #f)
           (number . 23)
           (description . "foo bar # baz")
           (directive todo (reason . #f))))
    ("not ok 23 - foo bar # baz # TODO Some Reason" .
     (test (result . #f)
           (number . 23)
           (description . "foo bar # baz")
           (directive todo (reason . "Some Reason"))))
    ;; Version lines
    ("TAP version 12" . (version . 12))
    ("TAP version 13" . (version . 13))
    ;; Bail out
    ("Bail out!" . (bail-out (reason . #f)))
    ("Bail out! Reason here!" . (bail-out (reason . "Reason here!")))
    ("Bail out!    Reason here!" . (bail-out (reason . "Reason here!")))
    ;; The plan
    ("1..23" .
     (plan (number . 23)
           (start . 1)
           (end . 23)
           (directive . #f)))
    ("1..0" .
     (plan (number . 0)
           (start . 1)
           (end . 0)
           (directive skip (reason . #f))))
    ("1..0 # SKIP" .
     (plan (number . 0)
           (start . 1)
           (end . 0)
           (directive skip (reason . #f))))
    ("1..0 # SKIP Some Reason!" .
     (plan (number . 0)
           (start . 1)
           (end . 0)
           (directive skip (reason . "Some Reason!"))))
    ("1..0 # SKIP    Some Reason!" .
     (plan (number . 0)
           (start . 1)
           (end . 0)
           (directive skip (reason . "Some Reason!"))))
    ;; Diagnostics
    ("#Some text here"   . (diagnostic . "Some text here"))
    ("# Some text here"  . (diagnostic . "Some text here"))
    ("#  Some text here" . (diagnostic . " Some text here"))
    ;; Unknown lines
    ("" . (unknown . ""))
    ("This is nothing TAP knows" . (unknown . "This is nothing TAP knows"))))

(with-test-bundle (test harness)
  (plan (length input-tests))
  (for-each
   (lambda (t)
     (define-test (format #f "TAP input parses as expected: ~s" (car t))
       (pass-if-equal? (input->record (car t))
                       (cdr t))))
   input-tests))
