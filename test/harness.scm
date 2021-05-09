;; Copyright (c) 2021 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (ice-9 match)
             (ice-9 regex)
             (srfi srfi-1)
             (test tap)
             (test tap-harness))

(define debug? #f)

(define (d:eq? a b)
  (when debug?
    (format #t "# (eq? a b)~%# a: ~a~%# b: ~a~%" a b))
  (eq? a b))

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
    ("ok 23 - foo bar # baz <HASH> SKIP" .
     (test (result . #t)
           (number . 23)
           (description . "foo bar # baz")
           (directive skip (reason . #f))))
    ("ok 23 - foo bar # baz <HASH> SKIP   Some Reason" .
     (test (result . #t)
           (number . 23)
           (description . "foo bar # baz")
           (directive skip (reason . "Some Reason"))))
    ("not ok 23 - foo bar # baz <HASH> TODO" .
     (test (result . #f)
           (number . 23)
           (description . "foo bar # baz")
           (directive todo (reason . #f))))
    ("not ok 23 - foo bar # baz <HASH> TODO Some Reason" .
     (test (result . #f)
           (number . 23)
           (description . "foo bar # baz")
           (directive todo (reason . "Some Reason"))))
    ;; Version lines
    ("TAP version 12" . (version . 12))
    ("TAP version 13" . (version . 13))
    ;; Bail out
    ("Bail out!" . (bailout (reason . #f)))
    ("Bail out! Reason here!" . (bailout (reason . "Reason here!")))
    ("Bail out!    Reason here!" . (bailout (reason . "Reason here!")))
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
    ("1..0 <HASH> SKIP" .
     (plan (number . 0)
           (start . 1)
           (end . 0)
           (directive skip (reason . #f))))
    ("1..0 <HASH> SKIP Some Reason!" .
     (plan (number . 0)
           (start . 1)
           (end . 0)
           (directive skip (reason . "Some Reason!"))))
    ("1..0 <HASH> SKIP    Some Reason!" .
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

(define (hm str)
  (let ((m (string-match "<HASH>" str)))
    (if m (regexp-substitute #f m 'pre "#" 'post) str)))

(define processor-tests
  `((plan-exists ,(make-harness-state) ("1..23") ,harness-plan)
    (init-state ,(make-harness-state) ()
                ,(lambda (s) (d:eq? 'init (harness-state s))))
    (deterministic-plan ,(make-harness-state)
                        ("1..23")
                        ,harness-deterministic?)
    (bailout-finishes ,(make-harness-state)
                      ("Bail out!")
                      ,(lambda (s) (eq? 'finished (harness-state s))))
    (non-deterministic-plan-finishes ,(make-harness-state)
                                     ("ok" "not ok" "ok" "1..3")
                                     ,(lambda (s) (eq? 'finished (harness-state s))))))

(with-test-bundle (test harness)
  (plan (+ (length input-tests)
           (length processor-tests)))
  (for-each
   (lambda (t)
     (define-test (format #f "TAP input parses as expected: ~s" (car t))
       (pass-if-equal? (input->record ((compose hm car) t))
                       (cdr t))))
   input-tests)

  (for-each
   (lambda (t)
     (define-test (format #f "TAP processor step works: ~a" (car t))
       (match (cdr t)
         ((state (input ...) callback)
          (pass-if-true (let ((final (fold (lambda (e s)
                                             (harness-process s e))
                                           state
                                           input)))
                          (when debug?
                            (format #t "# ~s~%" final))
                          (callback final)))))))
   processor-tests))
