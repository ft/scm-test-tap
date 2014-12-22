;; Copyright (c) 2014 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap))

(force-import (test tap) *plan* *test-pp-width*)
(define labels (make-labeled-values *plan* *test-pp-width*))

(with-test-bundle (test tap helpers)
  (plan (+ 2 (length labels)))
  (define-test "Is *plan* okay?"
    (pass-if-true (or (boolean? *plan*)
                      (symbol? *plan*))))
  (define-test "Default pp-width okay?"
    (pass-if-= *test-pp-width* 60))
  (tap/comment "This is a comment...")
  (map (lambda (x)
         (define-test (format #f "label: ~a, value: ~a" (car x) (cdr x))
           (pass-if-true #t)))
       labels))
