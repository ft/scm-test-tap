;; Copyright 2021 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Redistribution  and  use  in  source  and  binary  forms,  with  or  without
;; modification, are permitted provided that the following conditions are met:
;;
;;  1. Redistributions of source code must retain the above copyright notice,
;;     this list of conditions and the following disclaimer.
;;
;;  2. Redistributions  in binary  form must  reproduce the  above  copyright
;;     notice, this  list of conditions  and the following disclaimer  in the
;;     documentation and/or other materials provided with the distribution.
;;
;;  THIS SOFTWARE IS PROVIDED  "AS IS"  AND ANY EXPRESS OR  IMPLIED WARRANTIES,
;;  INCLUDING, BUT  NOT LIMITED TO,  THE IMPLIED WARRANTIES  OF MERCHANTABILITY
;;  AND FITNESS FOR A PARTICULAR PURPOSE  ARE DISCLAIMED. IN NO EVENT SHALL THE
;;  AUTHOR OR CONTRIBUTORS  OF THE PROJECT BE LIABLE FOR  ANY DIRECT, INDIRECT,
;;  INCIDENTAL, SPECIAL,  EXEMPLARY, OR  CONSEQUENTIAL DAMAGES  (INCLUDING, BUT
;;  NOT LIMITED TO,  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;  LOSS OF USE,
;;  DATA,  OR PROFITS;  OR BUSINESS  INTERRUPTION)  HOWEVER CAUSED  AND ON  ANY
;;  THEORY  OF  LIABILITY,  WHETHER  IN CONTRACT,  STRICT  LIABILITY,  OR  TORT
;;  (INCLUDING NEGLIGENCE  OR OTHERWISE) ARISING IN  ANY WAY OUT OF  THE USE OF
;;  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(define-module (test tap-harness)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:export (input->record))

(define (match-string-fn obj . lst)
  (call/ec
   (lambda (return)
     (fold (lambda (el acc)
             (match el
               ((pattern . callback)
                (let ((result (string-match pattern obj)))
                  (when result
                    (return (callback result))))))
             acc)
           #f lst))))

(define-syntax-rule (match-string (obj capture) (pattern e0 e* ...) ...)
  (match-string-fn obj (cons pattern (lambda (capture) e0 e* ...)) ...))

(define (match->number m i)
  (string->number (match:substring m i)))

(define (maybe-string m i)
  (let ((obj (match:substring m i)))
    (and (string? obj)
         (not (string-null? obj))
         obj)))

(define (test-directive s)
  (match-string (s m)
    ("[ \t]+#[ \t]+TODO(|[ \t]+(.*))$"        `(todo (reason . ,m)))
    ("[ \t]+#[ \t]+SKIP[^ \t]*(|[ \t]+(.*))$" `(skip (reason . ,m)))))

(define (test-suffix s)
  (if (and (string? s) (not (string-null? s)))
      (match (test-directive s)
        ((directive ('reason . m))
         (cons (match:prefix m)
               `(,directive (reason . ,(maybe-string m 2)))))
        (_ (cons s #f)))
      (cons #f #f)))

(define (test-result m)
  (let ((result (match:substring m 1))
        (snumber (match:substring m 3))
        (desc+direct (test-suffix (match:substring m 5))))
    `(test (result . ,(string= "ok" result))
           (number . ,(and snumber (string->number snumber)))
           (description . ,(car desc+direct))
           (directive . ,(cdr desc+direct)))))

(define (test-plan m)
  (let* ((start (match->number m 1))
         (end (match->number m 2))
         (number (1+ (- end start)))
         (skip? (match:substring m 4))
         (reason (match:substring m 5))
         (directive (if (or (<= number 0) skip?)
                        `(skip (reason . ,(and reason
                                               (not (string-null? reason))
                                               reason)))
                        #f)))
    `(plan (number . ,number)
           (start . ,start)
           (end . ,end)
           (directive . ,directive))))

(define version    "^TAP version ([0-9]+)$")
(define bailout    "^Bail out!(|[ \t]+(.*))$")
(define testplan   "^([0-9]+)\\.\\.([0-9]+)(|[ \t]+#[ \t]+SKIP[^ \t]*(|[ \t]+(.*)))$")
(define diagnostic "^# ?(.*)$")
(define testline   "^(ok|not ok)(| +([0-9]+))( -[ \t]+|[ \t]+|)(.*)$")

(define (input->record input)
  (or (match-string (input m)
        (version    `(version . ,(match->number m 1)))
        (bailout    `(bail-out (reason . ,(maybe-string m 2))))
        (testplan   (test-plan m))
        (diagnostic `(diagnostic . ,(match:substring m 1)))
        (testline   (test-result m)))
      `(unknown . ,input)))
