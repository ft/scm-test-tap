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
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:export (input->record
            harness
            make-harness-callback
            make-harness-state
            pp-harness-state
            harness-analyse
            harness-deterministic?
            harness-finalise
            harness-process
            harness-plan
            harness-state
            harness-results
            return
            clear-previous
            echo-input
            render-parsed
            progress-completion
            progress-plan
            progress-test))

(define *tap-harness-version* 12)

(define (assq-change alist key value)
  (let loop ((rest alist))
    (match rest
      (((k . v) . rest*)
       (if (eq? k key)
           (cons (cons key value) rest*)
           (cons (cons k v) (loop rest*))))
      (_ (list (cons key value))))))

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
  (define (test-version m) `(version . ,(match->number m 1)))
  (define (test-bailout m) `(bailout (reason . ,(maybe-string m 2))))
  (define (test-diagnostic m) `(diagnostic . ,(match:substring m 1)))
  (or (match-string (input m)
        (version    (test-version m))
        (bailout    (test-bailout m))
        (testplan   (test-plan m))
        (diagnostic (test-diagnostic m))
        (testline   (test-result m)))
      `(unknown . ,input)))

(define-immutable-record-type <harness-callback>
  (make-harness-callback* version bailout plan diagnostic test completion
                          unknown)
  harness-callback?
  (version    cb:version)
  (bailout    cb:bailout)
  (plan       cb:plan)
  (diagnostic cb:diagnostic)
  (test       cb:test)
  (completion cb:completion)
  (unknown    cb:unknown))

(define (default-callback state input parsed) state)

(define* (make-harness-callback #:key
                                (version default-callback)
                                (bailout default-callback)
                                (plan default-callback)
                                (diagnostic default-callback)
                                (test default-callback)
                                (completion default-callback)
                                (unknown default-callback))
  (make-harness-callback* version bailout plan diagnostic test completion
                          unknown))

(define (directive? directive obj)
  (let ((value (assq-ref obj 'directive)))
    (and value (eq? (car value) directive))))

(define (skip? obj)
  (directive? 'skip obj))

(define (todo? obj)
  (directive? 'todo obj))

(define (test-pass? test)
  (assq-ref test 'result))

(define (make-results)
  '((pass) (fail) (todo) (skip) (todo-but-pass) (skip-but-fail)))

(define (finalise-results results)
  (map (lambda (r) (cons (car r) (reverse (cdr r))))
       results))

(define-immutable-record-type <harness-state>
  (make-harness-state* version state auxiliary number plan deterministic? log results)
  harness-state?
  (version          harness-version          change-harness-version)
  (state            harness-state            change-harness-state)
  (auxiliary        harness-auxiliary        change-harness-auxiliary)
  (number           harness-number           change-harness-number)
  (plan             harness-plan             change-harness-plan)
  (deterministic?   harness-deterministic?   change-harness-deterministic?)
  (log              harness-log              change-harness-log)
  (results          harness-results          change-harness-results))

(define* (make-harness-state #:key
                             (version *tap-harness-version*)
                             (state 'init) (auxiliary '()) (number 1)
                             (plan #f) (deterministic? #f)
                             (log '()) (results (make-results)))
  (make-harness-state* version state auxiliary number plan deterministic? log results))

(define (harness-finalise s)
  (set-fields s
              ((harness-state) 'finished)
              ((harness-number) (1- (harness-number s)))
              ((harness-log) (reverse (harness-log s)))
              ((harness-results) (finalise-results (harness-results s)))))

(define (push-result state kind obj)
  (change-harness-results
   state
   (let loop ((rest (harness-results state)))
     (match (car rest)
       ((k . v) (if (eq? k kind)
                    (cons (cons* k obj v) (cdr rest))
                    (cons (car rest) (loop (cdr rest)))))
       (_ (throw 'unknown-result-kind kind))))))

(define (push-harness-log s obj)
  (cons obj (harness-log s)))

(define (push-harness-log* s obj)
  (change-harness-log s (push-harness-log s obj)))

(define (handle-version s version)
  (unless (= version *tap-harness-version*)
    (format #t "# Warning: Test indicates TAP version ~a, this harness implements ~a~%"
            version *tap-harness-version*))
  (change-harness-version s version))

(define (handle-bailout s bailout)
  (case (harness-state s)
    ((finished) (push-harness-log s (cons 'bailout-although-finished bailout)))
    ((init active) (push-harness-log* (change-harness-state s 'finished)
                                      (cons 'bailout bailout)))))

(define (handle-plan s plan)
  (case (harness-state s)
    ((init) (set-fields s
                        ((harness-deterministic?) #t)
                        ((harness-plan) plan)
                        ((harness-state) (if (skip? plan)
                                             'finished
                                             'active))))
    ((active) (cond ((not (harness-deterministic? s))
                     (set-fields s
                                 ((harness-plan) plan)
                                 ((harness-state) 'finished)))
                    (else (push-harness-log*
                           s (cons 'plan-with-existing-plan plan)))))
    ((finished) (push-harness-log* s (cons 'plan-although-finished plan)))))

(define (with-number? test)
  (assq-ref test 'number))

(define (change-test-number test n)
  (let loop ((rest test))
    (cond ((null? rest) rest)
          ((eq? (caar rest) 'number) (cons (cons 'number n)
                                           (cdr rest)))
          (else (cons (car rest) (loop (cdr rest)))))))

(define (add-result result s)
  (let* ((result (if (with-number? result)
                     result
                     (change-test-number result (harness-number s))))
         (kind (if (test-pass? result)
                   (cond ((todo? result) 'todo-but-pass)
                         ((skip? result) 'skip)
                         (else 'pass))
                   (cond ((todo? result) 'todo)
                         ((skip? result) 'skip-but-fail)
                         (else 'fail)))))
    (push-result s kind result)))

(define (harness-number++ s)
  (change-harness-number s (1+ (harness-number s))))

(define (handle-testresult s result)
  (harness-number++
   (add-result
    result
    (case (harness-state s)
      ((init) (set-fields s
                          ((harness-deterministic?) #f)
                          ((harness-state) 'active)))
      ((finished) (push-harness-log s (cons 'result-although-finished result)))
      ((active) s)))))

(define (handle-diagnostic s diagnostic)
  s)

(define (handle-unknown s data)
  s)

(define (handle-broken s obj input)
  (format #t "# Fatal Error! Got broken data!~%")
  (format #t "#   state: ~a~%" s)
  (format #t "#     obj: ~a~%" obj)
  (format #t "#   input: ~s~%" input)
  (format #t "# Please report this issue! Giving up.~%")
  (quit 1))

(define* (harness-process s input #:optional (cb (make-harness-callback)))
  (let* ((parsed (input->record input))
         (p (lambda (k s) ((k cb) s input parsed))))
    (match parsed
      (('version    . version)    (p cb:version    (handle-version    s version)))
      (('bailout    . bailout)    (p cb:bailout    (handle-bailout    s bailout)))
      (('plan       . plan)       (p cb:plan       (handle-plan       s plan)))
      (('diagnostic . diagnostic) (p cb:diagnostic (handle-diagnostic s diagnostic)))
      (('test       . testresult) (p cb:test       (handle-testresult s testresult)))
      (('unknown    . data)       (p cb:unknown    (handle-unknown    s data)))
      (broken (handle-broken s broken input)))))

(define* (harness #:optional (callback (make-harness-callback)))
  (let loop ((state (make-harness-state))
             (input (read-line))
             (results '()))
    (cond ((eof-object? input) (reverse (cons ((cb:completion callback)
                                               (harness-finalise state)
                                               input #f)
                                              results)))
          ((eq? (harness-state state) 'finished)
           (loop (make-harness-state) (read-line) (cons (harness-finalise state)
                                                        results)))
          (else (loop (harness-process state input callback)
                      (read-line) results)))))

(define (pp obj)
  (pretty-print obj
                #:per-line-prefix "    "
                #:width 120
                #:max-expr-width 80))

(define (pp-harness-state s)
  (let ((version (harness-version s))
        (state (harness-state s))
        (number (harness-number s))
        (plan (harness-plan s))
        (deterministic? (harness-deterministic? s))
        (log (harness-log s))
        (results (harness-results s)))
    (format #t "TAP version ~a~%" version)
    (format #t "state: ~a~%" state)
    (format #t "number: ~a~%" number)
    (format #t "plan: ~a (deterministic? ~a)~%" plan deterministic?)
    (format #t "log:~%")     (pp log)
    (format #t "results:~%") (pp results)
    s))

(define (number-of-tests state)
  (apply + (map (compose length cdr)
                (harness-results state))))

(define (harness-analyse-version state)
  (let ((version (harness-version state)))
    (format #t "Test Results (TAP version ~a):~%" version)
    (unless (= version *tap-harness-version*)
      (format #t "Warning: This processor implements version ~a!~%"
              *tap-harness-version*))))

(define (harness-analyse-plan state)
  (let ((tests-that-ran (number-of-tests state))
        (tests-planned (assq-ref (harness-plan state) 'number))
        (deterministic? (harness-deterministic? state)))
    (if deterministic?
        (if (= tests-planned tests-that-ran)
            (format #t "Ran ~a test~p, as planned.~%"
                    tests-planned tests-planned)
            (format #t "~a test~p were planned, but ran ~a, which is ~a!~%"
                    tests-planned tests-planned tests-that-ran
                    (if (< tests-that-ran tests-planned)
                        'less 'more)))
        (format #t "Ran ~a test~p.~%" tests-that-ran tests-that-ran))))

(define (harness-analyse-results state)
  (let* ((results (harness-results state))
         (get (lambda (k) (assq-ref results k)))
         (pnn (lambda (k s)
                (let ((n (length (get k))))
                  (when (positive? n)
                    (format #t "  • ~a of these~a~%" n s))))))
    (format #t "  • ~a of these passed.~%" (length (get 'pass)))
    (format #t "  • ~a of these failed.~%" (length (get 'fail)))
    (pnn 'skip                " were skipped.")
    (pnn 'skip-but-fail       " are marked to SKIP but signaled failure!")
    (pnn 'todo                " were marked as TODO.")
    (pnn 'todo-but-pass       " are marked as TODO but signaled success!")))

(define (harness-analyse-state state)
  (harness-analyse-version state)
  (harness-analyse-plan state)
  (harness-analyse-results state)
  state)

(define* (harness-analyse states #:key (pre-summary (lambda (s) #t)))
  (pre-summary states)
  (map-in-order harness-analyse-state states))

(define (echo-input s i p)
  (display i)
  (newline)
  s)

(define (render-directive d)
  (match d
    ((kind ('reason . r))
     (format #t "# ~a~a~a"
             (string-upcase (symbol->string kind))
             (if r " " "")
             (if r r "")))))

(define (render-parsed s i p)

  (define (add-directive d)
    (when d (display " ") (render-directive d)))

  (define* (maybe obj #:key (prefix " "))
    (when obj
      (display prefix)
      (display obj)))

  (match p
    (('test ('result . result)
            ('number . num)
            ('description . desc)
            ('directive . dir))
     (display (if result "ok" "not ok"))
     (maybe num)
     (maybe desc #:prefix " - ")
     (add-directive dir))

    (('plan ('number . num)
            ('start . start)
            ('end . end)
            ('directive . dir))
     (format #t "~a..~a" start end)
     (add-directive dir))

    (('diagnostic . text) (display "# ") (display text))

    (('bailout ('reason . reason))
     (display "Bail out!")
     (maybe reason))

    (('version . version)
     (display "TAP version ")
     (display version))

    (('unknown . text) (display text)))

  (newline)
  s)

(define (return)
  (display #\return))

(define (clear-previous s)
  (let ((n (assq-ref (harness-auxiliary s) 'last-progress-length)))
    (when n
      (display (make-string n #\space))
      (return))))

(define (progress-string state string)
  (display string)
  (return)
  (change-harness-auxiliary state (assq-change (harness-auxiliary state)
                                               'last-progress-length
                                               (string-length string))))

(define (progress-plan s i p)
  (clear-previous s)
  (let* ((n (assq-ref (harness-plan s) 'number))
         (str (if (harness-deterministic? s)
                  (format #f "Initialising deterministic plan: ~a tests." n)
                  (format #f "Non deterministic plan signals ~a tests." n))))
    (progress-string s str)))

(define (progress-test s i p)
  (clear-previous s)
  (let* ((n (assq-ref (harness-plan s) 'number))
         (m (1- (harness-number s)))
         (str (format #f "Running test ~a/~a" m n)))
    (progress-string s str)))

(define (progress-completion s i p)
  (clear-previous s)
  (if (zero? (length (assq-ref (harness-results s)
                               'fail)))
      (format #t "Test result: ok~%")
      (format #t "Test result: not ok~%"))
  s)
