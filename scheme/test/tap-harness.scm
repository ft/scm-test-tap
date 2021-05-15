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
  #:use-module (ice-9 popen)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-harness-callback
            make-bundle-state
            input->record
            harness-run
            harness-stdin
            harness-analyse
            pp-harness-state
            pp-bundle-state
            bundle-deterministic?
            bundle-finalise
            bundle-plan
            bundle-state
            bundle-results
            tap-process
            return
            clear-previous
            echo-input
            render-parsed
            progress-completion
            progress-plan
            progress-test))

(define *tap-harness-version* 12)
(define *progress-column* 50)

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

(define-immutable-record-type <bundle-state>
  (make-bundle-state* name title version
                      state auxiliary
                      number plan deterministic?
                      log results outcome)
  bundle-state?
  (name             bundle-name             change-bundle-name)
  (title            bundle-title            change-bundle-title)
  (version          bundle-version          change-bundle-version)
  (state            bundle-state            change-bundle-state)
  (auxiliary        bundle-auxiliary        change-bundle-auxiliary)
  (number           bundle-number           change-bundle-number)
  (plan             bundle-plan             change-bundle-plan)
  (deterministic?   bundle-deterministic?   change-bundle-deterministic?)
  (log              bundle-log              change-bundle-log)
  (results          bundle-results          change-bundle-results)
  (outcome          bundle-outcome          change-bundle-outcome))

(define* (make-bundle-state #:key
                            (name 'test) (title #f)
                            (version *tap-harness-version*)
                            (state 'init) (auxiliary '()) (number 1)
                            (plan #f) (deterministic? #f)
                            (log '()) (results (make-results))
                            (outcome '(pass)))
  (make-bundle-state* name title version
                      state auxiliary
                      number plan deterministic?
                      log results outcome))

(define (make-bundle-outcome s)
  (define (count k) (length (assq-ref (bundle-results s) k)))
  (let ((number-of-tests (if (bundle-deterministic? s)
                             (assq-ref (bundle-plan s) 'number)
                             (bundle-number s)))
        (n-pass (count 'pass))
        (n-fail (count 'fail))
        (n-skip (count 'skip))
        (n-todo (count 'todo))
        (n-skip-but-fail (count 'skip-but-fail))
        (n-todo-but-pass (count 'todo-but-pass)))
    (let ((n-combined (+ n-pass n-fail n-skip n-todo
                         n-skip-but-fail n-todo-but-pass)))
      (apply append (list (cond ((skip? (bundle-plan s)) '(skip))
                                ((not (= number-of-tests n-combined)) '(fail))
                                ((zero? n-fail) '(pass))
                                (else '(fail)))
                          (if (zero? n-todo-but-pass)
                              '()
                              '(todo-but-pass))
                          (if (zero? n-skip-but-fail)
                              '()
                              '(skip-but-fail)))))))

(define (bundle-finalise s)
  (let ((next (set-fields
               s
               ((bundle-state) 'finished)
               ((bundle-number) (1- (bundle-number s)))
               ((bundle-log) (reverse (bundle-log s)))
               ((bundle-results) (finalise-results (bundle-results s))))))
    (set-field next (bundle-outcome) (make-bundle-outcome next))))

(define (push-result state kind obj)
  (change-bundle-results
   state
   (let loop ((rest (bundle-results state)))
     (match (car rest)
       ((k . v) (if (eq? k kind)
                    (cons (cons* k obj v) (cdr rest))
                    (cons (car rest) (loop (cdr rest)))))
       (_ (throw 'unknown-result-kind kind))))))

(define (push-bundle-log s obj)
  (cons obj (bundle-log s)))

(define (push-bundle-log* s obj)
  (change-bundle-log s (push-bundle-log s obj)))

(define (handle-version s version)
  (unless (= version *tap-harness-version*)
    (format #t "# Warning: Test indicates TAP version ~a, this harness implements ~a~%"
            version *tap-harness-version*))
  (change-bundle-version s version))

(define (handle-bailout s bailout)
  (case (bundle-state s)
    ((finished) (push-bundle-log s (cons 'bailout-although-finished bailout)))
    ((init active) (push-bundle-log* (change-bundle-state s 'finished)
                                     (cons 'bailout bailout)))))

(define (handle-plan s plan)
  (case (bundle-state s)
    ((init) (set-fields s
                        ((bundle-deterministic?) #t)
                        ((bundle-plan) plan)
                        ((bundle-state) (if (skip? plan)
                                             'finished
                                             'active))))
    ((active) (cond ((not (bundle-deterministic? s))
                     (set-fields s
                                 ((bundle-plan) plan)
                                 ((bundle-state) 'finished)))
                    (else (push-bundle-log*
                           s (cons 'plan-with-existing-plan plan)))))
    ((finished) (push-bundle-log* s (cons 'plan-although-finished plan)))))

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
                     (change-test-number result (bundle-number s))))
         (kind (if (test-pass? result)
                   (cond ((todo? result) 'todo-but-pass)
                         ((skip? result) 'skip)
                         (else 'pass))
                   (cond ((todo? result) 'todo)
                         ((skip? result) 'skip-but-fail)
                         (else 'fail)))))
    (push-result s kind result)))

(define (bundle-number++ s)
  (change-bundle-number s (1+ (bundle-number s))))

(define (handle-testresult s result)
  (bundle-number++
   (add-result
    result
    (case (bundle-state s)
      ((init) (set-fields s
                          ((bundle-deterministic?) #f)
                          ((bundle-state) 'active)))
      ((finished) (push-bundle-log s (cons 'result-although-finished result)))
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

(define* (tap-process s input #:optional (cb (make-harness-callback)))
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

(define (run-test p r)
  (if r
      (open-pipe* OPEN_READ r p)
      (open-pipe p OPEN_READ)))

(define (program->state p r callback)
  (let ((port (run-test p r)))
    (let loop ((state (make-bundle-state #:name p)) (input (read-line port)))
      (if (eof-object? input)
          ((cb:completion callback) (bundle-finalise state) input #f)
          (loop (tap-process state input callback) (read-line port))))))

(define* (harness-run #:key
                      (run-programs '())
                      (runner #f)
                      (callback (make-harness-callback)))
  (map-in-order (lambda (p) (program->state p runner callback))
                run-programs))

(define* (harness-stdin #:optional (callback (make-harness-callback)))
  (let loop ((state (make-bundle-state))
             (input (read-line))
             (results '()))
    (cond ((eof-object? input) (reverse (cons ((cb:completion callback)
                                               (bundle-finalise state)
                                               input #f)
                                              results)))
          ((eq? (bundle-state state) 'finished)
           (loop (make-bundle-state) (read-line) (cons (bundle-finalise state)
                                                       results)))
          (else (loop (tap-process state input callback)
                      (read-line) results)))))

(define (pp obj)
  (pretty-print obj
                #:per-line-prefix "    "
                #:width 120
                #:max-expr-width 80))

(define (pp-bundle-state s)
  (let ((version (bundle-version s))
        (state (bundle-state s))
        (number (bundle-number s))
        (plan (bundle-plan s))
        (deterministic? (bundle-deterministic? s))
        (log (bundle-log s))
        (results (bundle-results s))
        (outcome (bundle-outcome s)))
    (format #t "TAP version ~a~%" version)
    (format #t "state: ~a~%" state)
    (format #t "number: ~a~%" number)
    (format #t "plan: ~a (deterministic? ~a)~%" plan deterministic?)
    (format #t "log:~%")     (pp log)
    (format #t "results:~%") (pp results)
    (format #t "outcome:~%") (pp outcome)
    s))

(define (pp-harness-state s)
  (map-in-order pp-bundle-state s))

(define (number-of-tests state)
  (apply + (map (compose length cdr)
                (bundle-results state))))

(define (bundle-analyse-version state)
  (let ((version (bundle-version state)))
    (format #t "Test Results (TAP version ~a):~%" version)
    (unless (= version *tap-harness-version*)
      (format #t "Warning: This processor implements version ~a!~%"
              *tap-harness-version*))))

(define (bundle-analyse-plan state)
  (let ((tests-that-ran (number-of-tests state))
        (tests-planned (assq-ref (bundle-plan state) 'number))
        (deterministic? (bundle-deterministic? state)))
    (if deterministic?
        (if (= tests-planned tests-that-ran)
            (format #t "Ran ~a test~p, as planned.~%"
                    tests-planned tests-planned)
            (format #t "~a test~p were planned, but ran ~a, which is ~a!~%"
                    tests-planned tests-planned tests-that-ran
                    (if (< tests-that-ran tests-planned)
                        'less 'more)))
        (format #t "Ran ~a test~p.~%" tests-that-ran tests-that-ran))))

(define (bundle-analyse-results state)
  (let* ((results (bundle-results state))
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

(define (bundle-analyse-state state)
  (bundle-analyse-version state)
  (bundle-analyse-plan state)
  (bundle-analyse-results state)
  state)

(define (starts-with obj x)
  (and (list? obj)
       (not (null? obj))
       (eq? (car obj) x)))

(define (bundle:pass? obj) (starts-with obj 'pass))
(define (bundle:fail? obj) (starts-with obj 'fail))
(define (bundle:oddity? obj) (not (= 1 (length obj))))

(define* (harness-analyse states #:key (pre-summary (lambda (s) #t)))
  (define (count* slot f)
    (fold (lambda (s cnt)
            (if (f (slot s))
                (1+ cnt)
                cnt))
          0 states))
  (define (count f) (count* bundle-outcome f))
  (pre-summary states)
  (let* ((n (length states))
         (n-skip (count* bundle-plan skip?))
         (n-required (- n n-skip))
         (n-pass (count bundle:pass?))
         (n-fail (count bundle:fail?))
         (n-oddity (count bundle:oddity?))
         (tell (lambda (m text)
                 (format #t "~a of ~a bundle~p ~a!~%"
                         m n-required n-required text))))
    (format #t "Processed ~a test bundle~p" n n)
    (if (zero? n-skip)
        (begin (display ".")
               (newline))
        (format #t "; ~a of which were skipped.~%" n-skip))
    (if (= (+ n n-skip) n-pass)
        (format #t "All test bundles passed!~%")
        (tell n-pass 'passed))
    (when (not (zero? n-fail))
      (tell n-fail 'failed))
    (when (not (zero? n-oddity))
      (tell n-oddity "encountered an oddity")))
  states)

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
  (let ((n (assq-ref (bundle-auxiliary s) 'last-progress-length)))
    (when n
      (display (make-string n #\space))
      (return))))

(define (progress-string state string)
  (display string)
  (return)
  (change-bundle-auxiliary state (assq-change (bundle-auxiliary state)
                                              'last-progress-length
                                              (string-length string))))

(define (progress-plan s i p)
  (clear-previous s)
  (let* ((n (assq-ref (bundle-plan s) 'number))
         (str (if (bundle-deterministic? s)
                  (format #f "Initialising deterministic plan: ~a tests." n)
                  (format #f "Non deterministic plan signals ~a tests." n))))
    (progress-string s str)))

(define (progress-test s i p)
  (clear-previous s)
  (let* ((n (assq-ref (bundle-plan s) 'number))
         (m (1- (bundle-number s)))
         (str (format #f "~a ~v,,'.t ~a/~a"
                      (bundle-name s) *progress-column* m n)))
    (progress-string s str)))

(define (progress-completion s i p)
  (define (is-pass?) (bundle:pass? (bundle-outcome s)))
  (define (is-skip?) (skip? (bundle-plan s)))
  (clear-previous s)
  (format #t "~a ~v,,'.t ~a~%"
          (bundle-name s)
          *progress-column*
          (cond ((is-pass?) 'ok)
                ((is-skip?) 'skip)
                (else 'fail)))
  s)
