;; TAP emiting test suite for scheme (currently GNU Guile only).

;; Copyright 2012-2016 Frank Terbeck <ft@bewatermyfriend.org>
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

;; A TAP test suite is a set of files, that emit output according to the TAP
;; protocol. One or more test cases form a test-bundle. A file may hold one or
;; more test-bundles.
;;
;; This implementation uses the following API:
;;
;;   - with-test-bundle
;;         Surrounds a set of test-cases.
;;
;;   - define-test
;;         Surrounds the code of one test case, which should be implemented
;;         in terms of a `pass-if-*' call (see below).
;;
;;   - plan and no-plan
;;         Setup the test plan for a test-bundle.
;;
;;   - Various pass-if-* macros
;;         These are convenience macros, to easily write test cases.
;;
;; A valid test script may look like this:
;;
;; (use-modules (test tap))
;;
;; (with-test-bundle (foo bar)
;;   (plan 1)
;;   (define-test "basic math"
;;     (pass-if-= (+ 1 2)
;;                3)))
;;
;; TAP Version Support
;;
;;   This code currently produces output according to TAP protocol version 12,
;;   but emitting version 13 output should not be a problem at all. However,
;;   since the current Perl Test::More module doesn't even do that itself, this
;;   code won't bother.

(define-module (test tap)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:export (pass-if-exception
            pass-if-any-exception
            pass-if-no-exception
            pass-if-=             pass-if-not-=
            pass-if-~=            pass-if-not-~=
            pass-if-eq?           pass-if-not-eq?
            pass-if-eqv?          pass-if-not-eqv?
            pass-if-equal?        pass-if-not-equal?
            pass-if-string=?      pass-if-not-string=?
            pass-if-string-ci=?   pass-if-not-string-ci=?
            pass-if-re-match      pass-if-not-re-match
            pass-if-true          pass-if-false
            plan                  no-plan
            require
            todo
            define-test
            with-fs-test-bundle
            with-test-bundle
            force-import
            make-labeled-values
            tap/bail-out
            tap/comment
            tap/set-option))

;; Internal variables

(define *plan* #f)
(define *todo-prints-diag* #f)
(define *test-case-count* 0)
(define *test-case-todo* #f)
(define *test-description* #f)
(define *test-hierarchy* '())
(define *test-pp-width* 60)
(define *test-force-tap-header* #f)
(define *test-fs-suffix* ".t$")
(define *test-fs-prefix* "^[0-9]+-")
(define *test-fs-root* (getcwd))
(define *test-fs-file* (car (command-line)))

(define-syntax tap:option
  (lambda (x)
    (syntax-case x ()
      ((_ name variable predicate)
       #'(list (quote name)
               (lambda () variable)
               (lambda (xx)
                 (set! variable xx))
               predicate
               (quote predicate))))))

(define options (list (tap:option fs-root *test-fs-root* string?)
                      (tap:option fs-suffix *test-fs-suffix* string?)
                      (tap:option fs-prefix *test-fs-prefix* string?)
                      (tap:option fs-file *test-fs-file* string?)
                      (tap:option tap-todo-prints-diagnose
                                  *todo-prints-diag* boolean?)
                      (tap:option tap-force-header *test-force-tap-header*
                                  boolean?)
                      (tap:option tap-pretty-print-width *test-pp-width*
                                  integer?)))

(define (opt:get-entry key extr)
  (let ((result (filter-map (lambda (x)
                              (if (eq? (car x) key)
                                  x #f))
                            options)))
    (if (null? result)
        '()
        (extr (car result)))))

(define (opt:get-value key)
  (let ((f (opt:get-entry key cadr)))
    (if (and (list? f)
             (null? f))
        f
        (f))))

(define (opt:get-setter key)
  (opt:get-entry key caddr))

(define (opt:get-predicate key)
  (opt:get-entry key cadddr))

(define (opt:get-pred-name key)
  (car (opt:get-entry key cddddr)))

(define (tap/set-option key value)
  (let ((s (opt:get-setter key))
        (p (opt:get-predicate key)))
    (cond ((not (null? s))
           (if (p value)
               (begin
                 (s value)
                 value)
               (begin
                 (format #t "Invalid value for `~a' (~s). " key value)
                 (format #t "Needs to satisfy `~a'.~%" (opt:get-pred-name key))
                 '())))
          (else
           (format #t "Invalid option: `~a'~%" key)
           (format #t "  valid options:~%")
           (let next ((o options))
             (cond
              ((null? o) '())
              (else
               (format #t "    ~a~%" (caar o))
               (next (cdr o)))))))))

;; Plan handling

(define (plan value)
  (set! *plan* value))

(define (no-plan)
  (set! *plan* 'no-plan))

;; Test suite core

;; `update-test' updates all relevant book-keeping when a new test is run. It
;; also handles some output which is applicable only for the first test within
;; a bundle.
(define (update-test name)
  (if (= *test-case-count* 0)
      (begin
        (tap/header)
        (tap/comment (format #f "test bundle: ~a" *test-hierarchy*))
        (if (number? *plan*)
            (tap/plan *plan*))))
  (set! *test-case-count* (+ *test-case-count* 1))
  (set! *test-description* name))

(define (print-location loc)
  (tap/comment (format #f "    file: ~s" (assq-ref loc 'filename)))
  (tap/comment (format #f "    line: ~d" (assq-ref loc 'line)))
  (tap/comment (format #f "  column: ~d" (assq-ref loc 'column))))

(define (pp-expression expression)
  (pretty-print expression
                #:width *test-pp-width*
                #:display? #f
                #:per-line-prefix "#     "))

;; `error-diag' provides detailed diagnostic output for failed tests.
(define (error-diag test loc expression data)
  (format #t "#~%# failed test: ~s~%" *test-description*)
  (format #t "#~%# location:~%")
  (print-location loc)
  (format #t "#~%# expression:~%")
  (pp-expression expression)
  (format #t "#~%# evaluated form:~%")
  (let ((form (let loop ((rest data) (acc (list test)))
                (if (null? rest)
                    (reverse acc)
                    (loop (cdr rest) (cons (cdar rest) acc))))))
    (pp-expression form))
  (format #t "#~%"))

;; `deal-with-exception' diagnoses caught exceptions.
(define* (deal-with-exception loc exp name argument #:key (skip-expr? #f))

  (define format-error-msgs '(unbound-variable
                              wrong-number-of-args
                              wrong-type-arg))

  (define (else-handler args)
    (tap/comment "argument:")
    (tap/comment (format #f "    ~s" argument)))

  (when loc
    (tap/comment "")
    (print-location loc))
  (unless skip-expr?
    (tap/comment "")
    (tap/comment "expression:")
    (pp-expression exp))
  (tap/comment "")
  (tap/comment "exception:")
  (tap/comment (format #f "    ~s" name))
  (tap/comment "")
  (cond ((member name format-error-msgs)
         (match argument
           ((#f fmt (arg) #f)
            (tap/comment (format #f fmt arg)))
           ((proc fmt (args _ ...) ...)
            (tap/comment (format #f "In procedure ~a: " proc))
            (tap/comment (string-append "    " (apply format #f fmt args))))
           (else (else-handler argument))))
        ((null? argument) (tap/comment "Empty exception argument."))
        (else (else-handler argument))))

;; `require' is used to dynamically determine whether a dependency of a
;; test-case or even a test-bundle is satisfied and either skip the test or the
;; whole bundle. `require' must be called as soon as possible within
;; `with-test-bundle' or `define-test'.
(define (require test)
  (if (not test)
      (throw 'sts/test-requirement-failed)))

;; The `todo' form may be used to wrap any number of expressions in. Any test
;; executed within this form will be marked as "TODO". So, it may fail without
;; failing the whole test-suite.
(define-syntax todo
  (lambda (x)
    (syntax-case x ()
      ((_ exp0 exp1 ...)
       #'(let ()
           (set! *test-case-todo* #t)
           exp0
           exp1
           ...
           (set! *test-case-todo* #f))))))

;; The `define-test' is used to introduce each and every test case together
;; with a name for human beings to recognise it by. Test code (as in
;; `pass-if-*') may not appear outside of an `define-test' form.
(define-syntax define-test
  (lambda (x)
    (syntax-case x (skip)
      ((_ skip name code ...)
       #'(let ()
           (update-test name)
           (tap/skip *test-case-count* *test-description*)))
      ((_ name code ...)
       #'(let ()
           (update-test name)
           (catch 'sts/test-requirement-failed
             (lambda ()
               code ...)
             (lambda (k . a)
               (tap/skip *test-case-count* *test-description*))))))))

;; `with-test-bundle' initialises all the accounting data for a test bundle and
;; provides an environment to run tests in. In particular, it handles plan
;; output and catches the exception thrown by `tap/bail-out'.
;;
;; `define-test' calls should *ONLY* appear within `with-test-bundle' (or its
;; cousin `with-fs-test-bundle' which is really implemented by the former,
;; too).
(define-syntax with-test-bundle
  (lambda (x)
    (syntax-case x (skip)
      ((_ skip hierarchy code ...)
       #'(tap/bundle-skip (format #f "~a" (quote hierarchy))))
      ((_ hierarchy code ...)
       #'(let ()
           (set! *test-case-count* 0)
           (set! *test-hierarchy* (quote hierarchy))
           (set! *plan* #f)
           (catch 'sts/tap/bail-out
             (lambda ()
               (catch 'sts/test-requirement-failed
                 (lambda ()
                   code ...
                   (if (eq? *plan* 'no-plan)
                       (tap/plan *test-case-count*)))
                 (lambda (k . a)
                   (tap/bundle-skip (format #f "~a" (quote hierarchy))))))
             (lambda (k . a)
               #t)))))))

;; `with-fs-test-bundle' is an addon to `with-test-bundle'. It takes the
;; test-code's files-name, strips off a root directory, a prefix and a suffix,
;; splits the rest at forward slashes and uses the resulting list as the
;; test-bundle's hierarchy:
;;
;;    root: "/usr/src/thing/tests"
;;  suffix: "-scm.t$"
;;  prefix: "^[0-9]+"-
;;    file: "/usr/src/thing/tests/foo/bar/0001-baz-scm.t"
;;
;; Resulting hierarchy: (foo bar baz)
;;
;; `deduce-hierarchy' maps a file-name to a hierarchy as described above. All
;; this hierarchy is an absolutely cosmetic feature, for human beings to tell
;; what a set of test-cases is about. Even if you don't set this feature up
;; correctly and still use `with-fs-test-bundle', the tests will run *just*
;; *fine*.
(define* (deduce-hierarchy filename
                           #:key
                           (root *test-fs-root*)
                           (suffix *test-fs-suffix*)
                           (prefix *test-fs-prefix*))
  (let* ((root-dir (if (string-match "/$" root)
                       root
                       (string-concatenate (list root "/"))))
         (relfile (if (string-match
                       (string-concatenate (list "^" root-dir))
                       filename)
                      (substring filename (string-length root-dir))
                      filename))
         (h (string-split relfile #\/))
         (h-start (list-head h (- (length h) 1)))
         (end (list-ref h (- (length h) 1)))
         (s-stripped (if (string-match suffix end)
                         (regexp-substitute #f
                                            (string-match suffix end)
                                            'pre)
                         end))
         (last-part (if (string-match prefix s-stripped)
                        (regexp-substitute #f
                                           (string-match prefix s-stripped)
                                           'post)
                        s-stripped)))
    (map (lambda (x)
           (string->symbol x))
         (append h-start (list last-part)))))

;; Finally, the `with-fs-test-bundle' macro, which is implemented in terms of
;; `with-test-bundle' through the help of `deduce-hierarchy'.
(define-syntax with-fs-test-bundle
  (lambda (x)
    (with-syntax (((hierarchy ...)
                   (map (lambda (x)
                          (datum->syntax #'x x))
                        (deduce-hierarchy *test-fs-file*))))
      (syntax-case x (skip)
        ((_ skip code ...)
         #'(with-test-bundle skip (hierarchy ...) code ...))
        ((_ code ...)
         #'(with-test-bundle (hierarchy ...) code ...))))))

(define (caught-title expected?)
  (let ((common "aught exception while evaluating test expression!"))
    (if expected?
        (tap/comment (string-append "C" common))
        (tap/comment (string-append "Unexpectedly c" common)))))

(define-syntax with-exception-handling
  (lambda (x)
    (syntax-case x ()
      ((_ exp)
       #'(catch #t
           (lambda () (cons #f exp))
           (lambda (a . k)
             (cons #t (list (current-source-location)
                            (quote exp)
                            a k))))))))

(define-syntax define-tap-test
  (lambda (stx-a)
    (syntax-case stx-a ()
      ((_ (name-a input-a ...) exp)
       #'(define-tap-test #f (name-a input-a ...) exp))
      ((_ allow-exception? (name-a input-a ...) exp)
       #'(begin
           (define-syntax name-a
             (lambda (stx)
               (with-ellipsis :::
                 (define (tree-map f p? t)
                   (define (atom? x)
                     (not (pair? x)))
                   (syntax-case t ()
                     (() t)
                     (e (p? #'e) (f t))
                     (e (atom? #'e) #'e)
                     (else (cons (tree-map f p? (car t))
                                 (tree-map f p? (cdr t))))))
                 (define (xchange-expressions in val expression)
                   (let ((ex (let loop ((i in) (v val) (acc '()))
                               (if (null? i)
                                   (reverse acc)
                                   (loop (cdr i) (cdr v)
                                         (cons (cons (car i) (car v)) acc))))))
                     (tree-map (lambda (x) (assoc-ref ex x))
                               (lambda (x) (member x in))
                               expression)))
                 (syntax-case stx ()
                   ((name input :::)
                    (with-syntax (((result :::)
                                   (generate-temporaries #'(input :::)))
                                  ((value :::)
                                   (generate-temporaries #'(input :::))))
                      (with-syntax ((exp* (xchange-expressions #'(input-a ...)
                                                               (if allow-exception?
                                                                   #'(result :::)
                                                                   #'(value :::))
                                                               #'exp)))
                        #'(let* ((result (with-exception-handling input)) :::
                                 (value (cdr result)) :::
                                 (final* (with-exception-handling exp*))
                                 (exception-in-arguments?
                                  (not (not (member #t (map car (list result :::))))))
                                 (late-exception? (car final*))
                                 (final (cdr final*))
                                 (failed? (or (and (not allow-exception?)
                                                   exception-in-arguments?)
                                              late-exception?
                                              (not final))))
                            (tap/result *test-case-count*
                                        *test-description*
                                        *test-case-todo*
                                        (not failed?))
                            (when (and (or *todo-prints-diag*
                                           (not *test-case-todo*))
                                       failed?)
                              (error-diag 'name-a
                                          (current-source-location)
                                          'exp
                                          (list result :::)))
                            final)))))))))))))

;; pass-if-*

(define yes (lambda (x) #t))

(define-syntax handle-exception
  (lambda (x)
    (syntax-case x ()
      ((_ exp sel mod)
       #'(if (car exp)
             (match (cdr exp)
               ((loc expression name argument)
                (mod (sel name))))
             (mod #f))))))

(define-syntax any-exception-fails
  (lambda (x)
    (syntax-case x ()
      ((_ e) #'(handle-exception e yes not)))))

(define-syntax require-any-exception
  (lambda (x)
    (syntax-case x ()
      ((_ e) #'(handle-exception e yes identity)))))

(define-syntax require-specific-exception
  (lambda (x)
    (syntax-case x ()
      ((_ exception expression)
       #'(handle-exception expression
                           (lambda (x) (eq? (cdr exception) x))
                           identity)))))

(define-tap-test (pass-if-= a b) (= a b))
(define-tap-test (pass-if-not-= a b) (not (= a b)))

(define-tap-test (pass-if-~= a b eps) (and (< a (+ b eps))
                                           (> a (- b eps))))
(define-tap-test (pass-if-not-~= a b eps) (not (and (< a (+ b eps))
                                                    (> a (- b eps)))))

(define-tap-test (pass-if-eq? a b) (eq? a b))
(define-tap-test (pass-if-not-eq? a b) (not (eq? a b)))

(define-tap-test (pass-if-eqv? a b) (eqv? a b))
(define-tap-test (pass-if-not-eqv? a b) (not (eqv? a b)))

(define-tap-test (pass-if-equal? a b) (equal? a b))
(define-tap-test (pass-if-not-equal? a b) (not (equal? a b)))

(define-tap-test (pass-if-string=? a b) (string=? a b))
(define-tap-test (pass-if-not-string=? a b) (not (string=? a b)))

(define-tap-test (pass-if-string-ci=? a b) (string-ci=? a b))
(define-tap-test (pass-if-not-string-ci=? a b) (not (string-ci=? a b)))

(define-tap-test (pass-if-re-match p s) (string-match p s))
(define-tap-test (pass-if-not-re-match p s) (not (string-match p s)))

(define-tap-test #t (pass-if-no-exception expression)
  (any-exception-fails expression))
(define-tap-test #t (pass-if-any-exception expression)
  (require-any-exception expression))
(define-tap-test #t (pass-if-exception exception expression)
  (require-specific-exception exception expression))

(define-tap-test (pass-if-true x) x)
(define-tap-test (pass-if-false x) (not x))

;; Utility functions and macros

(define-syntax make-labeled-values
  (lambda (x)
    (syntax-case x ()
      ((_ symbol ...)
       #'(list (cons (quote symbol) symbol) ...)))))

(define-syntax force-import
  (lambda (x)
    (syntax-case x ()
      ((_ module symbol)
       #'(define symbol (@@ module symbol)))
      ((_ module symbol0 symbol1 ...)
       #'(begin (force-import module symbol0)
                (force-import module symbol1 ...))))))

;; --- TAP writers ---
;;
;; See <http://testanything.org> for details. This code tries to implement the
;; protocol in the version stored in the `*sts/tap/version*' variable.

(define *sts/tap/version* 12) ; v13 is with YAML support

;; Every test bundle with TAP version >= 13, prints a header announcing the
;; version of the implemented TAP protocol.
(define (tap/header)
  (if (or *test-force-tap-header*
          (>= *sts/tap/version* 13))
      (format #t "TAP version ~d~%" *sts/tap/version*)))

;; Every test bundle should announce how many tests it is going to run (if it
;; can't be deduced how many test there are going to be you can also print the
;; plan at the end of the bundle's output).
(define (tap/plan plan)
  (format #t "1..~d~%" plan))

;; If a bundle is to be skipped entirely (maybe a prerequisite is not
;; fullfilled), that has to be announced with a plan announcing a plan from 1
;; to 0 (yes, zero) with a SKIP and description.
(define (tap/bundle-skip description)
  (format #t "1..0 # SKIP ~a~%" description))

;; The normal outcome of a test is either "ok" or "not ok".
(define (tap/result num description todo result)
  (format #t "~a ~d - ~a~a~%" (if result "ok" "not ok")
                              num
                              description
                              (if todo " # TODO" "")))

;; Tests can also be skipped.
(define (tap/skip num description)
  (format #t "ok ~d - ~a # SKIP~%" num description))

;; You can also spit out arbitrary other data, which the harness will ignore.
(define (tap/comment data)
  (cond ((list? data)
         (for-each (lambda (string)
                     (format #t "# ~a~%" string))
                   data))
        (else (format #t "# ~a~%" data))))

;; When something went really wrong, you can bail out of a test bundle
;; entirely.
(define (tap/bail-out description)
  (let ((desc (if (string= description "")
                  ""
                  (string-concatenate (list " " description)))))
    (format #t "Bail out!~a~%" desc))
  (throw 'sts/tap/bail-out))

;; And finally, diagnostic messages may be formatted in YAML instead of simple
;; comments, as required by TAP version 13. TODO: This should probably be split
;; into multiple procedures.
(define* (tap/yaml #:key (message #f)
                         (severity #f)
                         (got #f)
                         (expected #f))
  (format #t " ~a~% ~a: ~a~% ~a~%" "---" "status" "not implemented yet" "..."))
