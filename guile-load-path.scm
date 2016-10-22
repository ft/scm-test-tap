;; -*- scheme -*-

(define (find-top-dir)
  (let loop ((start (getcwd)))
    (let ((file (string-concatenate `(,start "/guile-load-path.scm"))))
      (cond ((string=? start "/") (throw 'cannot-file-topdir))
            ((file-exists? file) start)
            (else (loop (dirname start)))))))

(let* ((topdir (find-top-dir))
       (scheme (string-concatenate `(,topdir "/scheme"))))
  (set! %load-should-auto-compile #f)
  (set! %load-path (cons scheme %load-path))
  (set! %load-compiled-path (cons scheme %load-compiled-path)))
