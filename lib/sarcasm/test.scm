;; (sarcasm test) library
;; a simple unit testing framework
(define-library (sarcasm test)
  (import (scheme base)
          (scheme write)
          (scheme time))
  (export
    test-begin test-end test-exit is is-q is-v is-true is-false
    %test-start %test-count %test-fail %do-test)
  (begin
    (define %test-start 0)
    (define %test-count 0)
    (define %test-fail  0)

    (define (test-begin)
      (display (make-string 80 #\-))
      (newline)
      (set! %test-start (current-second))
      (set! %test-count 0)
      (set! %test-fail  0))

    (define (test-end)
      (display (make-string 80 #\-))
      (newline)
      (display "Tests completed in ")
      (display (exact (truncate (* 1000000 (- (current-second) %test-start)))))
      (display "ms")
      (newline)
      (display "|   total: ") (display %test-count) (display " ") (newline)
      (display "| success: ") (display (- %test-count %test-fail)) (display " ") (newline)
      (display "| failure: ") (display %test-fail) (display " ") (newline)
      (newline))

    (define (test-exit)
      (exit %test-fail))

    (define (%do-test pred? expr expected got)
      (set! %test-count (+ 1 %test-count))
      (if (pred? expected got)
          (begin
            (display "[PASS] ")
            (write expr)
            (newline))
          (begin
            (set! %test-fail (+ 1 %test-fail))
            (display "[FAIL] ")
            (write expr)
            (display " => expected:")
            (write expected)
            (display " - actual:")
            (write got)
            (newline))))

    (define-syntax is
      (syntax-rules ()
        ((_ expected expr)
         (%do-test equal? 'expr expected expr))))

    (define-syntax is-v
      (syntax-rules ()
        ((_ expected expr)
         (%do-test eqv? 'expr expected expr))))

    (define-syntax is-q
      (syntax-rules ()
        ((_ expected expr)
         (%do-test eq? 'expr expected expr))))
    
    (define-syntax is-true
      (syntax-rules ()
        ((_ expr)
         (%do-test eq? 'expr #t expr))))

    (define-syntax is-false
      (syntax-rules ()
        ((_ expr)
         (%do-test eq? 'expr #f expr))))
    ))

