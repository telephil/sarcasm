;; (sarcasm test) library
;; a simple unit testing framework
(define-library (sarcasm test)
  (import (scheme base)
          (scheme write)
          (scheme time))
  (export
    test-begin test-end test-exit
    is is-q is-v is-true is-false is-error
    skip)
  (begin
    (define %test-start 0)
    (define %test-count 0)
    (define %test-fail  0)
    (define %test-skip  0)

    (define (test-begin)
      (display (make-string 80 #\-))
      (newline)
      (set! %test-start (current-jiffy))
      (set! %test-count 0)
      (set! %test-fail  0)
      (set! %test-skip  0))

    (define (test-end)
      (display (make-string 80 #\-))
      (newline)
      (display "Tests completed in ")
      (display (truncate (* 1000 (/ (- (current-jiffy) %test-start) (jiffies-per-second)))))
      (display "ms")
      (newline)
      (display "|   total: ") (display %test-count) (newline)
      (display "| success: ") (display (- %test-count %test-fail)) (newline)
      (display "| failure: ") (display %test-fail) (newline)
      (display "| skipped: ") (display %test-skip) (newline)
      (newline))

    (define (test-exit)
      (exit %test-fail))

    (define (%test pred? expr expected got)
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

    (define (%skip expr)
      (set! %test-count (+ 1 %test-count))
      (set! %test-skip  (+ 1 %test-skip))
      (display "[SKIP] ")
      (write expr)
      (newline))

    (define-syntax skip
      (syntax-rules ()
        ((_ expr)
         (%skip 'expr))))

    (define-syntax is
      (syntax-rules ()
        ((_ expected expr)
         (%test equal? 'expr expected expr))))

    (define-syntax is-v
      (syntax-rules ()
        ((_ expected expr)
         (%test eqv? 'expr expected expr))))

    (define-syntax is-q
      (syntax-rules ()
        ((_ expected expr)
         (%test eq? 'expr expected expr))))
    
    (define-syntax is-true
      (syntax-rules ()
        ((_ expr)
         (%test eq? 'expr #t expr))))

    (define-syntax is-false
      (syntax-rules ()
        ((_ expr)
         (%test eq? 'expr #f expr))))

    (define-syntax is-error
      (syntax-rules ()
        ((_ expr)
         (let ((result (call-with-current-continuation
                         (lambda (k)
                           (with-exception-handler
                             (lambda (x) (k 'error))
                             (lambda () expr 'success))))))
           (%test eq? 'expr 'error result)))))
    ))

