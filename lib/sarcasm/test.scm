;; (sarcasm test) library
;; a simple unit testing framework
(define-library (sarcasm test)
  (import (scheme base)
          (scheme write)
          (scheme time))
  (export
    test-begin test-end test-exit is
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

    (define (%do-test expr expected got)
      (set! %test-count (+ 1 %test-count))
      (if (equal? expected got)
          (begin
            (display "[PASS] ")
            (display expr)
            (newline))
          (begin
            (set! %test-fail (+ 1 %test-fail))
            (display "[FAIL] ")
            (display expr)
            (display " => expected:")
            (display expected)
            (display " - actual:")
            (display got)
            (newline))))

    (define-syntax is
      (syntax-rules ()
        ((_ expected expr)
         (%do-test 'expr expected expr))))))

