;; (sarcasm test) library
;; a simple unit testing framework
(define-library (sarcasm test)
  (import (scheme base)
          (scheme write))
  (export test-begin test-end is TEST-COUNT TEST-FAIL)
  (begin
    (define TEST-COUNT 0)
    (define TEST-FAIL  0)

    (define (test-begin)
      (set! TEST-COUNT 0)
      (set! TEST-FAIL  0))

    (define (test-end)
      (display "Tests run:")
      (display TEST-COUNT)
      (display " - failed:")
      (display TEST-FAIL)
      (newline))

    (define-syntax is
      (syntax-rules ()
        ((_ expected expr)
         (begin
           (set! TEST-COUNT (+ 1 TEST-COUNT))
           (if (equal? expected expr)
             (begin
               (display "[PASS] ")
               (display 'expr)
               (newline))
             (begin
               (set! TEST-FAIL (+ 1 TEST-FAIL))
               (display "[FAIL] ")
               (display 'expr)
               (display " => expected:")
               (display expected)
               (display " - actual:")
               (display expr)
               (newline)))))))))

