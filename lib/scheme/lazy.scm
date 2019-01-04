;; (scheme lazy) library
(define-library (scheme lazy)
  (import (scheme base))
  (export delay delay-force force make-promise promise?)
  (begin

  (define-record-type <promise>
    (promise done? value)
    promise?
    (done? promise-done? set-promise-done!)
    (value promise-value set-promise-value!))

  (define-syntax delay-force
    (syntax-rules ()
      ((_ expr) (make-promise #f (lambda () expr)))))

  (define-syntax delay
    (syntax-rules ()
      ((_ expr) (delay-force (make-promise #t expr)))))

  (define make-promise
    (lambda (done? proc)
      (promise done? proc)))

  (define (force p)
    (unless (promise? p) (error "force: expected a promise" (list p)))
    (if (promise-done? p)
        (promise-value p)
        (let ((promise* ((promise-value p))))
          (unless (promise-done? p)
            (promise-update! promise* p))
          (force p))))

  (define promise-update!
    (lambda (new old)
      (set-promise-done! old (promise-done? new))
      (set-promise-value! old (promise-value new))))))
