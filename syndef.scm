#|
(define-syntax define-record
  (syntax-rules ()
    ((_ name)
     (define (name)
       (list 'name 0 0)))))

|#

(define-record-type <pare>
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))

