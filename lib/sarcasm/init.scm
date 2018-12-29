#|----------------------------------------------------------------------------|#
#| sarcasm core: additionnal library procedures                               |#
#|----------------------------------------------------------------------------|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYNTAX
(define-syntax begin
  (syntax-rules ()
    ((begin) (void))
    ((begin expr ...) ((lambda () expr ...)))))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...) (if test1 (and test2 ...) #f))))

(define-syntax when
  (syntax-rules ()
    ((_ pred b1 ...)
     (if pred (begin b1 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((_ pred b1 ...)
     (if (not pred) (begin b1 ...)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIST OPERATIONS
(define (list-ref lst k)
  (if (zero? k)
      (car lst)
      (list-ref (cdr lst) (- k 1))))

(define (list-tail lst k)
  (if (zero? k)
      lst
      (list-tail (cdr lst) (- k 1))))

(define (foldl step initial lst)
  (if (null? lst)
      initial
      (foldl step (step (car lst) initial) (cdr lst))))

(define (reverse lst)
  (foldl cons '() lst))

(define (last lst)
  (if (null? lst)
      '()
      (if (null? (cdr lst))
          (car lst)
          (last (cdr lst)))))

(define (last-pair lst)
  (if (null? lst)
      '()
      (if (null? (cdr lst))
          lst
          (last-pair (cdr lst)))))

(define member
  (lambda (obj lst)
    (if (null? lst)
        #f
        (if (equal? obj (car lst))
            lst
            (member obj (cdr lst))))))

(define memq
  (lambda (obj lst)
    (if (null? lst)
        #f
        (if (eq? obj (car lst))
            lst
            (memq obj (cdr lst))))))

(define memv
  (lambda (obj lst)
    (if (null? lst)
        #f
        (if (eqv? obj (car lst))
            lst
            (memq obj (cdr lst))))))

