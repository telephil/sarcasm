(define (caar lst) (car (car lst)))
(define (cadr lst) (car (cdr lst)))
(define (cdar lst) (cdr (car lst)))
(define (cddr lst) (cdr (cdr lst)))
(define (caaar lst) (car (car (car lst))))
(define (caadr lst) (car (car (cdr lst))))
(define (cadar lst) (car (cdr (car lst))))
(define (cdaar lst) (cdr (car (car lst))))
(define (caddr lst) (car (cdr (cdr lst))))
(define (cdadr lst) (cdr (car (cdr lst))))
(define (cddar lst) (cdr (cdr (car lst))))
(define (cdddr lst) (cdr (cdr (cdr lst))))
(define (cddddr lst) (cdr (cdr (cdr (cdr lst)))))
(define (cadddr lst) (car (cdr (cdr (cdr lst)))))
(define (cdaddr lst) (cdr (car (cdr (cdr lst)))))
(define (cddadr lst) (cdr (cdr (car (cdr lst)))))
(define (cdddar lst) (cdr (cdr (cdr (car lst)))))
(define (caaddr lst) (car (car (cdr (cdr lst)))))
(define (cadadr lst) (car (cdr (car (cdr lst)))))

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
