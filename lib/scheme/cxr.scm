;; (scheme cxr) library
(define-library (scheme cxr)
  (import (sarcasm core))
  (export 
    caaar
    caadr cadar cdaar
    caddr cdadr cddar
    cdddr
    caaaar
    caaadr caadar cadaar cdaaar 
    caaddr cadadr cdaadr cdadar cddaar caddar
    cadddr cdaddr cddadr cdddar
    cddddr)
  (begin
    (define (caaar lst) (car (car (car lst))))
    (define (caadr lst) (car (car (cdr lst))))
    (define (cadar lst) (car (cdr (car lst))))
    (define (cdaar lst) (cdr (car (car lst))))
    (define (caddr lst) (car (cdr (cdr lst))))
    (define (cdadr lst) (cdr (car (cdr lst))))
    (define (cddar lst) (cdr (cdr (car lst))))
    (define (cdddr lst) (cdr (cdr (cdr lst))))
    (define (caaaar lst) (car (car (car (car lst)))))
    (define (caaadr lst) (car (car (car (cdr lst)))))
    (define (caadar lst) (car (car (cdr (car lst)))))
    (define (cadaar lst) (car (cdr (car (car lst)))))
    (define (cdaaar lst) (cdr (car (car (car lst)))))
    (define (caaddr lst) (car (car (cdr (cdr lst)))))
    (define (cadadr lst) (car (cdr (car (cdr lst)))))
    (define (cdaadr lst) (cdr (car (car (cdr lst)))))
    (define (cdadar lst) (cdr (car (cdr (car lst)))))
    (define (cddaar lst) (cdr (cdr (car (car lst)))))
    (define (caddar lst) (car (cdr (cdr (car lst)))))
    (define (cadddr lst) (car (cdr (cdr (cdr lst)))))
    (define (cdaddr lst) (cdr (car (cdr (cdr lst)))))
    (define (cddadr lst) (cdr (cdr (car (cdr lst)))))
    (define (cdddar lst) (cdr (cdr (cdr (car lst)))))
    (define (cddddr lst) (cdr (cdr (cdr (cdr lst))))))

