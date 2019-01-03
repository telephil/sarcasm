(import (scheme base)
        (scheme char)
        (scheme cxr)
        (scheme inexact)
        (scheme process-context)
        (scheme time)
        (scheme write)
        (sarcasm test))

(display "R7RS test suite\n")

(test-begin)

;; Numbers
(is-true  (number? 1))
(is-true  (number? 1.5))
(is-false (number? ""))
(is-true (real? 1.5))
(is-true (real? 2))
(is-true (integer? 10))
(is-false (integer? #\a))
(is-false (exact? 3.0))
(is-true (exact? 3))
(is-true (exact-integer? 32))
(is-false (exact-integer? 32.0))
(is-true (finite? 3))
(is-false (finite? +inf.0))
(is-false (infinite? 3))
(is-true (infinite? -inf.0))
(is-false (infinite? +nan.0))
(is-false (nan? 32))
(is-true (nan? +nan.0))
(is-true (= 1 1 1 1 1 1.0))
(is-true (< 1 2 3 4))
(is-false (< 1 1 2 3))
(is-true (<= 1 2 3 4))
(is-true (<= 1 1 2 3))
(is-true (> 3 2 1))
(is-false (> 1 2 3))
(is-true (>= 3 2 1))
(is-true (zero? 0))
(is-true (zero? 0.0))
(is-false (zero? 42))
(is-true (positive? 0.0))
(is-true (positive? 5))
(is-true (positive? +inf.0))
(is-false (positive? -1))
(is-true (negative? -1))
(is-false (negative? 0))
(is-true (negative? -inf.0))
(is-true (odd? 3))
(is-true (odd? 3.14))
(is-false (odd? 4))
(is-true (even? 12))
(is-false (even? 42.0))
(is 4 (max 3 4))
(is 4 (max 3.9 4))
(is -inf.0 (min 10 -5 4 -inf.0 12))
(is 0 (+))
(is 1 (+ 1))
(is 5 (+ 2 3))
(is 3 (+ 5 -2))
(is 3.0 (+ 0.5 2.5))
(is 1 (*))
(is 5 (* 5))
(is 2 (* 4 0.5))
(is 0 (- 0))
(is -5 (- 5))
(is 2 (- 5 3))
(is 2.0 (- 2.5 0.5))
(is 2 (truncate 2))
(is -4.0 (truncate -4.5))
(is 3.0 (truncate 3.5))
(is 2 (exact 2))
(is -3 (exact -3.14))
;; Booleans
(is #t #t)
(is #f #f)
(is #f '#f)
(is-false (not #t))
(is-false (not 3))
(is-false (not (list 3)))
(is-true (not #f))
(is-false (not '()))
(is-false (not (list)))
(is-false (not 'nil))
(is-true (boolean? #t))
(is-false (boolean? 0))
(is-false (boolean? '()))
(is-true (boolean=? #t #t #t))
;; Pairs and lists
(is-true (pair? '(a . b)))
(is-true (pair? '(a b c)))
(is-false (pair? '()))
(is-false (pair? '#(a b)))
(is '(a) (cons 'a '()))
(is '((a) b c d) (cons '(a) '(b c d)))
(is '("a" b c) (cons "a" '(b c)))
(is '(a . 3) (cons 'a 3))
(is '((a b) . c) (cons '(a b) 'c))
(is 'a (car '(a b c)))
(is '(a) (car '((a) b c)))
(is 1 (car '(1 . 2)))
(skip (is-error (car '())))
(is '(b c d) (cdr '((a) b c d)))
(is 2 (cdr '(1 . 2)))
(skip (is-error (cdr '())))
(define l (list 1 2 3))
(set-car! l 0)
(is '(0 2 3) l)
(set-cdr! l '(1 2))
(is '(0 1 2) l)
(is-true (null? '()))
(is-false (null? '(1 2 3)))
(is-true (list? '(a b c))) 
(is-true (list? '()))
(is-false (list? '(1 . 2)))
(is '(3 3) (make-list 2 3))
(is '(a 7 c) (list 'a (+ 3 4) 'c))
(is '() (list))
(is 3 (length '(a b c)))
(is 3 (length '(a (b) (c d e))))
(is 0 (length '()))
(is '() (append))
(is '(x y) (append '(x) '(y)))
(is '(a b c d) (append '(a) '(b c d)))
(is '(a (b) (c)) (append '(a (b)) '((c))))
(is '(a b c .d) (append '(a b) '(c . d)))
(is 'a (append '() 'a))
(is '(c b a) (reverse '(a b c)))
(is '((e (f)) d (b c) a) (reverse '(a (b c) d (e (f)))))
(is '(3 4) (list-tail '(1 2 3 4) 2))
(is 'c (list-ref '(a b c d) 2))
(is '(a b c) (memq 'a '(a b c)))
(is '(b c) (memq 'b '(a b c)))
(is-false (memq 'd '(a b c)))
(is-false (memq (list 'a) '(b (a) c)))
(is '((a) c) (member (list 'a) '(b (a) c)))
(is '("b" "c") (member "B" '("a" "b" "c") string-ci=?))
(is '(101 102) (memv 101 '(100 101 102)))
(define e '((a 1) (b 2) (c 3)))
(is '(a 1) (assq 'a e))
(is '(b 2) (assq 'b e))
(is-false (assq 'd e))
(is-false (assq (list 'a) '(((a)) ((b)) ((c)))))
(is '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))
(is '(2 4) (assoc 2.0 '((1 1) (2 4) (3 9))))
(is '(5 7) (assoc 5 '((2 3) (5 7) (11 13))))
;; Symbols
(is-true (symbol? 'foo))
(is-true (symbol? (car '(a b))))
(is-false (symbol? "bar"))
(is-true (symbol? 'nil))
(is-false (symbol? '()))
(is-false (symbol? #f))

(test-end) 
(test-exit)


