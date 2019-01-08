#|----------------------------------------------------------------------------|#
#| sarcasm core: additionnal library procedures                               |#
#|----------------------------------------------------------------------------|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYNTAX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
this break begin form in libraries
(define-syntax begin
  (syntax-rules ()
    ((begin) (void))
    ((begin expr ...) ((lambda () expr ...)))))
|#
(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...) (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #t)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (caar lst) (car (car lst)))
(define (cadr lst) (car (cdr lst)))
(define (cdar lst) (cdr (car lst)))
(define (cddr lst) (cdr (cdr lst)))

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
  (case-lambda
    ((obj lst pred?)
     (if (null? lst)
         #f
         (if (pred? obj (car lst))
             lst
             (member obj (cdr lst) pred?))))
    ((obj lst)
     (member obj lst equal?))))

(define memq
  (lambda (obj lst)
    (member obj lst eq?)))

(define memv
  (lambda (obj lst)
    (member obj lst eqv?)))

(define assoc
  (case-lambda
    ((obj alist) (assoc obj alist equal?))
    ((obj alist pred?)
     (if (null? alist)
         #f
         (if (pred? obj (caar alist))
             (car alist)
             (assoc obj (cdr alist) pred?))))))

(define assq
  (lambda (obj alist)
    (assoc obj alist eq?)))

(define assv
  (lambda (obj alist)
    (assoc obj alist eqv?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PORT OPERATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (call-with-port port proc)
  (define result (proc port))
  (close-port port)
  result)

(define (call-with-input-file filename proc)
  (define port (open-input-file filename))
  (call-with-port port proc))

(define (call-with-output-file filename proc)
  (define port (open-output-file filename))
  (call-with-port port proc))

(define (with-input-from-file filename thunk)
  (define orig-port (current-input-port))
  (define port (open-input-file filename))
  (%set-current-input-port port)
  (let ((result (thunk)))
    (%set-current-input-port orig-port)
    (close-port port)
    result))

(define (with-output-to-file filename thunk)
  (define orig-port (current-output-port))
  (define port (open-output-file filename))
  (%set-current-output-port port)
  (let ((result (thunk)))
    (%set-current-output-port orig-port)
    (close-port port)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONTROL FEATURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (collect-by f lst)
  (let loop ((args lst))
    (if (or (null? args) (null? (car args)))
        '()
        (cons (f args) (loop (cdr args))))))

(define (%list-mapper-arg-lists lst) 
  (let loop ((args lst))
    (if (or (null? args) (null? (car args)))
        '()
        (cons (collect-by caar args) (loop (collect-by cdar args))))))

(define (%list-mapper-check-lengths proc frst lst)
  (unless (null? lst)
    (unless (= (length (car lst)) (length frst))
      (error
        (let ((p (open-output-string)))
          (display proc p)
          (display ": lists " p)
          (display frst p)
          (display " and " p)
          (display (car lst) p)
          (display " differ in size" p)
          (get-output-string p))))
    (%list-mapper-check-lengths proc frst (cdr lst))))

(define (%list-mapper name accumulator f lists)
  (%list-mapper-check-lengths name (car lists) (cdr lists))
  (let loop ((args (%list-mapper-arg-lists lists)))
    (if (null? args) '()
        (accumulator (apply f (car args))
                     (loop (cdr args))))))

(define map
  (lambda (f . lists)
    (%list-mapper "map" cons f lists)))

(define for-each 
  (lambda (f . lists)
    (%list-mapper "for-each" void f lists)))

