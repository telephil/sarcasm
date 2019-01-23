#|----------------------------------------------------------------------------|#
#| sarcasm core: additionnal library procedures                               |#
#|----------------------------------------------------------------------------|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYNTAX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...)
                      body1 body2 ...)))
        tag)
      val ...))))
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

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

(define-syntax case
  (syntax-rules (else =>)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else => result))
     (result key))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case key
       ((atoms ...) => result))
     (if (memv key '(atoms ...))
         (result key)))
    ((case key
       ((atoms ...) => result)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (result key)
         (case key clause clauses ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))))

; !!! bug in syntax expander
(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
       (test expr ...)
       command ...)
     (letrec
       ((loop
          (lambda (var ...)
            (if test
                (begin
                  (if #f #f)
                  expr ...)
                (begin
                  command
                  ...
                  (loop (do "step" var step ...)
                        ...))))))
       (loop init ...)))
    ((do "step" x)
     x)
    ((do "step" x y)
     y)))

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
(define (%map-aux name accumulator f elts)
  (define length-error (lambda (x y)
                         (error
                          (let ((p (open-output-string)))
                            (display name p)
                            (display ": lists " p)
                            (display y p)
                            (display " and " p)
                            (display y p)
                            (display " differ in size" p)
                            (get-output-string p)))))
  (define check-lengths (lambda ()
                          (let loop ((first (car elts)) (args (cdr elts)))
                            (unless (null? args)
                              (unless (= (length (car args)) (length first))
                                (length-error first (car args))
                                (loop first (cdr args)))))))
  (define collect-args-by (lambda (f lst)
                            (let loop ((args lst))
                              (if (or (null? args) (null? (car args)))
                                  '()
                                  (cons (f (car args)) (loop (cdr args)))))))
  (check-lengths)
  (let loop ((args elts))
    (if (or (null? args) (null? (car args)))
        '()
        (accumulator (apply f (collect-args-by car args))
                     (loop (collect-args-by cdr args))))))

(define map
  (lambda (f . lists)
    (%map-aux "map" cons f lists)))

(define for-each 
  (lambda (f . lists)
    (%map-aux "for-each" void f lists)))

(define vector-map
  (lambda (f . lists)
    (list->vector
     (apply map (cons f (map vector->list lists))))))

(define vector-for-each
  (lambda (f . lists)
    (apply for-each (cons f (map vector->list lists)))))

(define string-map
  (lambda (f . lists)
    (list->string
     (apply map (cons f (map string->list lists))))))

(define string-for-each
  (lambda (f . lists)
    (apply for-each (cons f (map string->list lists)))))

(define values
  (lambda things
    (call-with-current-continuation
      (lambda (cont)
        (apply cont things)))))

;; values are just a list flag as values
;; this might not be the proper way to proceed though
(define (call-with-values producer consumer)
  (let ((v (producer)))
    (if (list? v)
        (apply consumer v)
        (consumer v)))) 

