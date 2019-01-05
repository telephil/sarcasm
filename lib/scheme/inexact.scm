;; (scheme inexact) library
(define-library (scheme inexact)
   (import (sarcasm core))
   (export
     finite?
     infinite? 
     nan? 
     exp 
     log
     cos
     sin
     tan
     acos
     asin
     atan
     sqrt))

