(define-library (sarcasm string)
  (import (sarcasm core))
  (export string-empty? starts-with? format)
  (begin

    ;; check whether string <str> is empty
    (define (string-empty? str)
      (string=? str ""))

    ;; check whether string <s> starts with <t>
    (define (starts-with? s t)
      (define lt (string-length t))
      (define ls (string-length s))
      (or (zero? lt)
          (and (<= lt ls)
               (string=? (substring s 0 (- lt 1)) t))))
    ))
