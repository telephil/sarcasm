(import (scheme base)
        (scheme write)
        (sarcasm test))

(test-begin)

(is 8 ((lambda (x) (+ x x)) 4))
(is 5 (+ 2 3))

(test-end) 
(test-exit)

