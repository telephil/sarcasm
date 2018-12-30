;; (scheme process-context) library
(define-library (scheme process-context)
   (import (sarcasm core))
   (export
     command-line emergency-exit
     exit
     get-environment-variable
     get-environment-variables))
