(define-library (sarcasm readline)
  (import (sarcasm core))
  (export
    read-history
    write-history
    set-completion-function!)
  ;; library is implemented on C side
  )
