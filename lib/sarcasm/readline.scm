(define-library (sarcasm readline)
  (import-module "libsarcasm_readline")
  (export
    readline
    add-history
    read-history
    write-history
    set-completion-function!)
  ;; library is implemented on C side
  )
