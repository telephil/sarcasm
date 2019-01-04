;; (scheme file) library
(define-library (scheme file)
   (import (sarcasm core))
   (export
     call-with-input-file call-with-output-file
     delete-file file-exists?
     open-binary-input-file open-binary-output-file
     open-input-file open-output-file
     with-input-from-file with-output-to-file
     ;; FIXME issue with internal library symbols
     %set-current-input-port %set-current-output-port
     ))
