(define-library (sarcasm repl)
  (import (sarcasm core))
  (export repl)
 
  (begin
    (define history-filename
      (string-append (get-environment-variable "HOME") "/.scm_history"))

    (define (load-history)
      (read-history history-filename))

    (define (save-history)
      (write-history history-filename))

    (define (starts-with? s t)
      (define lt (string-length t))
      (define ls (string-length s))
      (or (zero? lt)
          (and (<= lt ls)
               (string=? (substring s 0 (- lt 1)) t))))

    (define (completions-for text)
      (define loop
        (lambda (symbols)
          (if (null? symbols)
              '()
              (let ((sym (symbol->string (car symbols))))
                (if (starts-with? sym text)
                    (cons sym (loop (cdr symbols)))
                    (loop (cdr symbols)))))))
      (loop (environment-symbols (interaction-environment))))

    (define (banner)
      (display "sarcasm v0.1\n")
      (display "=============\n\n"))

    (define (end)
      (save-history)
      (display "Bye\n"))

    (define (error-handler err)
      (display "\x001b;[31mERROR\x001b;[0m ")
      (display (error-object-message err))
      (newline))

    (define (repl)
      (register-exit-hook end)
      (banner)
      (load-history)
      (set-completion-function! completions-for)
      (let loop ((line (readline "> ")))
        (when line
          (add-history line)
          (with-exception-handler 
            error-handler
            (lambda ()
              (let* ((r (read-from-string line))
                     (v (eval r (interaction-environment))))
                (write v)
                (newline))))
          (loop (readline "> ")))))
    ))
