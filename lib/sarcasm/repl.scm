(define-library (sarcasm repl)
  (import (scheme base)
          (sarcasm info)
          (sarcasm process)
          (sarcasm readline)
          (sarcasm string))
  (export repl)
 
  (begin
    (define history-filename
      (string-append (get-environment-variable "HOME") "/.scm_history"))

    (define (load-history)
      (read-history history-filename))

    (define (save-history)
      (write-history history-filename))

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
      (display (implementation-name))
      (display " v")
      (display (implementation-version))
      (newline)
      (display "=============\n\n"))

    (define (end)
      (save-history)
      (display "Bye\n"))

    (define (print-error err)
      (display "\x001b;[31mERROR\x001b;[0m ")
      (if (error-object? err)
          (begin
            (display (error-object-message err))
            (newline)
            (unless (null? (error-object-irritants err))
              (display "irritants:") (newline)
              (let loop ((irrs (error-object-irritants err)) (i 0))
                (unless (null? irrs)
                  (display (format "  [~a]: ~v~%" i (car irrs)))
                  (loop (cdr irrs) (+ 1 i))))))
          (begin
            (display "an error was raised with non-condition value ")
            (write err)
            (newline)))) 

    (define (println obj)
      (unless (void? obj)
        (write obj)
        (newline)))

    ;; filter EOF errors while reading
    ;; to allow user to complete expression
    (define (skip-error? err)
      (and (error-object? err)
           (starts-with? (error-object-message err)
                         "unexpected end of file")))

    (define (repl)
      (register-exit-hook end)
      (banner)
      (load-history)
      (set-completion-function! completions-for)
      (let loop ((buffer ""))
        (let ((line (readline (if (string-empty? buffer) "> " "+ "))))
          (if (not line) ;; empty string -> done
              (newline)
              (let ((input (string-append buffer line "\n")))
                (add-history input)
                (call-with-current-continuation
                  (lambda (return)
                    (with-exception-handler
                      (lambda (err)
                        (unless (skip-error? err)
                          (print-error err)
                          (set! input ""))
                        (return #f))
                      (lambda ()
                        (call-with-port
                          (open-input-string input)
                          (lambda (port)
                           (let read-loop ((expr (read port)))
                             (unless (eof-object? expr)
                               (println (eval expr (interaction-environment)))
                               (set! input "")
                               (read-loop (read port))))))))))
                (loop input))))))
    ))
