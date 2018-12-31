;; (scheme base) library
(define-library (scheme base)
  (import (sarcasm core))
  (export
    * + - < <= = > >=
    begin
    boolean?
    bytevector bytevector-append bytevector-copy bytevector-copy!  bytevector-length
    bytevector-u8-ref bytevector-u8-set!  bytevector?
    car cdr
    char->integer char-alphabetic?  char-ci<=?  char-ci<?  char-ci=?  char-ci>=?
    char-ci>?  char-downcase char-lower-case?  char-numeric?  char-upcase
    char-upper-case?  char-whitespace?  char<=?  char<?  char=?  char>=?
    char>?  char?
    close-input-port
    close-output-port
    command-line
    cons
    current-error-port
    current-input-port
    current-output-port
    current-second
    delete-file
    digit-value
    display
    emergency-exit
    eof-object?
    eq?
    equal?
    eqv?
    exact-integer?
    exact?
    exact
    truncate
    exit
    features
    file-exists?
    finite?
    flush-output-port
    get-environment-variable
    get-environment-variables
    get-output-string
    inexact?
    infinite?
    input-port?
    integer->char
    integer?
    length
    list
    list->string
    list->vector
    list?
    load
    make-bytevector
    make-list
    make-string
    make-vector
    nan?
    negative?
    newline
    not
    null?
    number?
    open-input-file
    open-input-string
    open-output-file
    open-output-string
    output-port?
    pair?
    peek-char
    port-open?
    port?
    positive?
    read-char
    read-line
    real?
    set-car!
    set-cdr!
    string
    string->list
    string->symbol
    string->utf8
    string->vector
    string-append
    string-ci<=?
    string-ci<?
    string-ci=?
    string-ci>=?
    string-ci>?
    string-copy
    string-copy!
    string-downcase
    string-fill!
    string-length
    string-ref
    string-set!
    string-upcase
    string<=?
    string<?
    string=?
    string>=?
    string>?
    string?
    substring
    symbol->string
    symbol=?
    symbol?
    utf8->string
    vector
    vector->list
    vector->string
    vector-append
    vector-copy
    vector-copy!
    vector-fill!
    vector-length
    vector-ref
    vector-set!
    vector?
    void
    write
    write-char
    zero?))

        
