(define-library (sarcasm foreign)
  (import (sarcasm core))
  (export
    foreign-lib
    foreign-obj
    foreign-type-void
    foreign-type-uint8
    foreign-type-sint8
    foreign-type-uint16
    foreign-type-sint16
    foreign-type-uint32
    foreign-type-sint32
    foreign-type-uint64
    foreign-type-sint64
    foreign-type-float
    foreign-type-double
    foreign-type-uchar
    foreign-type-schar
    foreign-type-ushort
    foreign-type-sshort
    foreign-type-uint
    foreign-type-sint
    foreign-type-ulong
    foreign-type-slong
    foreign-type-longdouble
    foreign-type-pointer)

  (begin
    (define foreign-type-void           0)
    (define foreign-type-uint8          1)
    (define foreign-type-sint8          2)
    (define foreign-type-uint16         3)
    (define foreign-type-sint16         4)
    (define foreign-type-uint32         5)
    (define foreign-type-sint32         6)
    (define foreign-type-uint64         7)
    (define foreign-type-sint64         8)
    (define foreign-type-float          9)
    (define foreign-type-double         10)
    (define foreign-type-uchar          11)
    (define foreign-type-schar          12)
    (define foreign-type-ushort         13)
    (define foreign-type-sshort         14)
    (define foreign-type-uint           15)
    (define foreign-type-sint           16)
    (define foreign-type-ulong          17)
    (define foreign-type-slong          18)
    (define foreign-type-longdouble     19)
    (define foreign-type-pointer        20)

    ))
