#!r6rs
(library (nausicaa net helpers ipv4-address-parser)
  (export make-ipv4-address-parser)
  (import
    (nausicaa)
    (nausicaa parser-tools lalr lr-driver)
    (prefix (nausicaa parser-tools lexical-tokens) lt.)
    (prefix (nausicaa parser-tools source-locations) sl.))
  (define (make-ipv4-address-parser)
    (lr-driver
      '#(((*default* . *error*) (NUMBER . 1))
         ((*default* . *error*) (DOT . 3))
         ((*default* . *error*) (*eoi* . 4))
         ((*default* . *error*) (NUMBER . 5))
         ((*default* . -1) (*eoi* . accept))
         ((*default* . *error*) (DOT . 6))
         ((*default* . *error*) (NUMBER . 7))
         ((*default* . *error*) (DOT . 8))
         ((*default* . *error*) (NUMBER . 9))
         ((*default* . -2) (PREFIX-LENGTH . 10))
         ((*default* . -3)))
      (vector '((1 . 2)) '() '() '() '() '() '() '() '() '()
        '())
      (vector '()
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          $1)
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $7 $6 $5 $4 $3 $2 $1
           . yy-stack-values)
          (yy-reduce-pop-and-push 7 1 (list $1 $3 $5 $7)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $8 $7 $6 $5 $4 $3 $2 $1
           . yy-stack-values)
          (yy-reduce-pop-and-push 8 1
            (list $1 $3 $5 $7 (list $8)) yy-stack-states
            yy-stack-values))))))
