#!r6rs
(library (nausicaa net addresses helpers ipv6-address-parser)
  (export make-ipv6-address-parser)
  (import
    (nausicaa)
    (nausicaa parser-tools lalr lr-driver)
    (prefix (nausicaa parser-tools lexical-tokens) lt.)
    (prefix (nausicaa parser-tools source-locations) sl.))
  (define (make-ipv6-address-parser)
    (lr-driver
      '#(((*default* . *error*) (COLON . 2) (NUMBER . 1))
         ((*default* . -9) (SLASH . 5) (COLON . 6))
         ((*default* . *error*) (COLON . 10))
         ((*default* . -4))
         ((*default* . *error*) (*eoi* . 11))
         ((*default* . *error*) (NUMBER . 12))
         ((*default* . *error*) (COLON . 10) (NUMBER . 13))
         ((*default* . -8)) ((*default* . -2))
         ((*default* . -3))
         ((*default* . -14) (NUMBER . 16) (SLASH . 5))
         ((*default* . -1) (*eoi* . accept))
         ((*default* . -19))
         ((*default* . -9) (SLASH . 5) (DOT . 20)
           (COLON . 6)) ((*default* . *error*) (DOT . 23))
         ((*default* . -5))
         ((*default* . -18) (SLASH . 5) (DOT . 20)
           (COLON . 24)) ((*default* . -12))
         ((*default* . -13)) ((*default* . -10))
         ((*default* . *error*) (NUMBER . 27))
         ((*default* . -7)) ((*default* . -6))
         ((*default* . *error*) (NUMBER . 28))
         ((*default* . *error*) (NUMBER . 30))
         ((*default* . -17)) ((*default* . -11))
         ((*default* . -23))
         ((*default* . *error*) (DOT . 20))
         ((*default* . -22) (SLASH . 5))
         ((*default* . -18) (SLASH . 5) (DOT . 20)
           (COLON . 24)) ((*default* . -16))
         ((*default* . -20)) ((*default* . -21))
         ((*default* . -15)))
      (vector '((3 . 3) (1 . 4)) '((6 . 7) (3 . 8) (2 . 9))
        '() '() '() '() '((9 . 14) (7 . 15)) '() '() '()
        '((9 . 14) (7 . 17) (6 . 18) (4 . 19)) '() '()
        '((6 . 7) (3 . 21) (2 . 22)) '() '()
        '((6 . 25) (5 . 26)) '() '() '() '() '() '()
        '((9 . 29)) '((9 . 14) (7 . 31)) '() '() '() '()
        '((8 . 32) (6 . 33)) '((6 . 25) (5 . 34)) '() '()
        '() '())
      (vector '()
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          $1)
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 1
            (cons (string->number $1 16) $2) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 1
            (cons (string->number $1 16) $2) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 1 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 2 $2 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 2
            (cons (string->number $2 16) $3) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 2
            (cons (string->number $2 16) $3) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 2 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 3 (cons #f $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 4
            (cons (string->number $1 16) $2) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 4 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 4 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 4 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 5
            (cons (string->number $2 16) $3) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 5 $2 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 5 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 5 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 6
            `((,(string->number $2))) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $4 $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 4 7 (cons* $1 $3 $4)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 8 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 8 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 9
            (+
              (bitwise-arithmetic-shift-left
                (string->number $1 10) 8)
              (string->number $3 10))
            yy-stack-states yy-stack-values))))))
