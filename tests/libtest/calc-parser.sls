#!r6rs
(library (libtest calc-parser)
  (export make-calc-parser)
  (import
    (nausicaa)
    (nausicaa parser-tools lalr lr-driver)
    (prefix (nausicaa parser-tools lexical-tokens) lt.)
    (prefix (nausicaa parser-tools source-locations) sl.)
    (libtest calc-parser-helper)
    (rnrs eval))
  (define (make-calc-parser)
    (lr-driver
      '#(((*default* . *error*) (error . 6) (ID . 5)
           (NUM . 4) (LPAREN . 3) (+ . 2) (- . 1))
         ((*default* . *error*) (ID . 12) (NUM . 4)
           (LPAREN . 3) (+ . 2) (- . 1))
         ((*default* . *error*) (ID . 12) (NUM . 4)
           (LPAREN . 3) (+ . 2) (- . 1))
         ((*default* . *error*) (ID . 12) (NUM . 4)
           (LPAREN . 3) (+ . 2) (- . 1)) ((*default* . -25))
         ((*default* . -23) (ASSIGN . 17) (LPAREN . 16))
         ((*default* . *error*) (NEWLINE . 18))
         ((*default* . *error*) (NEWLINE . 31) (+ . 30)
           (- . 29) (* . 28) (/ . 27) (DIV . 26) (MOD . 25)
           (EXPT . 24) (LESS . 23) (GREAT . 22)
           (LESSEQ . 21) (GREATEQ . 20) (EQUAL . 19))
         ((*default* . *error*) (NEWLINE . 32))
         ((*default* . -4))
         ((*default* . -2) (- . 1) (+ . 2) (LPAREN . 3)
           (NUM . 4) (ID . 5) (error . 6))
         ((*default* . *error*) (*eoi* . 34))
         ((*default* . -23) (LPAREN . 16))
         ((*default* . -14) (EQUAL . 19) (GREATEQ . 20)
           (LESSEQ . 21) (GREAT . 22) (LESS . 23)
           (EXPT . 24) (MOD . 25) (DIV . 26) (/ . 27)
           (* . 28)) ((*default* . -13))
         ((*default* . *error*) (RPAREN . 35) (+ . 30)
           (- . 29) (* . 28) (/ . 27) (DIV . 26) (MOD . 25)
           (EXPT . 24) (LESS . 23) (GREAT . 22)
           (LESSEQ . 21) (GREATEQ . 20) (EQUAL . 19))
         ((*default* . -27) (- . 1) (+ . 2) (LPAREN . 3)
           (NUM . 4) (ID . 12))
         ((*default* . *error*) (ID . 12) (NUM . 4)
           (LPAREN . 3) (+ . 2) (- . 1)) ((*default* . -7))
         ((*default* . *error*) (ID . 12) (NUM . 4)
           (LPAREN . 3) (+ . 2) (- . 1))
         ((*default* . *error*) (ID . 12) (NUM . 4)
           (LPAREN . 3) (+ . 2) (- . 1))
         ((*default* . *error*) (ID . 12) (NUM . 4)
           (LPAREN . 3) (+ . 2) (- . 1))
         ((*default* . *error*) (ID . 12) (NUM . 4)
           (LPAREN . 3) (+ . 2) (- . 1))
         ((*default* . *error*) (ID . 12) (NUM . 4)
           (LPAREN . 3) (+ . 2) (- . 1))
         ((*default* . *error*) (ID . 12) (NUM . 4)
           (LPAREN . 3) (+ . 2) (- . 1))
         ((*default* . *error*) (ID . 12) (NUM . 4)
           (LPAREN . 3) (+ . 2) (- . 1))
         ((*default* . *error*) (ID . 12) (NUM . 4)
           (LPAREN . 3) (+ . 2) (- . 1))
         ((*default* . *error*) (ID . 12) (NUM . 4)
           (LPAREN . 3) (+ . 2) (- . 1))
         ((*default* . *error*) (ID . 12) (NUM . 4)
           (LPAREN . 3) (+ . 2) (- . 1))
         ((*default* . *error*) (ID . 12) (NUM . 4)
           (LPAREN . 3) (+ . 2) (- . 1))
         ((*default* . *error*) (ID . 12) (NUM . 4)
           (LPAREN . 3) (+ . 2) (- . 1)) ((*default* . -6))
         ((*default* . -5)) ((*default* . -3))
         ((*default* . -1) (*eoi* . accept))
         ((*default* . -26))
         ((*default* . *error*) (RPAREN . 51))
         ((*default* . -30) (EQUAL . 19) (GREATEQ . 20)
           (LESSEQ . 21) (GREAT . 22) (LESS . 23)
           (EXPT . 24) (MOD . 25) (DIV . 26) (/ . 27)
           (* . 28) (- . 29) (+ . 30) (COMMA . 52))
         ((*default* . -8) (+ . 30) (- . 29) (* . 28)
           (/ . 27) (DIV . 26) (MOD . 25) (EXPT . 24)
           (LESS . 23) (GREAT . 22) (LESSEQ . 21)
           (GREATEQ . 20) (EQUAL . 19)) ((*default* . -22))
         ((*default* . -21)) ((*default* . -20))
         ((*default* . -19)) ((*default* . -18))
         ((*default* . -17)) ((*default* . -16))
         ((*default* . -15)) ((*default* . -12))
         ((*default* . -11))
         ((*default* . -10) (EQUAL . 19) (GREATEQ . 20)
           (LESSEQ . 21) (GREAT . 22) (LESS . 23)
           (EXPT . 24) (MOD . 25) (DIV . 26) (/ . 27)
           (* . 28))
         ((*default* . -9) (EQUAL . 19) (GREATEQ . 20)
           (LESSEQ . 21) (GREAT . 22) (LESS . 23)
           (EXPT . 24) (MOD . 25) (DIV . 26) (/ . 27)
           (* . 28)) ((*default* . -24))
         ((*default* . *error*) (ID . 12) (NUM . 4)
           (LPAREN . 3) (+ . 2) (- . 1)) ((*default* . -28))
         ((*default* . -30) (EQUAL . 19) (GREATEQ . 20)
           (LESSEQ . 21) (GREAT . 22) (LESS . 23)
           (EXPT . 24) (MOD . 25) (DIV . 26) (/ . 27)
           (* . 28) (- . 29) (+ . 30) (COMMA . 52))
         ((*default* . -29)))
      (vector '((5 . 7) (4 . 8) (3 . 9) (2 . 10) (1 . 11))
        '((5 . 13)) '((5 . 14)) '((5 . 15)) '() '() '() '()
        '() '() '((5 . 7) (4 . 8) (3 . 33)) '() '() '() '()
        '() '((6 . 36) (5 . 37)) '((5 . 38)) '() '((5 . 39))
        '((5 . 40)) '((5 . 41)) '((5 . 42)) '((5 . 43))
        '((5 . 44)) '((5 . 45)) '((5 . 46)) '((5 . 47))
        '((5 . 48)) '((5 . 49)) '((5 . 50)) '() '() '() '()
        '() '() '((7 . 53)) '() '() '() '() '() '() '() '()
        '() '() '() '() '() '() '((5 . 54)) '() '((7 . 55))
        '())
      (vector '()
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          $1)
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 1 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 2
            (let ((result $2))
              (when result
                (evaluated-expressions
                  (cons result (evaluated-expressions))))
              result)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2
            (let ((result $1))
              (when result
                (evaluated-expressions
                  (cons result (evaluated-expressions))))
              result)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 3 #f yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 4
            (begin
              (hashtable-set! (table-of-variables) $1 $3)
              #f)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 5 (+ $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 5 (- $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 5 (* $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 5 (/ $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 5 $2 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 5 (- $2) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 5 (div $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 5 (mod $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 5 (expt $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 5 (< $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 5 (> $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 5 (<= $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 5 (>= $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 5 (= $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 5
            (hashtable-ref (table-of-variables) $1 #f)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $4 $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 4 5
            (apply (eval $1 (environment '(rnrs))) $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 5 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 5 $2 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 6 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 6 (cons $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 7 (cons $2 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 7 '() yy-stack-states
            yy-stack-values))))))
