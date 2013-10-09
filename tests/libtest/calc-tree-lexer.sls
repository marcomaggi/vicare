#!r6rs
(library (libtest calc-tree-lexer)
  (export
    calc-lexer-table/tree)
  (import (rnrs)
(vicare parser-tools silex input-system)
)

;
; Table generated from the file ../../tests/calc.l by SILex 1.0
;

(define calc-lexer-table/tree
  (vector
   'line
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline)
       		(eof-object)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline)
         	(assertion-violation #f
                  "invalid lexer token")
       ))
   (vector
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
        	;; skip blanks, tabs and newlines
        (yycontinue)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
      		(string->number (string-append "+" yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
      		(string->number yytext)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
     		+nan.0
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
      		+inf.0
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
      		-inf.0
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
          	(case (string->symbol yytext)
		  ((+) '+)
		  ((-) '-)
		  ((*) '*)
		  ((/) '/)
		  ((%) 'mod)
		  ((^) 'expt)
		  ((//) 'div)
		  ((=) '=)
		  ((<) '<)
		  ((>) '>)
		  ((<=) '<=)
		  ((>=) '>=))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
        	(string->symbol yytext)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
       		'cons
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
        	#\(
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
        	#\)
        )))
   'decision-trees
   0
   0
   '#((47 (37 (14 (11 (9 err 16) (13 err 16)) (33 (32 err 16) (= 35 15
    err))) (42 (40 (38 8 err) (41 2 1)) (44 (43 8 11) (45 3 (46 12 13)))))
    (94 (61 (58 (48 5 14) (60 err 7)) (63 (62 8 6) (65 err (91 4 err))))
    (105 (96 (95 8 4) (97 err 4)) (110 (106 9 4) (111 10 (123 4 err))))))
    err err err (48 (37 (34 (33 err 4) (36 err 4)) (39 (38 err 4) (45 err
    (47 4 err)))) (96 (60 (59 4 err) (91 4 (95 err 4))) (123 (97 err 4) (=
    126 4 err)))) (= 47 8 err) (= 61 8 err) (= 61 8 err) err (59 (38 (34
    (33 err 4) (= 36 4 err)) (45 (39 4 err) (= 47 err 4))) (97 (91 (60 err
    4) (= 95 4 err)) (123 (= 110 17 4) (= 126 4 err)))) (59 (38 (34 (33 err
    4) (= 36 4 err)) (45 (39 4 err) (= 47 err 4))) (97 (91 (60 err 4) (= 95
    4 err)) (123 (98 18 4) (= 126 4 err)))) (106 (105 err 19) (= 110 20
    err)) (106 (105 err 21) (= 110 22 err)) (48 err (58 23 err)) (69 (47
    (46 err 25) (48 err (58 14 err))) (102 (70 24 (101 err 24)) (= 105 26
    err))) (89 (79 (= 66 29 err) (80 28 (88 err 27))) (111 (= 98 29 err)
    (120 (112 28 err) (121 27 err)))) (13 (9 err (11 16 err)) (32 (14 16
    err) (33 16 err))) (59 (38 (34 (33 err 4) (= 36 4 err)) (45 (39 4 err)
    (= 47 err 4))) (97 (91 (60 err 4) (= 95 4 err)) (123 (= 102 30 4) (=
    126 4 err)))) (59 (38 (34 (33 err 4) (= 36 4 err)) (45 (39 4 err) (= 47
    err 4))) (97 (91 (60 err 4) (= 95 4 err)) (123 (= 110 31 4) (= 126 4
    err)))) (= 110 32 err) (= 97 33 err) (= 110 34 err) (= 97 35 err) (70
    (58 (48 err 23) (69 err 36)) (102 (101 err 36) (= 105 26 err))) (45 (=
    43 38 err) (48 (46 38 err) (58 37 err))) (58 (48 err 23) (= 105 26
    err)) err (65 (48 err (58 39 err)) (97 (71 39 err) (103 39 err))) (48
    err (56 40 err)) (48 err (50 41 err)) (48 (38 (34 (33 err 4) (= 36 4
    err)) (45 (39 4 err) (46 4 (47 42 err)))) (96 (60 (59 4 err) (91 4 (95
    err 4))) (123 (97 err 4) (= 126 4 err)))) (48 (38 (34 (33 err 4) (= 36
    4 err)) (45 (39 4 err) (46 4 (47 43 err)))) (96 (60 (59 4 err) (91 4
    (95 err 4))) (123 (97 err 4) (= 126 4 err)))) (= 102 44 err) (= 110 45
    err) (= 102 46 err) (= 110 47 err) (45 (= 43 49 err) (48 (46 49 err)
    (58 48 err))) (58 (48 err 37) (= 105 26 err)) (48 err (58 37 err)) (71
    (58 (48 err 39) (65 err 39)) (103 (97 err 39) (= 105 26 err))) (56 (48
    err 40) (= 105 26 err)) (50 (48 err 41) (= 105 26 err)) (49 (38 (34 (33
    err 4) (= 36 4 err)) (45 (39 4 err) (47 4 (48 err 50)))) (96 (60 (59 4
    err) (91 4 (95 err 4))) (123 (97 err 4) (= 126 4 err)))) (49 (38 (34
    (33 err 4) (= 36 4 err)) (45 (39 4 err) (47 4 (48 err 51)))) (96 (60
    (59 4 err) (91 4 (95 err 4))) (123 (97 err 4) (= 126 4 err)))) (= 46 52
    err) (= 46 53 err) (= 46 54 err) (= 46 55 err) (58 (48 err 48) (= 105
    26 err)) (48 err (58 48 err)) (48 (37 (34 (33 err 4) (36 err 4)) (39
    (38 err 4) (45 err (47 4 err)))) (96 (60 (59 4 err) (91 4 (95 err 4)))
    (123 (97 err 4) (= 126 4 err)))) (48 (37 (34 (33 err 4) (36 err 4)) (39
    (38 err 4) (45 err (47 4 err)))) (96 (60 (59 4 err) (91 4 (95 err 4)))
    (123 (97 err 4) (= 126 4 err)))) (= 48 56 err) (= 48 57 err) (= 48 58
    err) (= 48 57 err) err err err)
   '#((#f . #f) (10 . 10) (9 . 9) (8 . 8) (7 . 7) (6 . 6) (6 . 6) (6 . 6)
    (6 . 6) (7 . 7) (7 . 7) (6 . 6) (6 . 6) (#f . #f) (2 . 2) (#f . #f) (0
    . 0) (7 . 7) (7 . 7) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (2 . 2)
    (#f . #f) (2 . 2) (1 . 1) (#f . #f) (#f . #f) (#f . #f) (7 . 7) (7 . 7)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (2 . 2) (#f . #f) (2
    . 2) (2 . 2) (2 . 2) (7 . 7) (7 . 7) (#f . #f) (#f . #f) (#f . #f) (#f
    . #f) (2 . 2) (#f . #f) (4 . 4) (3 . 3) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (4 . 4) (3 . 3) (5 . 5))))

) ; end of library

