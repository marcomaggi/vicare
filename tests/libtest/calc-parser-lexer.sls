#!r6rs
(library (libtest calc-parser-lexer)
  (export
    calc-parser-lexer-table)
  (import (nausicaa)
(vicare parser-tools silex input-system)
(ikarus system $fx)
(prefix (nausicaa parser-tools lexical-tokens) lt.)
(prefix (nausicaa parser-tools source-locations) sl.)
(nausicaa parser-tools silex default-error-handler)
)

;
; Table generated from the file #f by SILex 1.0
;

(define calc-parser-lexer-table
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       		(silex-default-eof-handler)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
         	(assertion-violation #f "invalid lexer token")
       ))
   (vector
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
        	;; skip spaced and tabs
        (yycontinue)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
      		(lt.<lexical-token> ((lt.category: 'NUM)
				     (lt.location: (sl.<source-location> ((sl.line:   yyline)
									  (sl.column: yycolumn)
									  (sl.offset: yyoffset))))
				     (lt.value:    (string->number (string-append "+" yytext)))
				     (lt.length:   (string-length yytext))))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
      		(lt.<lexical-token> ((lt.category: 'NUM)
				     (lt.location: (sl.<source-location> ((sl.line:   yyline)
									  (sl.column: yycolumn)
									  (sl.offset: yyoffset))))
				     (lt.value:    (string->number yytext))
				     (lt.length:   (string-length yytext))))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
     		(lt.<lexical-token> ((lt.category: 'NUM)
				     (lt.location: (sl.<source-location> ((sl.line:   yyline)
									  (sl.column: yycolumn)
									  (sl.offset: yyoffset))))
				     (lt.value:    +nan.0)
				     (lt.length:   (string-length yytext))))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
     		(lt.<lexical-token> ((lt.category: 'NUM)
				     (lt.location: (sl.<source-location> ((sl.line:   yyline)
									  (sl.column: yycolumn)
									  (sl.offset: yyoffset))))
				     (lt.value:    +inf.0)
				     (lt.length:   (string-length yytext))))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
          	(let ((position (sl.<source-location> ((sl.line:   yyline)
						       (sl.column: yycolumn)
						       (sl.offset: yyoffset))))
		      (len	(string-length yytext)))
		  (define (%make-lt category value len)
		    (lt.<lexical-token> ((lt.category: category)
					 (lt.location: position)
					 (lt.value:    value)
					 (lt.length:   len))))
		  (case (string->symbol yytext)
		    ((+)	(%make-lt '+ '+ len))
		    ((-)	(%make-lt '- '- len))
		    ((*)	(%make-lt '* '* len))
		    ((/)	(%make-lt '/ '/ len))
		    ((%)	(%make-lt 'MOD mod len))
		    ((^)	(%make-lt 'EXPT expt len))
		    ((\x5C;)	(%make-lt 'DIV div len))
		    ((<)	(%make-lt 'LESS < len))
		    ((>)	(%make-lt 'GREAT > len))
		    ((<=)	(%make-lt 'LESSEQ <= len))
		    ((>=)	(%make-lt 'GREATEQ >= len))
		    ((==)	(%make-lt 'EQUAL = len))
		    (else       (error #f "unknown operator" yytext len))))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        	(lt.<lexical-token> ((lt.category: 'ID)
				     (lt.location: (sl.<source-location> ((sl.line:   yyline)
									  (sl.column: yycolumn)
									  (sl.offset: yyoffset))))
				     (lt.value:    (string->symbol yytext))
				     (lt.length:   (string-length yytext))))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        	(lt.<lexical-token> ((lt.category: 'ASSIGN)
				     (lt.location: (sl.<source-location> ((sl.line:   yyline)
									  (sl.column: yycolumn)
									  (sl.offset: yyoffset))))
				     (lt.value:    'ASSIGN)
				     (lt.length:   (string-length yytext))))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
       		(lt.<lexical-token> ((lt.category: 'COMMA)
				     (lt.location: (sl.<source-location> ((sl.line:   yyline)
									  (sl.column: yycolumn)
									  (sl.offset: yyoffset))))
				     (lt.value:    'COMMA)
				     (lt.length:    (string-length yytext))))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
         	(lt.<lexical-token> ((lt.category: 'NEWLINE)
				     (lt.location: (sl.<source-location> ((sl.line:   yyline)
									  (sl.column: yycolumn)
									  (sl.offset: yyoffset))))
				     (lt.value:    'NEWLINE)
				     (lt.length:   (string-length yytext))))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        	(lt.<lexical-token> ((lt.category: 'LPAREN)
				     (lt.location: (sl.<source-location> ((sl.line:   yyline)
									  (sl.column: yycolumn)
									  (sl.offset: yyoffset))))
				     (lt.value:    'LPAREN)
				     (lt.length:   (string-length yytext))))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        	(lt.<lexical-token> ((lt.category: 'RPAREN)
				     (lt.location: (sl.<source-location> ((sl.line:   yyline)
									  (sl.column: yycolumn)
									  (sl.offset: yyoffset))))
				     (lt.value:    'RPAREN)
				     (lt.length:   (string-length yytext))))
        )))
   'decision-trees
   0
   0
   '#((47 (36 (13 (10 (9 err 17) (11 3 err)) (32 (14 3 err) (33 17 (35 err
    16)))) (42 (38 (37 err 8) (40 err (41 2 1))) (44 (43 8 12) (45 4 (46 13
    14))))) (93 (62 (58 (48 8 15) (60 err (61 7 9))) (65 (63 6 err) (91 5
    (92 err 8)))) (105 (95 (94 err 8) (= 96 err 5)) (110 (106 10 5) (111 11
    (123 5 err)))))) err err (11 (10 err 3) (= 13 3 err)) err (64 (47 (46
    err 5) (48 err (58 5 err))) (96 (91 5 (95 err 5)) (97 err (123 5
    err)))) (= 61 8 err) (= 61 8 err) err (= 61 8 err) (91 (48 (= 46 5 err)
    (58 5 (64 err 5))) (97 (= 95 5 err) (111 (110 5 18) (123 5 err)))) (91
    (48 (= 46 5 err) (58 5 (64 err 5))) (97 (= 95 5 err) (98 19 (123 5
    err)))) (= 110 20 err) (= 110 21 err) (48 err (58 22 err)) (69 (47 (46
    err 24) (48 err (58 15 err))) (102 (70 23 (101 err 23)) (= 105 25
    err))) (89 (79 (= 66 28 err) (80 27 (88 err 26))) (111 (= 98 28 err)
    (120 (112 27 err) (121 26 err)))) (10 (9 err 17) (= 32 17 err)) (91 (48
    (= 46 5 err) (58 5 (64 err 5))) (97 (= 95 5 err) (103 (102 5 29) (123 5
    err)))) (91 (48 (= 46 5 err) (58 5 (64 err 5))) (97 (= 95 5 err) (111
    (110 5 30) (123 5 err)))) (= 97 31 err) (= 97 32 err) (70 (58 (48 err
    22) (69 err 33)) (102 (101 err 33) (= 105 25 err))) (45 (= 43 35 err)
    (48 (46 35 err) (58 34 err))) (58 (48 err 22) (= 105 25 err)) err (65
    (48 err (58 36 err)) (97 (71 36 err) (103 36 err))) (48 err (56 37
    err)) (48 err (50 38 err)) (64 (47 (46 err 39) (48 err (58 5 err))) (96
    (91 5 (95 err 5)) (97 err (123 5 err)))) (64 (47 (46 err 40) (48 err
    (58 5 err))) (96 (91 5 (95 err 5)) (97 err (123 5 err)))) (= 110 41
    err) (= 110 42 err) (45 (= 43 44 err) (48 (46 44 err) (58 43 err))) (58
    (48 err 34) (= 105 25 err)) (48 err (58 34 err)) (71 (58 (48 err 36)
    (65 err 36)) (103 (97 err 36) (= 105 25 err))) (56 (48 err 37) (= 105
    25 err)) (50 (48 err 38) (= 105 25 err)) (64 (48 (= 46 5 err) (49 45
    (58 5 err))) (96 (91 5 (95 err 5)) (97 err (123 5 err)))) (64 (48 (= 46
    5 err) (49 46 (58 5 err))) (96 (91 5 (95 err 5)) (97 err (123 5 err))))
    (= 46 47 err) (= 46 48 err) (58 (48 err 43) (= 105 25 err)) (48 err (58
    43 err)) (64 (47 (46 err 5) (48 err (58 5 err))) (96 (91 5 (95 err 5))
    (97 err (123 5 err)))) (64 (47 (46 err 5) (48 err (58 5 err))) (96 (91
    5 (95 err 5)) (97 err (123 5 err)))) (= 48 49 err) (= 48 49 err) err)
   '#((#f . #f) (11 . 11) (10 . 10) (9 . 9) (8 . 8) (6 . 6) (5 . 5) (5 . 5)
    (5 . 5) (7 . 7) (6 . 6) (6 . 6) (5 . 5) (5 . 5) (#f . #f) (2 . 2) (#f .
    #f) (0 . 0) (6 . 6) (6 . 6) (#f . #f) (#f . #f) (2 . 2) (#f . #f) (2 .
    2) (1 . 1) (#f . #f) (#f . #f) (#f . #f) (6 . 6) (6 . 6) (#f . #f) (#f
    . #f) (#f . #f) (2 . 2) (#f . #f) (2 . 2) (2 . 2) (2 . 2) (6 . 6) (6 .
    6) (#f . #f) (#f . #f) (2 . 2) (#f . #f) (4 . 4) (3 . 3) (#f . #f) (#f
    . #f) (3 . 3))))

) ; end of library

