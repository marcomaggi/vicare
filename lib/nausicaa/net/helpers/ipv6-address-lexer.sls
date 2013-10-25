#!r6rs
(library (nausicaa net helpers ipv6-address-lexer)
  (export
    ipv6-address-lexer-table)
  (import (nausicaa)
(vicare parser-tools silex input-system)
(ikarus system $fx)
(nausicaa parser-tools silex default-error-handler)
(prefix (nausicaa parser-tools lexical-tokens) lt.)
(prefix (nausicaa parser-tools source-locations) sl.)
)

;
; Table generated from the file ../../lib/nausicaa/net/helpers/ipv6-address-lexer.l by SILex 1.0
;

(define ipv6-address-lexer-table
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       			(silex-default-eof-handler)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
         		(silex-default-error-handler)

;;; end of file
       ))
   (vector
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
       			(lt.<lexical-token> ((lt.category: 'COLON)
					     (lt.location: (sl.<source-location> ((sl.line:   yyline)
										  (sl.column: yycolumn)
										  (sl.offset: yyoffset))))
					     (lt.value:    #\:)
					     (lt.length:   1)))
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
     			(lt.<lexical-token> ((lt.category: 'DOT)
					     (lt.location: (sl.<source-location> ((sl.line:   yyline)
										  (sl.column: yycolumn)
										  (sl.offset: yyoffset))))
					     (lt.value:    #\.)
					     (lt.length:   1)))
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
       			(lt.<lexical-token> ((lt.category: 'SLASH)
					     (lt.location: (sl.<source-location> ((sl.line:   yyline)
										  (sl.column: yycolumn)
										  (sl.offset: yyoffset))))
					     (lt.value:    #\/)
					     (lt.length:   1)))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        		(lt.<lexical-token> ((lt.category: 'NUMBER)
					     (lt.location: (sl.<source-location> ((sl.line:   yyline)
										  (sl.column: yycolumn)
										  (sl.offset: yyoffset))))
					     (lt.value:    yytext)
					     (lt.length:   (string-length yytext))))
        )))
   'decision-trees
   0
   0
   '#((58 (48 (46 err (47 6 5)) (50 2 (51 1 3))) (71 (59 7 (65 err 4)) (97
    err (103 4 err)))) (58 (53 (48 err 10) (54 9 8)) (71 (65 err 8) (97 err
    (103 8 err)))) (65 (48 err (58 11 err)) (97 (71 8 err) (103 8 err)))
    (65 (48 err (58 8 err)) (97 (71 8 err) (103 8 err))) (65 (48 err (58 8
    err)) (97 (71 8 err) (103 8 err))) err err err (65 (48 err (58 12 err))
    (97 (71 12 err) (103 12 err))) (65 (48 err (58 12 err)) (97 (71 12 err)
    (103 12 err))) (65 (48 err (58 12 err)) (97 (71 12 err) (103 12 err)))
    (65 (48 err (58 12 err)) (97 (71 12 err) (103 12 err))) (65 (48 err (58
    13 err)) (97 (71 13 err) (103 13 err))) err)
   '#((#f . #f) (3 . 3) (3 . 3) (3 . 3) (3 . 3) (2 . 2) (1 . 1) (0 . 0) (3
    . 3) (3 . 3) (3 . 3) (3 . 3) (3 . 3) (3 . 3))))

) ; end of library

