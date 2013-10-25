#!r6rs
(library (nausicaa net helpers ipv4-address-lexer)
  (export
    ipv4-address-lexer-table)
  (import (nausicaa)
(vicare parser-tools silex input-system)
(ikarus system $fx)
(nausicaa parser-tools silex default-error-handler)
(prefix (nausicaa parser-tools lexical-tokens) lt.)
(prefix (nausicaa parser-tools source-locations) sl.)
)

;
; Table generated from the file ../../lib/nausicaa/net/helpers/ipv4-address-lexer.l by SILex 1.0
;

(define ipv4-address-lexer-table
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
     			(lt.<lexical-token> ((lt.category: 'DOT)
					     (lt.location: (sl.<source-location> ((sl.line:   yyline)
										  (sl.column: yycolumn)
										  (sl.offset: yyoffset))))
					     (lt.value:    #\.)
					     (lt.length:   1)))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        		(lt.<lexical-token> ((lt.category: 'NUMBER)
					     (lt.location: (sl.<source-location> ((sl.line:   yyline)
										  (sl.column: yycolumn)
										  (sl.offset: yyoffset))))
					     (lt.value:    (string->number yytext))
					     (lt.length:   (string-length yytext))))

;; {hexint}		(lt.<lexical-token> ((lt.category: 'NUMBER)
;; 					 (lt.location: (sl.<source-location> ((sl.line:   yyline)
;; 									(sl.column: yycolumn)
;; 									(sl.offset: yyoffset))))
;; 					 (lt.value:    (string->number (substring yytext 2 (string-length yytext)) 16))
;; 					 (lt.length:   (string-length yytext))))

;; {octint}		(lt.<lexical-token> ((lt.cateory: 'NUMBER)
;; 					 (lt.location: (sl.<source-location> ((sl.line:   yyline)
;; 									(sl.column: yycolumn)
;; 									(sl.offset: yyoffset))))
;; 					 (lt.value:    (string->number (substring yytext 1 (string-length yytext)) 8))
;; 					 (lt.category: (string-length yytext))))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
               		(lt.<lexical-token> ((lt.category: 'PREFIX-LENGTH)
					     (lt.location: (sl.<source-location> ((sl.line:   yyline)
										  (sl.column: yycolumn)
										  (sl.offset: yyoffset))))
					     (lt.value:    (string->number (substring yytext 1 (string-length yytext))))
					     (lt.length:   (string-length yytext))))
        )))
   'decision-trees
   0
   0
   '#((49 (47 (46 err 6) (48 1 5)) (51 (50 3 2) (58 4 err))) (50 (48 err
    (49 10 9)) (52 (51 8 7) (58 10 err))) (53 (48 err 12) (54 11 (58 5
    err))) (48 err (58 13 err)) (48 err (58 5 err)) err err (48 err (51 10
    err)) (48 err (58 10 err)) (48 err (58 10 err)) err (48 err (54 5 err))
    (48 err (58 5 err)) (48 err (58 5 err)))
   '#((#f . #f) (#f . #f) (1 . 1) (1 . 1) (1 . 1) (1 . 1) (0 . 0) (2 . 2)
    (2 . 2) (2 . 2) (2 . 2) (1 . 1) (1 . 1) (1 . 1))))

) ; end of library

