#!r6rs
(library (vicare parser-tools silex class-l)
  (export
    class-tables)
  (import (rnrs)
(vicare parser-tools silex input-system)
(vicare parser-tools silex semantic)
)

;
; Table generated from the file ../../lib/vicare/parser-tools/silex/class.l by SILex 1.0
;

(define class-tables
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       			(make-tok eof-tok    yytext yyline yycolumn)

;;; end of file
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       (assertion-violation #f "invalid token")
       ))
   (vector
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
   			(make-tok rbrack-tok yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
   			(make-tok minus-tok  yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
     			(parse-spec-char     yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
            		(parse-digits-char   yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
             		(parse-digits-char   yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                     	(parse-hex-digits-char yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
       			(parse-quoted-char   yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
   			(parse-ordinary-char yytext yyline yycolumn)
        )))
   'code
   (lambda (<<EOF>>-pre-action
            <<ERROR>>-pre-action
            rules-pre-action
            IS)
     (letrec
         ((user-action-<<EOF>> #f)
          (user-action-<<ERROR>> #f)
          (user-action-0 #f)
          (user-action-1 #f)
          (user-action-2 #f)
          (user-action-3 #f)
          (user-action-4 #f)
          (user-action-5 #f)
          (user-action-6 #f)
          (user-action-7 #f)
          (start-go-to-end    (<input-system>-start-go-to-end	IS))
          (end-go-to-point    (<input-system>-end-go-to-point	IS))
          (init-lexeme        (<input-system>-init-lexeme	IS))
          (get-start-line     (<input-system>-get-start-line	IS))
          (get-start-column   (<input-system>-get-start-column	IS))
          (get-start-offset   (<input-system>-get-start-offset	IS))
          (peek-left-context  (<input-system>-peek-left-context	IS))
          (peek-char          (<input-system>-peek-char		IS))
          (read-char          (<input-system>-read-char		IS))
          (get-start-end-text (<input-system>-get-start-end-text IS))
          (user-getc          (<input-system>-user-getc		IS))
          (user-ungetc        (<input-system>-user-ungetc	IS))
          (action-<<EOF>>
           (lambda (yyline yycolumn yyoffset)
             (user-action-<<EOF>> "" yyline yycolumn yyoffset)))
          (action-<<ERROR>>
           (lambda (yyline yycolumn yyoffset)
             (user-action-<<ERROR>> "" yyline yycolumn yyoffset)))
          (action-0
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-0 yytext yyline yycolumn yyoffset))))
          (action-1
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-1 yytext yyline yycolumn yyoffset))))
          (action-2
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-2 yytext yyline yycolumn yyoffset))))
          (action-3
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-3 yytext yyline yycolumn yyoffset))))
          (action-4
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-4 yytext yyline yycolumn yyoffset))))
          (action-5
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-5 yytext yyline yycolumn yyoffset))))
          (action-6
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-6 yytext yyline yycolumn yyoffset))))
          (action-7
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-7 yytext yyline yycolumn yyoffset))))
          (state-0
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 46)
                       (if (< c 36)
                           (if (< c 35)
                               (state-1 action)
                               (state-2 action))
                           (if (< c 45)
                               (state-1 action)
                               (state-4 action)))
                       (if (< c 93)
                           (if (< c 92)
                               (state-1 action)
                               (state-3 action))
                           (if (< c 94)
                               (state-5 action)
                               (state-1 action))))
                   action))))
          (state-1
           (lambda (action)
             (end-go-to-point)
             action-7))
          (state-2
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 89)
                       (if (< c 88)
                           action-7
                           (state-6 action-7))
                       (if (= c 120)
                           (state-6 action-7)
                           action-7))
                   action-7))))
          (state-3
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 48)
                       (if (= c 45)
                           (state-8 action-7)
                           (state-7 action-7))
                       (if (< c 110)
                           (if (< c 58)
                               (state-9 action-7)
                               (state-7 action-7))
                           (if (< c 111)
                               (state-10 action-7)
                               (state-7 action-7))))
                   action-7))))
          (state-4
           (lambda (action)
             (end-go-to-point)
             action-1))
          (state-5
           (lambda (action)
             (end-go-to-point)
             action-0))
          (state-6
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 65)
                       (if (< c 48)
                           action
                           (if (< c 58)
                               (state-11 action)
                               action))
                       (if (< c 97)
                           (if (< c 71)
                               (state-11 action)
                               action)
                           (if (< c 103)
                               (state-11 action)
                               action)))
                   action))))
          (state-7
           (lambda (action)
             (end-go-to-point)
             action-6))
          (state-8
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 48)
                       action-6
                       (if (< c 58)
                           (state-12 action-6)
                           action-6))
                   action-6))))
          (state-9
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 48)
                       action-3
                       (if (< c 58)
                           (state-13 action-3)
                           action-3))
                   action-3))))
          (state-10
           (lambda (action)
             (end-go-to-point)
             action-2))
          (state-11
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 65)
                       (if (< c 48)
                           action-5
                           (if (< c 58)
                               (state-11 action-5)
                               action-5))
                       (if (< c 97)
                           (if (< c 71)
                               (state-11 action-5)
                               action-5)
                           (if (< c 103)
                               (state-11 action-5)
                               action-5)))
                   action-5))))
          (state-12
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 48)
                       action-4
                       (if (< c 58)
                           (state-12 action-4)
                           action-4))
                   action-4))))
          (state-13
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 48)
                       action-3
                       (if (< c 58)
                           (state-13 action-3)
                           action-3))
                   action-3))))
          (start-automaton
           (lambda ()
             (if (peek-char)
                 (state-0 action-<<ERROR>>)
               action-<<EOF>>)))
          (final-lexer
           (lambda ()
             (init-lexeme)
             (let ((yyline (get-start-line))
                   (yycolumn (get-start-column))
                   (yyoffset (get-start-offset)))
               ((start-automaton) yyline yycolumn yyoffset)))))
       (set! user-action-<<EOF>> (<<EOF>>-pre-action
                                  final-lexer user-getc user-ungetc))
       (set! user-action-<<ERROR>> (<<ERROR>>-pre-action
                                    final-lexer user-getc user-ungetc))
       (set! user-action-0 ((vector-ref rules-pre-action 1)
                            final-lexer user-getc user-ungetc))
       (set! user-action-1 ((vector-ref rules-pre-action 3)
                            final-lexer user-getc user-ungetc))
       (set! user-action-2 ((vector-ref rules-pre-action 5)
                            final-lexer user-getc user-ungetc))
       (set! user-action-3 ((vector-ref rules-pre-action 7)
                            final-lexer user-getc user-ungetc))
       (set! user-action-4 ((vector-ref rules-pre-action 9)
                            final-lexer user-getc user-ungetc))
       (set! user-action-5 ((vector-ref rules-pre-action 11)
                            final-lexer user-getc user-ungetc))
       (set! user-action-6 ((vector-ref rules-pre-action 13)
                            final-lexer user-getc user-ungetc))
       (set! user-action-7 ((vector-ref rules-pre-action 15)
                            final-lexer user-getc user-ungetc))
       final-lexer))))

) ; end of library

