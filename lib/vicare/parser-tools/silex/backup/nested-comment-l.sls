#!r6rs
(library (vicare parser-tools silex nested-comment-l)
  (export
    nested-comment-tables)
  (import (rnrs)
(vicare parser-tools silex input-system)
(vicare parser-tools silex semantic)
)

;
; Table generated from the file nested-comment.l by SILex 1.0
;

(define nested-comment-tables
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       		(make-tok eof-tok		yytext yyline yycolumn)

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
      		(make-tok open-comment-tok	yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
       		(make-tok close-comment-tok	yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
      		(parse-ordinary-char		yytext yyline yycolumn)
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
          (state-0
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 36)
                       (if (< c 35)
                           (state-1 action)
                           (state-3 action))
                       (if (= c 124)
                           (state-2 action)
                           (state-1 action)))
                   action))))
          (state-1
           (lambda (action)
             (end-go-to-point)
             action-2))
          (state-2
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (= c 35)
                       (state-4 action-2)
                       action-2)
                   action-2))))
          (state-3
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (= c 124)
                       (state-5 action-2)
                       action-2)
                   action-2))))
          (state-4
           (lambda (action)
             (end-go-to-point)
             action-1))
          (state-5
           (lambda (action)
             (end-go-to-point)
             action-0))
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
       final-lexer))))

) ; end of library

