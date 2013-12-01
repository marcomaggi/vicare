;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for silex error handling
;;;Date: Fri Jul  8, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (nausicaa)
  (prefix (vicare parser-tools silex) lex.)
  (prefix (vicare parser-tools silex lexer) lex.)
  (prefix (nausicaa parser-tools lexical-tokens) pt.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare parser-tools: SILex error handling\n")


(parametrise ((check-test-name	'error-recovery))

  (define description "%%
A		(<lexical-token>
                  ((category: 'A)
                   (location: (<source-location> ((line: yyline) (column: yycolumn) (offset: yyoffset))))
                   (value: yytext)
                   (length: (string-length yytext))))

<<EOF>>		(<lexical-token>
                  ((category: '*eoi*)
                   (location: (<source-location> ((line: yyline) (column: yycolumn) (offset: yyoffset))))
                   (value: (eof-object))
                   (length: 1)))

<<ERROR>>	(<lexical-token>
                  ((category: '*lexer-error*)
                   (location: (<source-location> ((line: yyline) (column: yycolumn) (offset: yyoffset))))
                   (value: yytext)
                   (length: (string-length yytext))))
")

  (define table
    (lex.lex (lex.input-string:		description)
	     (lex.counters:		'all)
	     (lex.library-language:	'(vicare))
	     (lex.library-imports:	'((nausicaa parser-tools lexical-tokens)
					  (nausicaa parser-tools source-locations)))
	     (lex.output-value:		#t)
	     (lex.lexer-format:		'decision-tree)))

  (check	;correct string
      (let* ((IS	(lex.make-IS (lex.string: "AAA")
				     (lex.counters: 'all)))
	     (lexer (lex.make-lexer table IS)))
        (let* (((T1 pt.<lexical-token>) (lexer))
	       ((T2 pt.<lexical-token>) (lexer))
	       ((T3 pt.<lexical-token>) (lexer))
	       ((T4 pt.<lexical-token>) (lexer)))
	  (list (T1 category)
		(T2 category)
		(T3 category)
		(T4 category))))
    => '(A A A *eoi*))

  (check	;lexer error
      (let* ((IS	(lex.make-IS (lex.string: "AAAB")
				     (lex.counters: 'all)))
	     (lexer (lex.make-lexer table IS)))
        (let* (((T1 pt.<lexical-token>) (lexer))
	       ((T2 pt.<lexical-token>) (lexer))
	       ((T3 pt.<lexical-token>) (lexer))
	       ((T4 pt.<lexical-token>) (lexer)))
	  (list (T1 category)
		(T2 category)
		(T3 category)
		(T4 category))))
    => '(A A A *lexer-error*))

  (check	;lexer error and recovery
      (let* ((IS	(lex.make-IS (lex.string: "AAABBAA")
				     (lex.counters: 'all)))
	     (lexer (lex.make-lexer table IS)))
        (let* (((T1 pt.<lexical-token>) (lexer))
	       ((T2 pt.<lexical-token>) (lexer))
	       ((T3 pt.<lexical-token>) (lexer))
	       ((T4 pt.<lexical-token>) (lexer)))
	  (let ((getc (lex.lexer-get-func-getc IS)))
	    (getc)
	    (getc))
	  (let* (((T5 pt.<lexical-token>) (lexer))
		 ((T6 pt.<lexical-token>) (lexer))
		 ((T7 pt.<lexical-token>) (lexer)))
	    (list (T1 category) (T2 category) (T3 category)
		  (T4 category)
		  (T5 category) (T6 category) (T7 category)))))
    => '(A A A *lexer-error* A A *eoi*))

  #t)


(parametrise ((check-test-name	'getc))

  (define description "%%
A		(<lexical-token>
                  ((category: 'A)
                   (location: (<source-location> ((line: yyline) (column: yycolumn) (offset: yyoffset))))
                   (value: yytext)
                   (length: (string-length yytext))))

;; test what happens when getting chars without ungetting them
B		(begin
                  (yygetc) (yygetc)
                  (<lexical-token>
                    ((category: 'B)
                     (location: (<source-location> ((line: yyline) (column: yycolumn) (offset: yyoffset))))
                     (value: yytext)
                     (length: (string-length yytext)))))

<<EOF>>		(<lexical-token>
                  ((category: '*eoi*)
                   (location: (<source-location> ((line: yyline) (column: yycolumn) (offset: yyoffset))))
                   (value: (eof-object))
                   (length: 1)))

<<ERROR>>	(<lexical-token>
                  ((category: '*lexer-error*)
                   (location: (<source-location> ((line: yyline) (column: yycolumn) (offset: yyoffset))))
                   (value: yytext)
                   (length: (string-length yytext))))
")

  (define table
    (lex.lex (lex.input-string:		description)
	     (lex.counters:		'all)
	     (lex.library-language:	'(vicare))
	     (lex.library-imports:	'((nausicaa parser-tools lexical-tokens)
					  (nausicaa parser-tools source-locations)))
	     (lex.output-value:		#t)
	     (lex.lexer-format:		'decision-tree)))

  (check	;only A
      (let* ((IS	(lex.make-IS (lex.string: "AAA")
				     (lex.counters: 'all)))
	     (lexer (lex.make-lexer table IS)))
        (let* (((T1 pt.<lexical-token>) (lexer))
	       ((T2 pt.<lexical-token>) (lexer))
	       ((T3 pt.<lexical-token>) (lexer))
	       ((T4 pt.<lexical-token>) (lexer)))
	  (list (T1 category)
		(T2 category)
		(T3 category)
		(T4 category))))
    => '(A A A *eoi*))

  (check	;Get  three  A,  then  one  B  which  discards  the  two
		;subsequent chars, finally get one A.
      (let* ((IS	(lex.make-IS (lex.string: "AAABCDA")
				     (lex.counters: 'all)))
	     (lexer (lex.make-lexer table IS)))
        (let* (((T1 pt.<lexical-token>) (lexer))
	       ((T2 pt.<lexical-token>) (lexer))
	       ((T3 pt.<lexical-token>) (lexer))
	       ((T4 pt.<lexical-token>) (lexer))
	       ((T5 pt.<lexical-token>) (lexer))
	       ((T6 pt.<lexical-token>) (lexer)))
	  (list (T1 category) (T2 category) (T3 category)
		(T4 category) (T5 category) (T6 category))))
    => '(A A A B A *eoi*))

  #t)


;;;; done

(check-report)

;;; end of file
