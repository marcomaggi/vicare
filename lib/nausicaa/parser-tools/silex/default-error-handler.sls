;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Scheme
;;;Contents: default error handler for SILex lexers
;;;Date: Tue Jun  1, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa parser-tools silex default-error-handler)
  (export silex-default-error-handler
	  silex-default-eof-handler)
  (import (nausicaa)
    (nausicaa parser-tools source-locations)
    (nausicaa parser-tools lexical-tokens))


(define-syntax (silex-default-error-handler stx)
  (syntax-case stx ()
    ((?key ?yytext)
     #`(%silex-default-error-handler ?yytext
				     #,(datum->syntax #'?key 'yyline)
				     #,(datum->syntax #'?key 'yycolumn)
				     #,(datum->syntax #'?key 'yyoffset)
				     #,(datum->syntax #'?key 'yygetc)
				     #,(datum->syntax #'?key 'yyungetc)
				     ))
    ((?key)
     #`(%silex-default-error-handler #,(datum->syntax #'?key 'yytext)
				     #,(datum->syntax #'?key 'yyline)
				     #,(datum->syntax #'?key 'yycolumn)
				     #,(datum->syntax #'?key 'yyoffset)
				     #,(datum->syntax #'?key 'yygetc)
				     #,(datum->syntax #'?key 'yyungetc)
				     ))
    ))

(define (%silex-default-error-handler yytext yyline yycolumn yyoffset yygetc yyungetc)
  (let ((text (letrec ((the-count 10)
		       (unget (lambda (count)
				(unless (zero? count)
				  (yyungetc)
				  (unget (- count 1)))))
		       (done  (lambda (count chars)
				(unget count)
				(apply string
				       (reverse (if (= count the-count)
						    (cons* #\. #\. #\. chars)
						  chars))))))
		(let loop ((count (string-length yytext))
			   (chars (reverse (string->list yytext))))
		  (if (= the-count count)
		      (done count chars)
		    (let ((ch (yygetc)))
		      (if (eof-object? ch)
			  (done count chars)
			(loop (+ 1 count) (cons ch chars)))))))))
    (<lexical-token> ((category: '*lexer-error*)
		      (location: (<source-location> ((line:   yyline)
						     (column: yycolumn)
						     (offset: yyoffset))))
		      (value:    text)
		      (length:   (string-length text))))))


(define-syntax (silex-default-eof-handler stx)
  (syntax-case stx ()
    ((?key)
     #`(<lexical-token> ((category:	'*eoi*)
			 (location:	(<source-location> ((line:   #,(datum->syntax #'?key 'yyline))
							    (column: #,(datum->syntax #'?key 'yycolumn))
							    (offset: #,(datum->syntax #'?key 'yyoffset)))))
			 (value:	(eof-object))
			 (length:	0))))
    ))


;;;; done

)

;;; end of file
