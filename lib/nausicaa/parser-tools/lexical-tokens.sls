;;;
;;;Part of: Vicare Scheme
;;;Contents: lexical token record
;;;Date: Tue Jul 21, 2009
;;;
;;;Abstract
;;;
;;;	The <LEXICAL-TOKEN>  record type describes tokens  produced by a
;;;	lexer and consumed  by a parser.  It is meant to  be used by all
;;;	the parser libraries distributed with Vicare.
;;;
;;;Copyright (c) 2009-2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2005-2008 Dominique Boucher
;;;
;;;Original  code  by  Dominique  Boucher.   Port  to  R6RS  and  Vicare
;;;integration by Marco Maggi.
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
(library (nausicaa parser-tools lexical-tokens)
  (export
    <lexical-token>
    <end-of-input-token>
    <lexer-error>

    ;; auxiliary syntaxes
    category:
    length:
    location:
    value:
    error-message:)
  (import (nausicaa)
    (nausicaa parser-tools source-locations)
    (prefix (vicare language-extensions makers) mk.)
    (vicare arguments validation)
    (vicare system $fx))


(define-auxiliary-syntaxes
  category:
  error-message:
  length:
  location:
  value:)


(define-class <lexical-token>
  (nongenerative vicare:parser-tools:<lexical-token>)

  (fields (immutable (category <symbol>))
	  (immutable (location <source-location>))
	  (immutable value)
	  (immutable (length <fixnum>)))

  (protocol
   (lambda (make-top)
     (lambda (category location value length)
       (define who 'make-<lexical-token>)
       (with-arguments-validation (who)
	   ((symbol	category)
	    (fixnum	length))
	 ((make-top) category location value length)))))

  (maker
   (lambda (stx)
     (syntax-case stx ()
       ((_ (?clause ...))
	#'(%make-lexical-token ?clause ...)))))

  (method (special? (O <lexical-token>))
    (or (eq? '*eoi*         (O $category))
	(eq? '*lexer-error* (O $category))))

  (method (end-of-input? (O <lexical-token>))
    (eq? '*eoi* (O $category)))

  (method (lexer-error? (O <lexical-token>))
    (eq? '*lexer-error* (O $category)))

  #| end of class definition |# )

(mk.define-maker %make-lexical-token
    make-<lexical-token>
  ((category:	#f (mk.mandatory))
   (location:	#f)
   (value:	#f)
   (length:	0)))


(define-label <end-of-input-token>
  (parent <lexical-token>)
  (predicate (lambda ((O <lexical-token>))
	       (eq? '*eoi* (O $category))))
  (protocol (lambda ()
	      (lambda (location value length)
		(make-<lexical-token> '*eoi* location value length))))
  (maker
   (lambda (stx)
     (syntax-case stx ()
       ((_ (?clause ...))
	#'(%make-end-of-input-token ?clause ...)))))

  #| end of label definition |# )

(mk.define-maker %make-end-of-input-token
    make-<end-of-input-token>
  ((location:	#f)
   (value:	#f)
   (length:	0)))


(define-class <lexer-error>
  (nongenerative vicare:parser-tools:lexical-tokens:<lexer-error>)
  (parent <lexical-token>)
  (fields (immutable (message <string>)))

  (maker
   (lambda (stx)
     (syntax-case stx ()
       ((_ (?expr ...))
	#'(%make-lexer-error ?expr ...)))))

  (protocol
   (lambda (make-lexical-token)
     (lambda (location value length error-message)
       (define who 'make-<lexer-error>)
       (with-arguments-validation (who)
	   ((string	error-message))
	 ((make-lexical-token '*lexer-error* location value length) error-message)))))

  #| end of class definition |# )

(mk.define-maker %make-lexer-error
    (make-<lexer-error>)
  ((location:		#f)
   (value:		#f)
   (length:		0)
   (error-message:	#f (mk.mandatory))))


;;;; done

)

;;; end of file
