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
    <end-of-input>
    <lexer-error>

    ;; auxiliary syntaxes
    category:
    length:
    location:
    value:
    error-message:)
  (import (nausicaa)
    (prefix (nausicaa parser-tools source-locations) sl.)
    (prefix (vicare language-extensions makers) mk.)
    (vicare arguments validation)
    (vicare system $fx))


(define-auxiliary-syntaxes
  category:
  error-message:
  length:
  location:
  value:)


(module (<lexical-token>)

  (define-class <lexical-token>
    (nongenerative nausicaa:parser-tools:lexical-tokens:<lexical-token>)

    (fields (immutable (category	<symbol>))
	    (immutable (location	sl.<source-location>))
	    (immutable value)
	    (immutable (length		<nonnegative-fixnum>)))

    (protocol
     (lambda (make-top)
       (lambda ((category <symbol>) (location sl.<source-location>) value (length <nonnegative-fixnum>))
	 ((make-top) category location value length))))

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
     (location:	(sl.unspecified-source-location))
     (value:	#f)
     (length:	0)))

  #| end of module |# )


(module (<end-of-input>)

  (define-label <end-of-input>
    (parent <lexical-token>)
    (predicate (lambda ((O <lexical-token>))
		 (eq? '*eoi* (O $category))))
    (protocol (lambda ()
		(lambda (location)
		  (<lexical-token> ((category:	'*eoi*)
				    (location:	location)
				    (value:	(eof-object))
				    (length:	0))))))
    (maker
     (lambda (stx)
       (syntax-case stx ()
	 ((_ (?clause ...))
	  #'(%make-end-of-input ?clause ...)))))

    #| end of label definition |# )

  (mk.define-maker %make-end-of-input
      make-<end-of-input>
    ((location:	(sl.unspecified-source-location))))

  #| end of module |# )


(module (<lexer-error>)

  (define-class <lexer-error>
    (parent <lexical-token>)
    (nongenerative nausicaa:parser-tools:lexical-tokens:<lexer-error>)
    (fields (immutable (message <string>)))

    (maker
     (lambda (stx)
       (syntax-case stx ()
	 ((_ (?expr ...))
	  #'(%make-lexer-error ?expr ...)))))

    (protocol
     (lambda (make-lexical-token)
       (lambda (location (error-message <string>))
	 ((make-lexical-token '*lexer-error* location #f 0) error-message))))

    #| end of class definition |# )

  (mk.define-maker %make-lexer-error
      (make-<lexer-error>)
    ((location:		(sl.unspecified-source-location))
     (error-message:	#f (mk.mandatory))))

  #| end of module |# )


;;;; done

)

;;; end of file
