;;;
;;;Part of: Vicare Scheme
;;;Contents: variables library
;;;Date: Tue Jul  7, 2009
;;;
;;;Abstract
;;;
;;;	This was originally part of Nausicaa.
;;;
;;;Copyright (c) 2009, 2010, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare language-extensions variables)
  (export
    make-variable			variable?
    variable-ref			variable-set!
    (rename ($variable-value		$variable-ref)
	    ($variable-value-set!	$variable-set!))
    define-variable
    define-variable-alias		$define-variable-alias)
  (import (vicare))


(define-record-type variable
  (sealed #t)
  (opaque #t)
  (nongenerative vicare:variables::variable)
  (fields (mutable value variable-ref variable-set!))
  (protocol (lambda (make-record)
	      (case-lambda
	       (()
		(make-record (void)))
	       ((value)
		(make-record value))))))

;;; --------------------------------------------------------------------

(define-syntax (define-variable stx)
  (syntax-case stx ()
    ((_ (?name . ?args) ?form0 ?form ...)
     #'(define-variable ?name (lambda ?args ?form0 ?form ...)))
    ((_ ?name)
     #'(define-variable ?name (void)))
    ((_ ?name ?value)
     #'($define-variable-alias ?name (make-variable ?value)))
    ))

(define-syntax (define-variable-alias stx)
  (syntax-case stx ()
    ((_ ?alias ?variable-expr)
     #'(begin
	 (define the-var ?variable-expr)
	 (define-syntax ?alias
	   (identifier-syntax
	    (_
	     (variable-ref the-var))
	    ((set! _ ??value)
	     (variable-set! the-var ??value))))))
    ))

(define-syntax ($define-variable-alias stx)
  (syntax-case stx ()
    ((_ ?alias ?variable-expr)
     #'(begin
	 (define the-var ?variable-expr)
	 (define-syntax ?alias
	   (identifier-syntax
	    (_
	     ($variable-value the-var))
	    ((set! _ ??value)
	     ($variable-value-set! the-var ??value))))))
    ))


;;;; done

)

;;; end of file
