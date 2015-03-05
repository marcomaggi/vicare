;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: definition of common auxiliary syntaxes
;;;Date: Sat Sep  7, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare containers auxiliary-syntaxes)
  (export
    view start past
    define-instantiable-body)
  (import (vicare))


(define-auxiliary-syntaxes view start past)

(define-syntax (define-instantiable-body stx)
  (syntax-case stx ()
    ((_ ?name . ?body)
     #'(define-syntax ?name
	 (lambda (stx)
	   (define (symbol-subst ctx from to body)
	     (define (%subst body from to)
	       (cond ((or (null?       body)
			  (boolean?    body)
			  (char?       body)
			  (string?     body)
			  (number?     body)
			  (bytevector? body))
		      body)
		     ((symbol? body)
		      (if (eq? from body)
			  to
			body))
		     ((pair? body)
		      (cons (%subst (car body) from to)
			    (%subst (cdr body) from to)))
		     ((vector? body)
		      (list->vector (map (lambda (item)
					   (%subst item from to))
				      (vector->list body))))
		     (else
		      (error 'symbol-subst "unknown value while substituting symbols" body))))
	     (let ((from (syntax->datum from))
		   (to   (syntax->datum to))
		   (body (syntax->datum body)))
	       (datum->syntax ctx (fold-left %subst body from to))))
	   (syntax-case stx ()
	     ((?ctx ((?from ?to) (... ...)))
	      (symbol-subst #'?ctx
			    #'(?from (... ...))
			    #'(?to   (... ...))
			    #'(begin . ?body)))))))
    ))


;;;; done

)

;;; end of file
