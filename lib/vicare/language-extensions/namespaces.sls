;;;
;;;Part of: Vicare/Scheme
;;;Contents: namespaces definition
;;;Date: Wed Jun  8, 2011
;;;
;;;Abstract
;;;
;;;	This library was originally part of Nausicaa.
;;;
;;;Copyright (C) 2011, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare language-extensions namespaces)
  (export define-namespace using export)
  (import (vicare))


(define-syntax :using)

(define-syntax (define-namespace stx)
  (define (syntax->list stx)
    (syntax-case stx ()
      ((?car . ?cdr)
       (cons #'?car (syntax->list #'?cdr)))
      (() '())))
  (define (all-identifiers? stx)
    (for-all identifier? (syntax->list stx)))
  (syntax-case stx (export)
    ((_ ?name (export ?export ...) ?body ...)
     (and (identifier? #'?name)
	  (all-identifiers? #'(?export ...)))
     #'(module (?name)
	 (define-syntax (?name stx)
	   (syntax-case stx (:using)
	     ((?kwd :using)
	      (with-syntax (((EXPORT (... ...)) (datum->syntax #'?kwd '(?export ...))))
		#'(begin
		    (define-syntax EXPORT
		      (lambda (stx)
			(syntax-case stx ()
			  ((_ ?arg ((... ...) (... ...)))
			   #'(?kwd EXPORT ?arg ((... ...) (... ...)))))))
		    (... ...))))
	     ((_ ?key ?arg (... ...))
	      (identifier? #'?key)
	      (case (syntax->datum #'?key)
		((?export)	#'(?export ?arg (... ...)))
		...
		(else
		 (syntax-violation '?name
		   "unknown namespace member or invalid syntax" stx #'?key))))
	     ))
	 ?body ...))))

(define-syntax (using stx)
  (define (syntax->list stx)
    (syntax-case stx ()
      ((?car . ?cdr)
       (cons #'?car (syntax->list #'?cdr)))
      (() '())))
  (define (all-identifiers? stx)
    (for-all identifier? (syntax->list stx)))
  (syntax-case stx ()
    ((_ ?name0 ?name ...)
     (and (identifier? #'?name0)
	  (all-identifiers? #'(?name ...)))
     #'(begin
	 (?name0 :using)
	 (?name  :using)
	 ...))))


;;;; done

)

;;; end of file
;; Local Variables:
;; coding: utf-8
;; End:
