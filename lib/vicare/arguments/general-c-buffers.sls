;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: syntaxes to handle generalised C strings and buffers
;;;Date: Tue Oct  2, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare arguments general-c-buffers)
  (export
    with-general-c-strings
    with-general-c-strings/false
    with-general-c-pathnames
    with-general-c-pathnames/false
    string-to-bytevector)
  (import (vicare)
    (vicare syntactic-extensions))


;;;; helpers

(define-auxiliary-syntaxes string-to-bytevector)


;;;; general C strings

(define-syntax* (with-general-c-strings stx)
  (syntax-case stx (string-to-bytevector)
    ((_ ((?str^ ?str) ...) ?string->bytevector ?body0 . ?body)
     (identifier? #'?string->bytevector)
     #'(with-general-c-strings ((?str^ ?str) ...)
	   (string-to-bytevector ?string->bytevector)
	 ?body0 . ?body))
    ((_ ((?str^ ?str) ...)
	(string-to-bytevector ?string->bytevector)
	?body0 . ?body)
     (identifier? #'?string->bytevector)
     #'(let ((?str^ (let ((str ?str))
		      (cond ((string? str)
			     (?string->bytevector str))
			    ((or (bytevector?   str)
				 (pointer?      str)
				 (memory-block? str))
			     str)
			    (else
			     (assertion-violation #f "invalid general C string" str)))))
	     ...)
	 ?body0 . ?body))))

(define-syntax* (with-general-c-strings/false stx)
  (syntax-case stx (string-to-bytevector)
    ((?key ((?str^ ?str) ...) ?string->bytevector ?body0 . ?body)
     (identifier? #'?string->bytevector)
     #'(with-general-c-strings/false ((?str^ ?str) ...)
	   (string-to-bytevector ?string->bytevector)
	 ?body0 . ?body))
    ((_ ((?str^ ?str) ...)
	(string-to-bytevector ?string->bytevector)
	?body0 . ?body)
     (identifier? #'?string->bytevector)
     #'(let ((?str^ (let ((str ?str))
		      (cond ((string? str)
			     (?string->bytevector str))
			    ((or (bytevector?   str)
				 (pointer?      str)
				 (memory-block? str))
			     str)
			    ((not str)
			     str)
			    (else
			     (assertion-violation #f "invalid general C string" str)))))
	     ...)
	 ?body0 . ?body))))


;;;; general C pathnames

(define-syntax* (with-general-c-pathnames stx)
  (syntax-case stx ()
    ((_ ((?str^ ?str) ...) ?body0 . ?body)
     #'(let ((?str^ (let ((str ?str))
		      (cond ((string? str)
			     ((string->pathname-func) str))
			    ((or (bytevector?   str)
				 (pointer?      str)
				 (memory-block? str))
			     str)
			    (else
			     (assertion-violation #f "invalid general C pathname" str)))))
	     ...)
	 ?body0 . ?body))))

(define-syntax* (with-general-c-pathnames/false stx)
  (syntax-case stx ()
    ((_ ((?str^ ?str) ...) ?body0 . ?body)
     #'(let ((?str^ (let ((str ?str))
		      (cond ((string? str)
			     ((string->pathname-func) str))
			    ((or (bytevector?   str)
				 (pointer?      str)
				 (memory-block? str))
			     str)
			    ((not str)
			     str)
			    (else
			     (assertion-violation #f "invalid general C pathname" str)))))
	     ...)
	 ?body0 . ?body))))


;;;; done

)

;;; end of file
;;Local Variables:
;;eval: (put 'with-general-c-strings 'scheme-indent-function 2)
;;eval: (put 'with-general-c-strings/false 'scheme-indent-function 2)
;;eval: (put 'with-general-c-pathnames 'scheme-indent-function 1)
;;eval: (put 'with-general-c-pathnames/false 'scheme-indent-function 1)
;;End:
