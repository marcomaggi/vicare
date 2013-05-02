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
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
    general-c-buffer-len
    with-general-c-strings
    with-general-c-strings/false
    with-general-c-pathnames
    with-general-c-pathnames/false
    string-to-bytevector)
  (import (vicare)
    (vicare language-extensions syntaxes)
    (vicare arguments validation)
    (prefix (only (vicare unsafe operations)
		  bytevector-length)
	    $))


;;;; helpers

(define-auxiliary-syntaxes string-to-bytevector)


;;;; helper functions

(define (general-c-buffer-len buf buf.len)
  (define who 'general-c-buffer-len)
  (with-arguments-validation (who)
      ((general-c-buffer	buf)
       (size_t/false		buf.len))
    (cond ((bytevector? buf)
	   ($bytevector-length buf))
	  ((memory-block? buf)
	   (memory-block-size buf))
	  ((pointer? buf)
	   buf.len)
	  (else
	   (assertion-violation who "internal error" buf buf.len)))))


;;;; general C strings

(define-syntax (with-general-c-strings stx)
  (syntax-case stx (string-to-bytevector)
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
	 ?body0 . ?body))
    ((_ ((?str^ ?str) ...) ?body0 . ?body)
     #'(with-general-c-strings ((?str^ ?str) ...)
	   (string-to-bytevector string->ascii)
	 ?body0 . ?body))
    ))

(define-syntax (with-general-c-strings/false stx)
  (syntax-case stx (string-to-bytevector)
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
	 ?body0 . ?body))
    ((?key ((?str^ ?str) ...) ?body0 . ?body)
     #'(with-general-c-strings/false ((?str^ ?str) ...)
	   (string-to-bytevector string->ascii)
	 ?body0 . ?body))
    ))


;;;; general C pathnames

(define-syntax (with-general-c-pathnames stx)
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

(define-syntax (with-general-c-pathnames/false stx)
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
