;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: base class for IP addresses
;;;Date: Sun Nov  3, 2013
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
(library (nausicaa net addresses ip-address)
  (export
    <ip-address>
    <reg-name-address>
    <ipvfuture-address>

    ;; multimethods
    ip-address-string
    ip-address-ascii)
  (import (nausicaa))


;;;; generic functions

(define-generic ip-address-string (ip-address))
(define-generic ip-address-ascii  (ip-address))


;;;; base IP address class

(define-class <ip-address>
  (nongenerative nausicaa:net:addresses:<ip-address>)
  (abstract)

  (super-protocol
   (lambda (make-top)
     (lambda ()
       ((make-top) #f #f))))

  (fields (mutable memoized-string)
	  (mutable memoized-ascii))

  (virtual-fields

   (immutable (string <string>)
	      (lambda ((O <ip-address>))
		(or (O $memoized-string)
		    (receive-and-return (str)
			(ip-address-string O)
		      (set! (O $memoized-string) str)))))

   (immutable (ascii <bytevector>)
	      (lambda ((O <ip-address>))
		(or (O $memoized-ascii)
		    (receive-and-return (bv)
			(ip-address-ascii O)
		      (set! (O $memoized-ascii) bv)))))

   #| end of virtual-fields |# )

  #| end of class |# )


;;;; registered name address class

(define-class <reg-name-address>
  (nongenerative nausicaa:net:addresses:<reg-name-address>)
  (parent <ip-address>)

  (protocol (lambda (make-top)
	      (lambda ((addr <bytevector>))
		(receive-and-return ((O <ip-address>))
		    ((make-top))
		  (set! (O $memoized-ascii) addr)))))

  (fields (immutable (bytevector <bytevector>))))


;;;; IP version "future" address class

(define-class <ipvfuture-address>
  (nongenerative nausicaa:net:addresses:<ipvfuture-address>)
  (parent <ip-address>)

  (protocol (lambda (make-top)
	      (lambda ((addr <bytevector>))
		(receive-and-return ((O <ip-address>))
		    ((make-top))
		  (set! (O $memoized-ascii) addr)))))

  #| end of class |# )


;;;; done

)

;;; end of file
