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
(library (nausicaa net addresses ip)
  (export
    <ip-address>
    <ip-numeric-address>
    <reg-name-address>
    <ipvfuture-address>

    ;; multimethods
    ip-address-representation-string
    ip-address-representation-ascii
    ip-address-representation-bignum)
  (import (nausicaa))


;;;; generic functions

(define-generic ip-address-representation-string	(ip-address))
(define-generic ip-address-representation-ascii		(ip-address))
(define-generic ip-address-representation-bignum	(ip-address))


;;;; base IP address class

(define-class <ip-address>
  (nongenerative nausicaa:net:addresses:<ip-address>)
  (abstract)

  (super-protocol
   (lambda (make-top)
     (lambda ()
       ((make-top) #f #f))))

  (fields (mutable memoized-representation-string)
	  (mutable memoized-representation-ascii))

  (virtual-fields

   (immutable (string <string>)
	      (lambda ((O <ip-address>))
		(or (O $memoized-representation-string)
		    (receive-and-return ((str <string>))
			(ip-address-representation-string O)
		      (set! (O $memoized-representation-string) str)))))

   (immutable (ascii <ascii-bytevector>)
	      (lambda ((O <ip-address>))
		(or (O $memoized-representation-ascii)
		    (receive-and-return ((bv <ascii-bytevector>))
			(ip-address-representation-ascii O)
		      (set! (O $memoized-representation-ascii) bv)))))

   #| end of virtual-fields |# )

  #| end of class |# )

(define-method (ip-address-representation-ascii (O <ip-address>))
  (string->ascii (O string)))


;;;; numeric IP address class

(define-class <ip-numeric-address>
  (nongenerative nausicaa:net:addresses:<ip-numeric-address>)
  (parent <ip-address>)
  (abstract)

  (super-protocol
   (lambda (make-ip-address)
     (lambda ()
       ((make-ip-address) #f))))

  (fields (mutable memoized-representation-bignum))

  (virtual-fields

   (immutable (bignum <exact-integer>)
	      (lambda ((O <ip-numeric-address>))
		(or (O $memoized-representation-bignum)
		    (receive-and-return (str)
			(ip-address-representation-bignum O)
		      (set! (O $memoized-representation-bignum) str)))))

   #| end of virtual-fields |# )

  #| end of class |# )


;;;; registered name address class

(define-class <reg-name-address>
  (nongenerative nausicaa:net:addresses:<reg-name-address>)
  (parent <ip-address>)

  (protocol (lambda (make-top)
	      (lambda ((addr <ascii-bytevector>))
		(receive-and-return ((O <ip-address>))
		    ((make-top))
		  (set! (O $memoized-representation-ascii) addr)))))

  #| end of class |# )

(define-method (ip-address-representation-string (O <reg-name-address>))
  (uri-encoding->string (O ascii)))


;;;; IP version "future" address class

(define-class <ipvfuture-address>
  (nongenerative nausicaa:net:addresses:<ipvfuture-address>)
  (parent <ip-address>)

  (protocol (lambda (make-top)
	      (lambda ((addr <ascii-bytevector>))
		(receive-and-return ((O <ip-address>))
		    ((make-top))
		  (set! (O $memoized-representation-ascii) addr)))))

  #| end of class |# )


;;;; done

)

;;; end of file
