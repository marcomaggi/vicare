;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: IPv6 address object type
;;;Date: Wed Jun  9, 2010
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
(library (nausicaa net addresses ipv6)
  (export
    <ipv6-address>		<ipv6-address-prefix>
    )
  (import (nausicaa)
    (vicare unsafe operations))


(define-class <ipv6-address>
  (nongenerative nausicaa:net:ipv6-address:<ipv6-address>)

  (protocol (lambda (make-top)
	      (lambda (addr-ell)
		(apply (make-top) #f #f #f #f #f addr-ell))))

  (fields (mutable cached-bignum)
	  (mutable cached-string)
	  (mutable cached-unspecified?)
	  (mutable cached-loopback?)
	  (mutable cached-global-unicast?)
	  seventh  sixth   fifth  fourth
	  third    second  first  zeroth)

  (virtual-fields bignum string
		  unspecified?
		  loopback?
		  multicast?
		  link-local-unicast?
		  global-unicast?)

  #| end of class |# )

(define (<ipv6-address>-bignum (o <ipv6-address>))
  (or (o cached-bignum)
      (receive-and-return (bn)
	  (+ (o zeroth)
	     (bitwise-arithmetic-shift-left (o first)    16)
	     (bitwise-arithmetic-shift-left (o second)   32)
	     (bitwise-arithmetic-shift-left (o third)    48)
	     (bitwise-arithmetic-shift-left (o fourth)   64)
	     (bitwise-arithmetic-shift-left (o fifth)    80)
	     (bitwise-arithmetic-shift-left (o sixth)    96)
	     (bitwise-arithmetic-shift-left (o seventh) 112))
	(set! (o cached-bignum) bn))))

(define (<ipv6-address>-string (o <ipv6-address>))
  (or (o cached-string)
      (receive-and-return (S)
	  (string-append (number->string (o seventh)  16) ":"
			 (number->string (o sixth)    16) ":"
			 (number->string (o fifth)    16) ":"
			 (number->string (o fourth)   16) ":"
			 (number->string (o third)    16) ":"
			 (number->string (o second)   16) ":"
			 (number->string (o first)    16) ":"
			 (number->string (o zeroth)   16))
	(set! (o cached-string) S))))

;;; --------------------------------------------------------------------

(define (<ipv6-address>-unspecified? (o <ipv6-address>))
  (or (o cached-unspecified?)
      (receive-and-return (B)
	  (and (zero? (o zeroth))
	       (zero? (o first))
	       (zero? (o second))
	       (zero? (o third))
	       (zero? (o fourth))
	       (zero? (o fifth))
	       (zero? (o sixth))
	       (zero? (o seventh)))
	(set! (o cached-unspecified?) B))))

(define (<ipv6-address>-loopback? (o <ipv6-address>))
  (or (o cached-unspecified?)
      (receive-and-return (B)
	  (and (= 1 (o zeroth))
	       (zero? (o first))
	       (zero? (o second))
	       (zero? (o third))
	       (zero? (o fourth))
	       (zero? (o fifth))
	       (zero? (o sixth))
	       (zero? (o seventh)))
	(set! (o cached-unspecified?) B))))

(define (<ipv6-address>-multicast? (o <ipv6-address>))
;;;                        012345678
  (= #xFF00 (bitwise-and #b11111111100000000 (o seventh))))

(define (<ipv6-address>-link-local-unicast? (o <ipv6-address>))
;;;                        0123456789
  (= #xFE80 (bitwise-and #b11111111110000000 (o seventh))))

(define (<ipv6-address>-global-unicast? (o <ipv6-address>))
  (or (o cached-global-unicast?)
      (not (or (o unspecified?)
	       (o loopback?)
	       (o multicast?)
	       (o link-local-unicast?)))))


(define-class <ipv6-address-prefix>
  (nongenerative nausicaa:net:ipv6-address:<ipv6-address-prefix>)
  (parent <ipv6-address>)

  (protocol (lambda (make-address)
	      (lambda (addr-ell number-of-bits)
		((make-address addr-ell) number-of-bits #f))))

  (fields prefix-length
	  (mutable cached-string))

  (virtual-fields (immutable string
			     (lambda ((o <ipv6-address-prefix>))
			       (or (o $cached-string)
				   (receive-and-return (S)
				       (string-append (slot-ref o string <ipv6-address>)
						      "/"
						      (number->string (o prefix-length)))
				     (set! (o $cached-string) S))))))

  #| end of class |# )


;;;; done

)

;;; end of file
