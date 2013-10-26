;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: IPv4 address object type
;;;Date: Fri Jun 11, 2010
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
(library (nausicaa net addresses ipv4)
  (export
    <ipv4-address>			<ipv4-address-prefix>
    <ipv4-address-fixnum>		<list-of-ipv4-address-fixnums>
    <ipv4-address-prefix-length>)
  (import (nausicaa)
    (vicare unsafe operations))


;;;; auxiliary labels

(define-label <ipv4-address-fixnum>
  (parent <nonnegative-fixnum>)
  (predicate (lambda (N)
	       ($fx<= N 255))))

(define-label <list-of-ipv4-address-fixnums>
  (parent <list>)
  (predicate (lambda ((obj <list>))
	       (and (= 4 (obj length))
		    (for-all (<ipv4-address-fixnum>) obj)))))

(define-label <ipv4-address-prefix-length>
  (parent <nonnegative-fixnum>)
  (predicate (lambda (N)
	       ($fx<= N 32))))


(define-class <ipv4-address>
  (nongenerative nausicaa:net:ipv4-address:<ipv4-address>)

  (protocol
   (lambda (make-top)
     (define (%make-address third second first zeroth)
       ((make-top) #f #f third second first zeroth))
     (case-lambda
      (((third <ipv4-address-fixnum>) (second <ipv4-address-fixnum>)
	(first <ipv4-address-fixnum>) (zeroth <ipv4-address-fixnum>))
       (%make-address third second first zeroth))
      (((addr-ell <list-of-ipv4-address-fixnums>))
       (apply %make-address addr-ell))
      )))

  (fields (mutable memoized-bignum-rep)
	  (mutable memoized-string-rep)
	  (immutable (third	<ipv4-address-fixnum>))
	  (immutable (second	<ipv4-address-fixnum>))
	  (immutable (first	<ipv4-address-fixnum>))
	  (immutable (zeroth	<ipv4-address-fixnum>)))

  (virtual-fields
   (immutable (bignum <exact-integer>)
	      (lambda ((o <ipv4-address>))
		(or (o $memoized-bignum-rep)
		    (receive-and-return (bn)
			(+ (o $zeroth)
			   (fxarithmetic-shift-left (o $first)   8)
			   (fxarithmetic-shift-left (o $second) 16)
			   (fxarithmetic-shift-left (o $third)  24))
		      (set! (o $memoized-bignum-rep) bn)))))

   (immutable (string <string>)
	      (lambda ((o <ipv4-address>))
		(or (o $memoized-string-rep)
		    (receive-and-return (S)
			(string-append (o $third  $string) "."
				       (o $second $string) "."
				       (o $first  $string) "."
				       (o $zeroth $string))
		      (set! (o $memoized-string-rep) S)))))

;;;

   (immutable (private?		<boolean>)
	      (lambda ((O <ipv4-address>))
		(or ($fx= 10 (O $third))
		    (and ($fx= 172 (O $third)) ($fx= #b00010000 ($fxlogand #b11110000 (O $second))))
		    (and ($fx= 192 (O $third)) ($fx= 168 (O $second))))))

   (immutable (loopback?	<boolean>)
	      (lambda ((O <ipv4-address>))
		($fx= 127 (O $third))))

   (immutable (localhost?	<boolean>)
	      (lambda ((O <ipv4-address>))
		(and ($fx= 127 (O $third))
		     ($fx=   0 (O $second))
		     ($fx=   0 (O $first))
		     ($fx=   1 (O $zeroth)))))

   (immutable (link-local?	<boolean>)
	      (lambda ((O <ipv4-address>))
		(and ($fx= 169 (O $third))
		     ($fx= 254 (O $second)))))

   (immutable (reserved?	<boolean>)
	      (lambda ((O <ipv4-address>))
		(or (and ($fx= 192 (O $third))
			 ($fx=   0 (O $second))
			 ($fx=   0 (O $first)))
		    ($fx= 240 ($fxlogand #b11110000 (O $third))))))

   (immutable (test-net-1?	<boolean>)
	      (lambda ((O <ipv4-address>))
		(and ($fx= 192 (O $third))
		     ($fx=   0 (O $second))
		     ($fx=   2 (O $first)))))

   (immutable (six-to-four-relay-anycast?	<boolean>)
	      (lambda ((O <ipv4-address>))
		(and ($fx= 192 (O $third))
		     ($fx=  88 (O $second))
		     ($fx=  99 (O $first)))))

   (immutable (benchmark-tests?	<boolean>)
	      (lambda ((O <ipv4-address>))
		(and ($fx= 198 (O $third))
		     ($fx=  18 ($fxlogand #b11111110 (O $second))))))

   (immutable (test-net-2?	<boolean>)
	      (lambda ((O <ipv4-address>))
		(and ($fx= 198 (O $third))
		     ($fx=  51 (O $second))
		     ($fx= 100 (O $first)))))

   (immutable (test-net-3?	<boolean>)
	      (lambda ((O <ipv4-address>))
		(and ($fx= 203 (O $third))
		     ($fx=   0 (O $second))
		     ($fx= 113 (O $first)))))

   (immutable (multicast?	<boolean>)
	      (lambda ((O <ipv4-address>))
		($fx= 224 ($fxlogand #b11110000 (O $third)))))

   (immutable (limited-broadcast?	<boolean>)
	      (lambda ((O <ipv4-address>))
		(and ($fx= 255 (O $third))
		     ($fx= 255 (O $second))
		     ($fx= 255 (O $first))
		     ($fx= 255 (O $zeroth)))))

   #| end of virtual-fields |# )

  #| end of class |# )


(define-class <ipv4-address-prefix>
  (nongenerative nausicaa:net:ipv4-address:<ipv4-address-prefix>)
  (parent <ipv4-address>)

  (protocol (lambda (make-address)
	      (lambda ((addr-ell <list-of-ipv4-address-fixnums>)
		  (number-of-bits <ipv4-address-prefix-length>))
		((make-address addr-ell) number-of-bits #f))))

  (fields (immutable (prefix-length <ipv4-address-prefix-length>))
	  (mutable   memoized-string-rep))

  (virtual-fields (immutable (string <string>)
			     (lambda ((o <ipv4-address-prefix>))
			       (or (o $memoized-string-rep)
				   (receive-and-return (S)
				       (string-append (slot-ref o string <ipv4-address>)
						      "/"
						      (o $prefix-length $string))
				     (set! (o $memoized-string-rep) S))))))

  #| end of class |# )


;;;; done

)

;;; end of file
