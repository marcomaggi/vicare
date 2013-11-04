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
    <ipv4-address-fixnum>		<vector-of-ipv4-address-fixnums>
    <ipv4-address-prefix-length>)
  (import (nausicaa)
    (nausicaa net addresses ip)
    (vicare unsafe operations)
    ;;FIXME This  import spec  should be removable  after the  next boot
    ;;image rotation.  (Marco Maggi; Mon Nov 4, 2013)
    (except (vicare system $vectors)
	    $vector-empty?))


;;;; auxiliary labels

(define-label <ipv4-address-fixnum>
  (parent <nonnegative-fixnum>)
  (predicate (lambda (N)
	       ($fx<= N 255))))

(define-label <vector-of-ipv4-address-fixnums>
  (parent <vector>)
  (predicate (lambda ((obj <vector>))
	       (and ($fx= 4 (obj $length))
		    ($vector-for-all1 (<ipv4-address-fixnum>) obj)))))

(define-label <ipv4-address-prefix-length>
  (parent <nonnegative-fixnum>)
  (predicate (lambda (N)
	       ($fx<= N 32))))


(define-class <ipv4-address>
  (nongenerative nausicaa:net:ipv4-address:<ipv4-address>)
  (parent <ip-numeric-address>)

  (protocol
   (lambda (make-ip-numeric-address)
     (define (%make-address third second first zeroth)
       ((make-ip-numeric-address) third second first zeroth))
     (case-lambda
      (((third <ipv4-address-fixnum>) (second <ipv4-address-fixnum>)
	(first <ipv4-address-fixnum>) (zeroth <ipv4-address-fixnum>))
       (%make-address third second first zeroth))
      (((addr <vector-of-ipv4-address-fixnums>))
       (%make-address ($vector-ref addr 0)
		      ($vector-ref addr 1)
		      ($vector-ref addr 2)
		      ($vector-ref addr 3)))
      )))

  (fields (immutable (third	<ipv4-address-fixnum>))
	  (immutable (second	<ipv4-address-fixnum>))
	  (immutable (first	<ipv4-address-fixnum>))
	  (immutable (zeroth	<ipv4-address-fixnum>)))

  (virtual-fields

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

(define-method (ip-address-representation-bignum (O <ipv4-address>))
  (+ (O $zeroth)
     (fxarithmetic-shift-left (O $first)   8)
     (fxarithmetic-shift-left (O $second) 16)
     (fxarithmetic-shift-left (O $third)  24)))

(define-method (ip-address-representation-string (O <ipv4-address>))
  (string-append (O $third  $string) "."
		 (O $second $string) "."
		 (O $first  $string) "."
		 (O $zeroth $string)))


(define-class <ipv4-address-prefix>
  (nongenerative nausicaa:net:ipv4-address:<ipv4-address-prefix>)
  (parent <ipv4-address>)

  (protocol (lambda (make-address)
	      (lambda ((addr <vector-of-ipv4-address-fixnums>)
		  (number-of-bits <ipv4-address-prefix-length>))
		((make-address addr) number-of-bits))))

  (fields (immutable (prefix-length <ipv4-address-prefix-length>)))

  #| end of class |# )

(define-method (ip-address-representation-string (O <ipv4-address-prefix>))
  (string-append (slot-ref O string <ipv4-address>)
		 "/"
		 (O $prefix-length $string)))


;;;; done

)

;;; end of file
