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
;;;Copyright (c) 2010-2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
    <ipv6-address>			<ipv6-address-prefix>
    <ipv6-address-fixnum>		<vector-of-ipv6-address-fixnums>
    <ipv6-address-prefix-length>)
  (import (nausicaa)
    (nausicaa net addresses ip)
    (vicare unsafe operations)
    ;;FIXME This  import spec  should be removable  after the  next boot
    ;;image rotation.  (Marco Maggi; Mon Nov 4, 2013)
    (except (vicare system $vectors)
	    $vector-empty?))


;;;; auxiliary labels

(define-label <ipv6-address-fixnum>
  (parent <nonnegative-fixnum>)
  (predicate (lambda (N)
	       ($fx<= N #xFFFF))))

(define-label <vector-of-ipv6-address-fixnums>
  (parent <vector>)
  (predicate (lambda ((obj <vector>))
	       (and ($fx= 8 (obj $length))
		    ($vector-for-all1 (<ipv6-address-fixnum>) obj)))))

(define-label <ipv6-address-prefix-length>
  (parent <nonnegative-fixnum>)
  (predicate (lambda (N)
	       ($fx<= N 128))))


(define-class <ipv6-address>
  (nongenerative nausicaa:net:ipv6-addresses:<ipv6-address>)
  (parent <ip-numeric-address>)

  (protocol
   (lambda (make-ip-address)
     (define (%make-address seventh sixth fifth fourth third second first zeroth)
       ((make-ip-address)
	;; memoized values
	#f #f #f
	;; address components
	seventh sixth fifth fourth third second first zeroth))
     (case-lambda
      (((seventh <ipv6-address-fixnum>) (sixth  <ipv6-address-fixnum>)
	(fifth   <ipv6-address-fixnum>) (fourth <ipv6-address-fixnum>)
	(third   <ipv6-address-fixnum>) (second <ipv6-address-fixnum>)
	(first   <ipv6-address-fixnum>) (zeroth <ipv6-address-fixnum>))
       (%make-address seventh sixth fifth fourth third second first zeroth))
      (((addr <vector-of-ipv6-address-fixnums>))
       (%make-address ($vector-ref addr 0)
		      ($vector-ref addr 1)
		      ($vector-ref addr 2)
		      ($vector-ref addr 3)
		      ($vector-ref addr 4)
		      ($vector-ref addr 5)
		      ($vector-ref addr 6)
		      ($vector-ref addr 7)))
      )))

  (fields (mutable memoized-unspecified?)
	  (mutable memoized-loopback?)
	  (mutable memoized-global-unicast?)
	  (immutable (seventh		<ipv6-address-fixnum>))
	  (immutable (sixth		<ipv6-address-fixnum>))
	  (immutable (fifth		<ipv6-address-fixnum>))
	  (immutable (fourth		<ipv6-address-fixnum>))
	  (immutable (third		<ipv6-address-fixnum>))
	  (immutable (second		<ipv6-address-fixnum>))
	  (immutable (first		<ipv6-address-fixnum>))
	  (immutable (zeroth		<ipv6-address-fixnum>)))

  (virtual-fields

   (immutable (unspecified? <boolean>)
	      (lambda ((o <ipv6-address>))
		(or (o memoized-unspecified?)
		    (receive-and-return (B)
			(and ($fxzero? (o $zeroth))
			     ($fxzero? (o $first))
			     ($fxzero? (o $second))
			     ($fxzero? (o $third))
			     ($fxzero? (o $fourth))
			     ($fxzero? (o $fifth))
			     ($fxzero? (o $sixth))
			     ($fxzero? (o $seventh)))
		      (set! (o $memoized-unspecified?) B)))))

   (immutable (loopback? <boolean>)
	      (lambda ((o <ipv6-address>))
		(or (o $memoized-unspecified?)
		    (receive-and-return (B)
			(and ($fx= 1   (o $zeroth))
			     ($fxzero? (o $first))
			     ($fxzero? (o $second))
			     ($fxzero? (o $third))
			     ($fxzero? (o $fourth))
			     ($fxzero? (o $fifth))
			     ($fxzero? (o $sixth))
			     ($fxzero? (o $seventh)))
		      (set! (o $memoized-unspecified?) B)))))

   (immutable (multicast? <boolean>)
	      (lambda ((o <ipv6-address>))
;;;                                      012345678
		(= #xFF00 (bitwise-and #b11111111100000000 (o $seventh)))))

   (immutable (link-local-unicast? <boolean>)
	      (lambda ((o <ipv6-address>))
;;;                                      0123456789
		(= #xFE80 (bitwise-and #b11111111110000000 (o $seventh)))))

   (immutable (global-unicast? <boolean>)
	      (lambda ((o <ipv6-address>))
		(or (o $memoized-global-unicast?)
		    (not (or (o unspecified?)
			     (o loopback?)
			     (o multicast?)
			     (o link-local-unicast?))))))

   #| end of virtual fields |# )

  #| end of class |# )

(define-method (ip-address-representation-bignum (O <ipv6-address>))
  (+ (O $zeroth)
     (bitwise-arithmetic-shift-left (O $first)    16)
     (bitwise-arithmetic-shift-left (O $second)   32)
     (bitwise-arithmetic-shift-left (O $third)    48)
     (bitwise-arithmetic-shift-left (O $fourth)   64)
     (bitwise-arithmetic-shift-left (O $fifth)    80)
     (bitwise-arithmetic-shift-left (O $sixth)    96)
     (bitwise-arithmetic-shift-left (O $seventh) 112)))

(define-method (ip-address-representation-string (O <ipv6-address>))
  (string-append (O $seventh $string 16) ":"
		 (O $sixth   $string 16) ":"
		 (O $fifth   $string 16) ":"
		 (O $fourth  $string 16) ":"
		 (O $third   $string 16) ":"
		 (O $second  $string 16) ":"
		 (O $first   $string 16) ":"
		 (O $zeroth  $string 16)))


(define-class <ipv6-address-prefix>
  (nongenerative nausicaa:net:ipv6-addresses:<ipv6-address-prefix>)
  (parent <ipv6-address>)

  (protocol (lambda (make-address)
	      (lambda ((addr           <vector-of-ipv6-address-fixnums>)
		  (number-of-bits <ipv6-address-prefix-length>))
		((make-address addr) number-of-bits))))

  (fields (immutable (prefix-length <ipv6-address-prefix-length>)))

  #| end of class |# )

(define-method (ip-address-representation-string (O <ipv6-address-prefix>))
  (string-append (slot-ref O string <ipv6-address>)
		 "/"
		 (O $prefix-length $string)))


;;;; done

)

;;; end of file
