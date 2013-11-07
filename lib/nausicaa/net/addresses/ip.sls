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


#!vicare
(library (nausicaa net addresses ip)
  (export
    <ip-address>
    <ip-numeric-address>
    <reg-name-address>
    <ipvfuture-address>

    ;; multimethods
    ip-address-representation-string
    ip-address-representation-percent
    ip-address-representation-bignum)
  (import (nausicaa)
    (vicare language-extensions keywords)
    (vicare unsafe operations)
    ;;FIXME  To be  removed at  the  next boot  image rotation.   (Marco
    ;;Maggi; Thu Nov 7, 2013)
    (only (vicare system $strings)
	  $string->ascii
	  $ascii->string))


;;;; generic functions

(define-generic ip-address-representation-string	(ip-address))
(define-generic ip-address-representation-percent	(ip-address))
(define-generic ip-address-representation-bignum	(ip-address))


;;;; base IP address class

(define-class <ip-address>
  (nongenerative nausicaa:net:addresses:<ip-address>)
  (abstract)

  (super-protocol
   (lambda (make-top)
     (case-lambda
      (()
       ((make-top) #f #f))
      (args
       (let-keywords args args #f
	 ((with-argument	string-rep	#f	#:string-rep)
	  (with-argument	percent-rep	#f	#:percent-rep))
	 (assert (null? args))
	 ((make-top) string-rep percent-rep)))
      )))

  (fields
   (mutable memoized-representation-string)
		;False or  string object representation of  the address.
		;It    must   represent    the    address   string    in
		;percent-encoding  as   defined  by  RFC   3986.   Every
		;character in the string can be directly converted to an
		;ASCII encoded character.
		;
		;FIXME This member should have access level "private".

   (mutable memoized-representation-percent)
		;False  or  bytevector   object  representation  of  the
		;address.     It   must    represent    a   string    in
		;percent-encoding as  defined by RFC 3986.   Every octet
		;in   the  bytevector   represents   an  ASCII   encoded
		;character.
		;
		;FIXME This member should have access level "private".

   #| end of fields |# )

  (virtual-fields
   (immutable (string <string>)
	      (lambda ((O <ip-address>))
		(or (O $memoized-representation-string)
		    (receive-and-return ((str <string>))
			(ip-address-representation-string O)
		      (set! (O $memoized-representation-string) str)))))

   (immutable (percent-encoded <percent-encoded-bytevector>)
	      (lambda ((O <ip-address>))
		(or (O $memoized-representation-percent)
		    (receive-and-return ((bv <percent-encoded-bytevector>))
			(ip-address-representation-percent O)
		      (set! (O $memoized-representation-percent) bv)))))

   #| end of virtual-fields |# )

  #| end of class |# )

(define-method (ip-address-representation-percent (O <ip-address>))
  ;;Build and return a bytevector representation of the address from its
  ;;string  representation.  Expects  all  the characters  in the  field
  ;;"string"  to  be directly  convertible  to  the corresponding  ASCII
  ;;encoding.
  ;;
  ($string->ascii (O string)))


;;;; numeric IP address class

(define-class <ip-numeric-address>
  (nongenerative nausicaa:net:addresses:<ip-numeric-address>)
  (parent <ip-address>)
  (abstract)

  (super-protocol
   (lambda (make-ip-address)
     (case-lambda
      (()
       ((make-ip-address) #f))
      (args
       (let-keywords args args #t
	 ((with-argument	bignum-rep	#f	#:bignum-rep))
	 ((apply make-ip-address args) bignum-rep)))
      )))

  (fields
   (mutable memoized-representation-bignum)
		;False  or exact  integer object  representation of  the
		;address.
		;
		;FIXME This member should have access level "private".
   #| end of fields |# )

  (virtual-fields

   (immutable (bignum <exact-integer>)
	      (lambda ((O <ip-numeric-address>))
		(or (O $memoized-representation-bignum)
		    (receive-and-return ((num <exact-integer>))
			(ip-address-representation-bignum O)
		      (set! (O $memoized-representation-bignum) num)))))

   #| end of virtual-fields |# )

  #| end of class |# )


;;;; registered name address class

(define-class <reg-name-address>
  (nongenerative nausicaa:net:addresses:<reg-name-address>)
  (parent <ip-address>)

  (protocol
   (lambda (make-ip-address)
     (lambda ((addr <percent-encoded-bytevector>))
       ((make-ip-address #:percent-rep addr)))))

  #| end of class |# )

(define-method (ip-address-representation-string (O <reg-name-address>))
  ;;Objects   of  type   "<reg-name-address>"  have   a  percent-encoded
  ;;bytevector representation  set by  the constructor;  we use  that to
  ;;build a string representation.
  ;;
  ;;Build and  return a  string representation of  the address  from its
  ;;percent-encoded   representation.    The   returned   object   still
  ;;represents a percent-encoded string.
  ;;
  ($ascii->string (O percent-encoded)))


;;;; IP version "future" address class

(define-label <ipvfuture-version-flag>
  (parent <fixnum>)
  (predicate (lambda (fx)
	       ($fx<= 0 fx 15))))

(define-class <ipvfuture-address>
  (nongenerative nausicaa:net:addresses:<ipvfuture-address>)
  (parent <ip-address>)

  (fields (immutable (version-flag	<ipvfuture-version-flag>))
	  (immutable (literal		<percent-encoded-bytevector>)))

  (protocol
   (lambda (make-ip-address)
     (lambda ((version-flag <ipvfuture-version-flag>) (literal <percent-encoded-bytevector>))
       ((make-ip-address) version-flag literal))))

  #| end of class |# )

(define-method (ip-address-representation-percent (O <ipvfuture-address>))
  ;;Build and return  a bytevector representation of the  address in the
  ;;format specified for URIs by RFC 3986.
  ;;
  (define-inline-constant INT-OBRACKET	(char->integer #\[))
  (define-inline-constant INT-CBRACKET	(char->integer #\]))
  (define-inline-constant INT-DOT	(char->integer #\.))
  (define-inline-constant INT-v		(char->integer #\v))
  (define-inline-constant INT-0		(char->integer #\0))
  (define-inline-constant INT-A		(char->integer #\A))
  (define-inline ($fixnum->ascii-hex n)
    (if ($fx<= 0 n 9)
	($fx+ INT-0 n)
      ($fx+ INT-A ($fx- n 10))))
  (receive (port getter)
      (open-bytevector-output-port)
    (put-u8 port INT-OBRACKET)
    (put-u8 port INT-v)
    (put-u8 port ($fixnum->ascii-hex (O $version-flag)))
    (put-u8 port INT-DOT)
    (put-bytevector port (O $literal))
    (put-u8 port INT-CBRACKET)
    (getter)))

(define-method (ip-address-representation-string (O <ipvfuture-address>))
  ;;Build  and return  a string  representation  of the  address in  the
  ;;format specified  for URIs by  RFC 3986.  The returned  object still
  ;;represents the literal as percent-encoded string.
  ;;
  ($ascii->string (O percent-encoded)))


;;;; done

)

;;; end of file
