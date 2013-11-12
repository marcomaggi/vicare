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

    <ipv4-address>			<ipv4-address-prefix>
    <ipv4-address-fixnum>		<vector-of-ipv4-address-fixnums>
    <ipv4-address-prefix-length>

    <ipv6-address>			<ipv6-address-prefix>
    <ipv6-address-fixnum>		<vector-of-ipv6-address-fixnums>
    <ipv6-address-prefix-length>

    ;; utility functions
    make-host-object

    ;; multimethods
    ip-address->string
    ip-address->bytevector
    ip-address->bignum)
  (import (nausicaa)
    (vicare language-extensions keywords)
    (vicare language-extensions ascii-chars)
    (vicare unsafe operations)
    ;;FIXME  To be  removed at  the  next boot  image rotation.   (Marco
    ;;Maggi; Thu Nov 7, 2013)
    (only (vicare system $strings)
	  $string->ascii
	  $ascii->string)
    ;;FIXME This  import spec  should be removable  after the  next boot
    ;;image rotation.  (Marco Maggi; Mon Nov 4, 2013)
    (except (vicare system $vectors)
	    $vector-empty?))


;;;; generic functions

(define-generic ip-address->string	(ip-address))
(define-generic ip-address->bytevector	(ip-address))
(define-generic ip-address->bignum	(ip-address))


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
	  (with-argument	percent-rep	#f	#:bytevector-rep))
	 (assert (null? args))
	 ((make-top) string-rep percent-rep)))
      )))

  (fields
   (mutable memoized-representation-string)
		;False or string object representation of the address as
		;defined by RFC 3986.  Every character in the string can
		;be directly  converted to  an ASCII  encoded character,
		;some   sequences   of   characters  may   represent   a
		;percent-encoded character.
		;
		;FIXME This member should have access level "private".

   (mutable memoized-representation-bytevector)
		;False  or  bytevector   object  representation  of  the
		;address as  defined by  RFC 3986.   Every octet  in the
		;bytevector represents an  ASCII encoded character, some
		;sequences  of octets  may  represent a  percent-encoded
		;character.
		;
		;FIXME This member should have access level "private".

   #| end of fields |# )

  (virtual-fields
   (immutable (string <ascii-string>)
	      (lambda ((O <ip-address>))
		(or (O $memoized-representation-string)
		    (receive-and-return ((str <string>))
			(ip-address->string O)
		      (set! (O $memoized-representation-string) str)))))

   (immutable (bytevector <ascii-bytevector>)
	      (lambda ((O <ip-address>))
		(or (O $memoized-representation-bytevector)
		    (receive-and-return ((bv <ascii-bytevector>))
			(ip-address->bytevector O)
		      (set! (O $memoized-representation-bytevector) bv)))))

   #| end of virtual-fields |# )

  #| end of class |# )

(define-method (ip-address->bytevector (O <ip-address>))
  ;;Build and return a bytevector representation of the address from its
  ;;string  representation.   Expect all  the  characters  in the  field
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
			(ip-address->bignum O)
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
       ((make-ip-address #:bytevector-rep addr)))))

  #| end of class |# )

(define-method (ip-address->string (O <reg-name-address>))
  ;;Objects   of  type   "<reg-name-address>"  have   a  percent-encoded
  ;;bytevector representation  set by  the constructor;  we use  that to
  ;;build a string representation.
  ;;
  ;;Build and  return a  string representation of  the address  from its
  ;;percent-encoded   representation.    The   returned   object   still
  ;;represents a percent-encoded string.
  ;;
  ($ascii->string (O bytevector)))


;;;; IP version "future" address class

(define-label <ipvfuture-version-flag>
  (parent <fixnum>)
  (predicate (lambda (fx)
	       ($fx<= 0 fx 15))))

(define-class <ipvfuture-address>
  (nongenerative nausicaa:net:addresses:<ipvfuture-address>)
  (parent <ip-address>)

  (fields (immutable (version-flag	<ipvfuture-version-flag>))
	  (immutable (literal		<ascii-bytevector>)))

  (protocol
   (lambda (make-ip-address)
     (lambda ((version-flag <ipvfuture-version-flag>) (literal <percent-encoded-bytevector>))
       ((make-ip-address) version-flag literal))))

  #| end of class |# )

(define-method (ip-address->bytevector (O <ipvfuture-address>))
  ;;Build and return  a bytevector representation of the  address in the
  ;;format specified for URIs by RFC 3986.
  ;;
  (define-inline-constant INT-OBRACKET	(char->integer #\[))
  (define-inline-constant INT-CBRACKET	(char->integer #\]))
  (define-inline-constant INT-DOT	(char->integer #\.))
  (define-inline-constant INT-v		(char->integer #\v))
  (define-inline-constant INT-0		(char->integer #\0))
  (define-inline-constant INT-A		(char->integer #\A))
  (receive (port getter)
      (open-bytevector-output-port)
    (put-u8 port INT-OBRACKET)
    (put-u8 port INT-v)
    (put-u8 port ($fixnum->ascii-hex (O $version-flag)))
    (put-u8 port INT-DOT)
    (put-bytevector port (O $literal))
    (put-u8 port INT-CBRACKET)
    (getter)))

(define-method (ip-address->string (O <ipvfuture-address>))
  ;;Build  and return  a string  representation  of the  address in  the
  ;;format specified for URIs by RFC 3986.
  ;;
  ($ascii->string (O bytevector)))


;;;; IPv4 auxiliary labels

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


;;;; IPv4 address class

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

(define-method (ip-address->bignum (O <ipv4-address>))
  (+ (O $zeroth)
     (fxarithmetic-shift-left (O $first)   8)
     (fxarithmetic-shift-left (O $second) 16)
     (fxarithmetic-shift-left (O $third)  24)))

(define-method (ip-address->string (O <ipv4-address>))
  ;;Build  and return  a string  representation  of the  address in  the
  ;;format specified for URIs by RFC 3986.
  ;;
  (string-append (O $third  $string) "."
		 (O $second $string) "."
		 (O $first  $string) "."
		 (O $zeroth $string)))


;;;; IPv4 address prefix class

(define-class <ipv4-address-prefix>
  (nongenerative nausicaa:net:ipv4-address:<ipv4-address-prefix>)

  (protocol
   (lambda (make-top)
     (case-lambda
      (((third <ipv4-address-fixnum>) (second <ipv4-address-fixnum>)
	(first <ipv4-address-fixnum>) (zeroth <ipv4-address-fixnum>))
       ((make-top) third second first zeroth))
      (((number-of-bits <ipv4-address-prefix-length>)
	(addr <vector-of-ipv4-address-fixnums>))
       ((make-top) number-of-bits
	($vector-ref addr 0)
	($vector-ref addr 1)
	($vector-ref addr 2)
	($vector-ref addr 3)))
      )))

  (fields (immutable (prefix-length	<ipv4-address-prefix-length>))
	  (immutable (third		<ipv4-address-fixnum>))
	  (immutable (second		<ipv4-address-fixnum>))
	  (immutable (first		<ipv4-address-fixnum>))
	  (immutable (zeroth		<ipv4-address-fixnum>)))

  (virtual-fields
   (immutable (string <string>)
	      (lambda ((O <ipv4-address-prefix>))
		(string-append (O $third   $string) "."
			       (O $second  $string) "."
			       (O $first   $string) "."
			       (O $zeroth  $string) "/"
			       (O $prefix-length $string))))

   (immutable (bytevector <ascii-bytevector>)
	      (lambda ((O <ipv4-address-prefix>))
		($string->ascii (O string))))

   #| end of virtual-fields |# )

  #| end of class |# )


;;;; IPv6 auxiliary labels

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


;;;; IPv6 address class

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
			(and (o $zeroth  $zero?)
			     (o $first   $zero?)
			     (o $second  $zero?)
			     (o $third   $zero?)
			     (o $fourth  $zero?)
			     (o $fifth   $zero?)
			     (o $sixth   $zero?)
			     (o $seventh $zero?))
		      (set! (o $memoized-unspecified?) B)))))

   (immutable (loopback? <boolean>)
	      (lambda ((o <ipv6-address>))
		(or (o $memoized-unspecified?)
		    (receive-and-return (B)
			(and ($fx= 1   (o $zeroth))
			     (o $first   $zero?)
			     (o $second  $zero?)
			     (o $third   $zero?)
			     (o $fourth  $zero?)
			     (o $fifth   $zero?)
			     (o $sixth   $zero?)
			     (o $seventh $zero?))
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

(define-method (ip-address->bignum (O <ipv6-address>))
  (+ (O $zeroth)
     (bitwise-arithmetic-shift-left (O $first)    16)
     (bitwise-arithmetic-shift-left (O $second)   32)
     (bitwise-arithmetic-shift-left (O $third)    48)
     (bitwise-arithmetic-shift-left (O $fourth)   64)
     (bitwise-arithmetic-shift-left (O $fifth)    80)
     (bitwise-arithmetic-shift-left (O $sixth)    96)
     (bitwise-arithmetic-shift-left (O $seventh) 112)))

(define-method (ip-address->string (O <ipv6-address>))
  ;;Build  and return  a string  representation  of the  address in  the
  ;;format specified for URIs by RFC 3986.
  ;;
  (string-append "["
		 (O $seventh $string 16) ":"
		 (O $sixth   $string 16) ":"
		 (O $fifth   $string 16) ":"
		 (O $fourth  $string 16) ":"
		 (O $third   $string 16) ":"
		 (O $second  $string 16) ":"
		 (O $first   $string 16) ":"
		 (O $zeroth  $string 16) "]"))

(define-method (ip-address->bytevector (O <ipv6-address>))
  ;;Build and return  a bytevector representation of the  address in the
  ;;format specified for URIs by RFC 3986.
  ;;
  ($string->ascii (O string)))


;;;; IPv6 address prefix class

(define-class <ipv6-address-prefix>
  (nongenerative nausicaa:net:ipv6-addresses:<ipv6-address-prefix>)

  (protocol (lambda (make-top)
	      (case-lambda
	       (((number-of-bits <ipv6-address-prefix-length>)
		 (numbers        <vector-of-ipv6-address-fixnums>))
		((make-top) number-of-bits
		 ($vector-ref numbers 0)
		 ($vector-ref numbers 1)
		 ($vector-ref numbers 2)
		 ($vector-ref numbers 3)
		 ($vector-ref numbers 4)
		 ($vector-ref numbers 5)
		 ($vector-ref numbers 6)
		 ($vector-ref numbers 7)))
	       (((number-of-bits <ipv6-address-prefix-length>)
		 (seventh <ipv6-address-fixnum>) (sixth  <ipv6-address-fixnum>)
		 (fifth   <ipv6-address-fixnum>) (fourth <ipv6-address-fixnum>)
		 (third   <ipv6-address-fixnum>) (second <ipv6-address-fixnum>)
		 (first   <ipv6-address-fixnum>) (zeroth <ipv6-address-fixnum>))
		((make-top) number-of-bits seventh sixth fifth fourth third second first zeroth))
	       )))

  (fields (immutable (prefix-length	<ipv6-address-prefix-length>))
	  (immutable (seventh		<ipv6-address-fixnum>))
	  (immutable (sixth		<ipv6-address-fixnum>))
	  (immutable (fifth		<ipv6-address-fixnum>))
	  (immutable (fourth		<ipv6-address-fixnum>))
	  (immutable (third		<ipv6-address-fixnum>))
	  (immutable (second		<ipv6-address-fixnum>))
	  (immutable (first		<ipv6-address-fixnum>))
	  (immutable (zeroth		<ipv6-address-fixnum>)))

  (virtual-fields
   (immutable (string <string>)
	      (lambda ((O <ipv6-address-prefix>))
		(string-append (O $seventh $string 16) ":"
			       (O $sixth   $string 16) ":"
			       (O $fifth   $string 16) ":"
			       (O $fourth  $string 16) ":"
			       (O $third   $string 16) ":"
			       (O $second  $string 16) ":"
			       (O $first   $string 16) ":"
			       (O $zeroth  $string 16) "/"
			       (O $prefix-length $string))))

   (immutable (bytevector <ascii-bytevector>)
	      (lambda ((O <ipv6-address-prefix>))
		($string->ascii (O string))))

   #| end of virtual-fields |# )

  #| end of class |# )


;;;; host types

(define (make-host-object (host.type <symbol>) (host.ascii <bytevector>) host.data)
  ;;Build  and return  a new  instance  of specialised  object of  class
  ;;"<ip-address>"  representing  the  host  component of  a  URI.   The
  ;;possible  classes of  the returned  object are:  <reg-name-address>,
  ;;<ipv4-address>, <ipv6-address>, <ipvfuture-address>.
  ;;
  ;;HOST.TYPE   must  be   a  symbol   among:  reg-name,   ipv4-address,
  ;;ipv6-address, ipvfuture.
  ;;
  ;;HOST.ASCII   must   be   a   bytevector   representing   the   ASCII
  ;;representation of the host component.
  ;;
  ;;HOST.DATA must be auxiliary data representing the host component:
  ;;
  ;;* For "reg-name": HOST.DATA is undefined.
  ;;
  ;;* For "ipv4-address": HOST.DATA must be a vector of 4 exact integers
  ;;  representing the address components.
  ;;
  ;;* For "ipv6-address": HOST.DATA must be a vector of 8 exact integers
  ;;  representing the address components.
  ;;
  ;;* For "ipvfuture":  HOST.DATA must be an  exact integer representing
  ;;  the version number of the IP address literal representation.
  ;;
  ;;The arguments  are modeled after  the 3 return values  of PARSE-HOST
  ;;from the library "(nausicaa parser-tools uri)".
  ;;
  (case host.type
    ((reg-name)
     ;;The  constructor will  validate the  argument as  percent-encoded
     ;;ASCII bytevector.
     (<reg-name-address> (host.ascii)))
    ((ipv4-address)
     ;;The  constructor will  validate the  argument as  vector of  IPv4
     ;;fixnums.
     (<ipv4-address> (host.data)))
    ((ipv6-address)
     ;;The  constructor will  validate the  argument as  vector of  IPv6
     ;;fixnums.
     (<ipv6-address> (host.data)))
    ((ipvfuture)
     ;;The constructor will validate the  argument as version tag number
     ;;and percent-encoded ASCII bytevector.
     (<ipvfuture-address> (host.data host.ascii)))
    (else
     (procedure-argument-violation __who__
       "invalid URI host type" host.type))))


;;;; done

)

;;; end of file
