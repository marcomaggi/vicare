;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: URI handling
;;;Date: Wed Jun  2, 2010
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
(library (nausicaa uri)
  (export
    <uri> <relative-ref>

    ;; URI components
    <scheme> <userinfo> <host> <port-number>
    <query> <fragment>
    <path> <path-empty> <path-abempty> <path-absolute> <path-rootless>
    <path-noscheme>

    ;; auxiliary classes and labels
    <segment>	<list-of-segments>

    ;; utility functions
    make-path-object

    ;; auxiliary syntaxes
    scheme
    userinfo		host		port-number
    path		query		fragment

;;; --------------------------------------------------------------------
;;; reexported from (nausicaa net addresses ip)

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
    (prefix (vicare language-extensions makers) mk.)
    (vicare unsafe operations)
    (vicare language-extensions ascii-chars)
    (nausicaa uri ip))


;;;; helpers

(define-auxiliary-syntaxes
  scheme
  userinfo
  host
  port-number
  path
  query
  fragment)

(define-syntax $bytevector-for-all
  (syntax-rules ()
    ((_ (?pred0 ?pred ...) ?start ?bv)
     (let loop ((bv ?bv)
		(i  ?start))
       (or ($fx= i ($bytevector-length bv))
	   (and (let ((chi ($bytevector-u8-ref bv i)))
		  (or (?pred0 chi) (?pred  chi) ...))
		(loop bv ($fxadd1 i))))))
    ))

(define-inline (%return-if-specified obj . irritants)
  (if (specified? obj)
      obj
    (apply assertion-violation #f
	   "attempt to access unspecified field"
	   irritants)))


;;;; auxiliary labels and classes: scheme

(define-label <scheme>
  (parent <nonempty-bytevector>)

  (protocol
   (lambda ()
     ;;Apply the predicate, through the tagged argument, and return.
     (lambda ((bv <scheme>))
       bv)))

  (predicate
   (lambda (bv)
     (and ($ascii-alphabetic? ($bytevector-u8-ref bv 0))
	  ($bytevector-for-all ($ascii-alpha-digit?
				$ascii-chi-plus?
				$ascii-chi-minus?
				$ascii-chi-dot?)
			       1 bv))))

  (virtual-fields
   (immutable (bytevector <ascii-bytevector>)
	      (lambda ((O <scheme>))
		;;58 = #\:
		(bytevector-append O '#vu8(58))))

   (immutable (string <ascii-string>)
	      (lambda ((O <scheme>))
		($ascii->string (O bytevector))))

   #| end of virtual-fields |# )

  (method (put-bytevector (O <scheme>) (port <binary-output-port>))
    (put-bytevector port O)
    ;;58 = #\:
    (put-u8         port 58))

  #| end of label |# )


;;;; auxiliary labels and classes: userinfo

(define-label <userinfo>
  (parent <bytevector>)

  (protocol
   (lambda ()
     ;;Apply the predicate, through the tagged argument, and return.
     (lambda ((bv <userinfo>))
       bv)))

  (predicate
   (lambda (bv)
     (let loop ((bv bv)
		(i  0))
       (or ($fx= i ($bytevector-length bv))
	   (let ((chi ($bytevector-u8-ref bv i)))
	     (and (or ($ascii-uri-unreserved? chi)
		      ($ascii-uri-sub-delim?  chi)
		      ($ascii-chi-colon?      chi)
		      ($ascii-uri-pct-encoded? chi bv i))
		  (loop bv ($fxadd1 i))))))))

  (virtual-fields
   (immutable (bytevector <ascii-bytevector>)
	      (lambda ((O <userinfo>))
		;;64 = #\@
		(bytevector-append O #vu8(64)) ))

   (immutable (string <ascii-string>)
	      (lambda ((O <userinfo>))
		($ascii->string (O bytevector))))

   #| end of virtual-fields |# )

  (method (put-bytevector (O <userinfo>) port)
    (put-bytevector port O)
    ;;64 = #\@
    (put-u8         port 64))

  #| end of class |# )


;;;; auxiliary labels and classes: host

(define-label <host>
  (parent <ip-address>)

  (method (put-bytevector (O <ip-address>) port)
    (put-bytevector port (O bytevector)))

  #| end of label |# )


;;;; auxiliary labels and classes: port number

(module (<port-number>)

  (define-label <port-number>
    (parent <nonnegative-fixnum>)
    (predicate
     (lambda (fx)
       ($fx<= fx 65535)))

    (protocol
     (lambda ()
       ;;Validate the value through the tagged argument and return it.
       (lambda ((fx <port-number>))
	 fx)))

    (virtual-fields
     (immutable (bytevector <ascii-bytevector>)
		(lambda ((O <port-number>))
		  ;;58 = #\:
		  (bytevector-append '#vu8(58) ($fixnum->bytevector O))))

     (immutable (string <ascii-string>)
		(lambda ((O <port-number>))
		  ($ascii->string (O bytevector))))

     #| end of virtual-fields |# )

    (method (put-bytevector (O <port-number>) port)
      ;;58 = #\:
      (put-u8 port 58)
      (put-bytevector port (string->ascii (number->string O))))

    #| end of label |# )

  (define-inline ($fixnum->bytevector fx)
    (string->ascii (fixnum->string fx)))

  (define-inline (fixnum->string fx)
    (number->string fx))

  #| end of module |# )


;;;; auxiliary labels and classes: query

(define-label <query>
  (parent <bytevector>)

  (protocol
   (lambda ()
     ;;Apply the predicate, through the tagged argument, and return.
     (lambda ((bv <query>))
       bv)))

  (predicate
   (lambda (bv)
     (let loop ((bv bv)
		(i  0))
       (or ($fx= i ($bytevector-length bv))
	   (and ($ascii-uri-pchar? ($bytevector-u8-ref bv i) bv i)
		(loop bv ($fxadd1 i)))))))

  (virtual-fields
   (immutable (bytevector <ascii-bytevector>)
	      (lambda ((O <query>))
		;;63 = ?
		(bytevector-append '#vu8(63) O)))

   (immutable (string <ascii-string>)
	      (lambda ((O <query>))
		($ascii->string (O bytevector))))

   #| end of virtual-fields |# )

  (method (put-bytevector (O <query>) port)
    ;;63 = ?
    (put-u8 port 63)
    (put-bytevector port O))

  #| end of label |# )


;;;; auxiliary labels and classes: fragment

(define-label <fragment>
  (parent <bytevector>)

  (protocol
   (lambda ()
     ;;Apply the predicate, through the tagged argument, and return.
     (lambda ((bv <fragment>))
       bv)))

  (predicate
   (lambda (bv)
     (let loop ((bv bv)
		(i  0))
       (or ($fx= i ($bytevector-length bv))
	   (and ($ascii-uri-pchar? ($bytevector-u8-ref bv i) bv i)
		(loop bv ($fxadd1 i)))))))

  (virtual-fields
   (immutable (bytevector <ascii-bytevector>)
	      (lambda ((O <fragment>))
		;;35 = #
		(bytevector-append '#vu8(35) O)))

   (immutable (string <ascii-string>)
	      (lambda ((O <fragment>))
		($ascii->string (O bytevector))))

   #| end of virtual-fields |# )

  (method (put-bytevector (O <fragment>) port)
    ;;35 = #
    (put-u8 port 35)
    (put-bytevector port O))

  #| end of label |# )


;;;; path types: auxiliary label <segment>

(define-label <segment>
  (parent <bytevector>)

  (protocol
   ;;Apply the predicate, through the tagged argument, and return.
   (lambda ()
     (lambda ((bv <fragment>)) bv)))

  (predicate
   (lambda (bv)
     (and ($bytevector-not-empty? bv)
	  (let loop ((bv bv)
		     (i  0))
	    (or ($fx= i ($bytevector-length bv))
		(and ($ascii-uri-pchar? ($bytevector-u8-ref bv i) bv i)
		     (loop bv ($fxadd1 i))))))))

  (virtual-fields

   (immutable (bytevector <segment>)
	      (lambda (O) O))

   (immutable (string <ascii-string>)
	      $ascii->string)

   #| end of virtual-fields |# )

  (method-syntax put-bytevector
    (syntax-rules ()
      ((_ ?bv ?port)
       (put-bytevector ?port ?bv))))

  #| end of label |# )


;;;; path types: auxiliary label <list-of-segments>

(module (<list-of-segments>)

  (define-label <list-of-segments>
    (parent <list>)

    (protocol
     (lambda ()
       (lambda (ell)
	 (%normalise-list-of-segments ell))))

    (predicate
     (lambda (O)
       (%normalised-list-of-segments? O)))

    (virtual-fields

     (immutable (bytevector <ascii-bytevector>)
		(lambda ((O <list-of-segments>))
		  (receive (port getter)
		      (open-bytevector-output-port)
		    (O put-bytevector port)
		    (getter))))

     (immutable (string <ascii-string>)
		(lambda ((O <list-of-segments>))
		  ($ascii->string (O bytevector))))

     #| end of virtual-fields |# )

    (method (put-bytevector (O <list-of-segments>) port)
      ;;We  know  that in  a  normalised  list  of segments:  a  segment
      ;;representing the current directory can come only as last one; so
      ;;if  we find  such  a segment  we stop  putting  after the  slash
      ;;character.   This  way  when reconstructing  the  original  URIs
      ;;"a/b/" and  "a/b/." we get  "a/b/".  As  a special case:  if the
      ;;path  is composed  of  the current  directory  segment only,  we
      ;;output nothing.
      ;;
      (when (pair? O)
	(if (null? (O $cdr))
	    (unless ($current-directory? (O $car))
	      (put-bytevector port (O $car)))
	  (begin
	    (put-bytevector port (O $car))
	    (let loop (((S <spine>) (O $cdr)))
	      (when (pair? S)
		;;47 = (char->integer #\/)
		(put-u8 port 47)
		(unless ($current-directory? (S $car))
		  (put-bytevector port (S $car))
		  (loop (S $cdr)))))))))

    #| end of label |# )

  (define (%normalise-list-of-segments list-of-segments)
    ;;Given a proper list of bytevectors representing URI path segments:
    ;;validate and  normalise it as  described in section  5.2.4 "Remove
    ;;Dot  Segments" of  RFC  3986.  If  successful  return a,  possibly
    ;;empty,  proper  list  of   bytevectors  representing  the  result;
    ;;otherwise  raise an  exception with  compound condition  object of
    ;;types:   "&procedure-argument-violation",    "&who",   "&message",
    ;;"&irritants".
    ;;
    ;;We expect  the input list to  be "short".  We would  like to visit
    ;;recursively the  argument, but to process  the "uplevel directory"
    ;;segments we need  access to the previous item in  the list as well
    ;;as the current one;  so we process it in a loop and  at the end we
    ;;reverse the accumulated result.
    ;;
    ;;How do  we handle  segments representing the  "current directory"?
    ;;When the original  URI string contains the  sequence of characters
    ;;"a/./b", we expect it to be parsed as the list of segments:
    ;;
    ;;   (#ve(ascii "a") #ve(ascii ".") #ve(ascii "b"))
    ;;
    ;;when the original  URI string contains the  sequence of characters
    ;;"a//b", we expect it to be parsed as the list of segments:
    ;;
    ;;   (#ve(ascii "a") #vu8() #ve(ascii "b"))
    ;;
    ;;so  we interpret  an empty  bytevector  as alias  for the  segment
    ;;containing a standalone  dot.  We discard such  segment, unless it
    ;;is the last one; this is  because when the original URI terminates
    ;;with "a/b/" (slash  as last character), we want the  result of the
    ;;path normalisation to be:
    ;;
    ;;   (#ve(ascii "a") #ve(ascii "b") #ve(ascii "."))
    ;;
    ;;so  that  reconstructing the  URI  we  get  "a/b/." which  can  be
    ;;normalised to  "a/b/"; by discarding  the trailing dot  segment we
    ;;would get "a/b" as reconstructed URI.
    ;;
    ;;How do  we handle  segments representing the  "uplevel directory"?
    ;;If a segment  exists on the output stack: discard  both it and the
    ;;uplevel  directory segment;  otherwise  just  discard the  uplevel
    ;;directory segment.
    ;;
    (let next-segment ((input-stack  list-of-segments)
		       (output-stack '()))
      (cond ((pair? input-stack)
	     (let ((head ($car input-stack))
		   (tail ($cdr input-stack)))
	       (if (bytevector? head)
		   (cond (($current-directory? head)
			  ;;Discard a  segment representing  the current
			  ;;directory; unless  it is the last,  in which
			  ;;case we normalise  it to "." and  push it on
			  ;;the output stack.
			  (if (pair? tail)
			      (next-segment tail output-stack)
			    (reverse (cons '#vu8(46) output-stack))))
			 (($uplevel-directory? head)
			  ;;Remove  the  previously pushed  segment,  if
			  ;;any.
			  (next-segment tail (if (null? output-stack)
						 output-stack
					       ($cdr output-stack))))
			 (((<segment>) head)
			  ;;Just  push  on  the output  stack  a  normal
			  ;;segment.
			  (next-segment tail (cons head output-stack)))
			 (else
			  (procedure-argument-violation __who__
			    "expected URI segment bytevector as item in list argument"
			    list-of-segments head)))
		 (procedure-argument-violation __who__
		   "expected bytevector as item in list argument"
		   list-of-segments head))))
	    ((null? input-stack)
	     (reverse output-stack))
	    (else
	     (procedure-argument-violation __who__
	       "expected proper list as argument" list-of-segments)))))

  (define (%normalised-list-of-segments? list-of-segments)
    ;;Return  #t  if  the  argument  is a  proper  list  of  bytevectors
    ;;representing URI path  segments as defined by  RFC 3986; otherwise
    ;;return #f.
    ;;
    ;;Each   segment  must   be  a   non-empty  bytevector;   a  segment
    ;;representing the current  directory "." is accepted only  if it is
    ;;the  last one;  a segment  representing the  uplevel directory  is
    ;;rejected.
    ;;
    (cond ((pair? list-of-segments)
	   (let ((head ($car list-of-segments))
		 (tail ($cdr list-of-segments)))
	     (and (bytevector? head)
		  (cond (($current-directory? head)
			 ;;Accept  a  segment representing  the  current
			 ;;directory only if it is the last one.
			 (if (pair? tail)
			     #f
			   (%normalised-list-of-segments? tail)))
			(($uplevel-directory? head)
			 ;;Reject  a  segment representing  the  uplevel
			 ;;directory.
			 #f)
			(((<segment>) head)
			 (%normalised-list-of-segments? tail))
			(else #f)))))
	  ((null? list-of-segments)
	   #t)
	  (else #f)))

  (define ($current-directory? bv)
    (or ($bytevector-empty? bv)
	(and ($fx= 1 ($bytevector-length bv))
	     ;;46 = #\.
	     ($fx= 46 ($bytevector-u8-ref bv 0)))))

  (define ($uplevel-directory? bv)
    (and ($fx= 2 ($bytevector-length bv))
	 ;;46 = #\.
	 ($fx= 46 ($bytevector-u8-ref bv 0))
	 ($fx= 46 ($bytevector-u8-ref bv 1))))

  #| end of module |# )


;;;; path types

(define-generic uri-path-put-bytevector	(path port))
(define-generic uri-path-symbol		(path))

;;; --------------------------------------------------------------------

(define-class <path>
  (nongenerative nausicaa:net:addresses:uri:<path>)
  (abstract)

  (fields (immutable (path <list-of-segments>))
	  (mutable   memoized-bytevector)
	  (mutable   memoized-string))

  (super-protocol
   (lambda (make-top)
     (lambda (path)
       ((make-top) (<list-of-segments> (path)) #f #f))))

  (virtual-fields

   (immutable (bytevector <ascii-bytevector>)
	      (lambda ((O <path>))
		(or (O $memoized-bytevector)
		    (receive-and-return (bv)
			(receive (port getter)
			    (open-bytevector-output-port)
			  (uri-path-put-bytevector O port)
			  (getter))
		      (set! (O $memoized-bytevector) bv)))))

   (immutable (string <ascii-string>)
	      (lambda ((O <path>))
		(or (O $memoized-string)
		    (receive-and-return (str)
			($ascii->string (O bytevector))
		      (set! (O $memoized-string) str)))))

   (immutable (type <symbol>)
	      uri-path-symbol)

   #| end of virtual-fields |# )

  (methods (put-bytevector	uri-path-put-bytevector))

  #| end of class |# )

(define-method (uri-path-put-bytevector (O <path>) (port <binary-output-port>))
  (O path put-bytevector port))

;;; --------------------------------------------------------------------

(define-class <path-empty>
  ;;There  is only  one instance  of this  class: the  constrctor always
  ;;returns the same object.
  ;;
  (nongenerative nausicaa:net:addresses:uri:<path-empty>)
  (parent <path>)
  (protocol
   (lambda (make-path)
     (let ((singleton-instance #f))
       (lambda ()
	 (or singleton-instance
	     (receive-and-return (rv)
		 ((make-path '()))
	       (set! singleton-instance rv)))))))
  #| end of class |# )

(define-method (uri-path-symbol (O <path-empty>))
  'path-empty)

;;; --------------------------------------------------------------------

(define-class <path-abempty>
  (nongenerative nausicaa:net:addresses:uri:<path-abempty>)
  (parent <path>)
  (protocol (lambda (make-path)
	      (lambda ((path <list>))
		((make-path path)))))
  #| end of class |# )

(define-method (uri-path-put-bytevector (O <path-abempty>) (port <binary-output-port>))
  ;;47 = (char->integer #\/)
  (put-u8 port 47)
  (call-next-method))

(define-method (uri-path-symbol (O <path-abempty>))
  'path-abempty)

;;; --------------------------------------------------------------------

(define-class <path-absolute>
  (nongenerative nausicaa:net:addresses:uri:<path-absolute>)
  (parent <path>)
  (protocol (lambda (make-path)
	      (lambda ((path <list>))
		((make-path path)))))
  #| end of class |# )

(define-method (uri-path-put-bytevector (O <path-absolute>) (port <binary-output-port>))
  ;;47 = (char->integer #\/)
  (put-u8 port 47)
  (call-next-method))

(define-method (uri-path-symbol (O <path-absolute>))
  'path-absolute)

;;; --------------------------------------------------------------------

(define-class <path-rootless>
  (nongenerative nausicaa:net:addresses:uri:<path-rootless>)
  (parent <path>)
  (protocol (lambda (make-path)
	      (lambda ((path <nonempty-list>))
		((make-path path)))))
  #| end of class |# )

(define-method (uri-path-symbol (O <path-rootless>))
  'path-rootless)

;;; --------------------------------------------------------------------

(define-class <path-noscheme>
  (nongenerative nausicaa:net:addresses:uri:<path-noscheme>)
  (parent <path>)
  (protocol (lambda (make-path)
	      (lambda ((path <nonempty-list>))
		((make-path path)))))
  #| end of class |# )

(define-method (uri-path-symbol (O <path-noscheme>))
  'path-noscheme)

;;; --------------------------------------------------------------------

(define (make-path-object (path-type <symbol>) path)
  (case path-type
    ((path-abempty)
     (<path-abempty>	(path)))
    ((path-absolute)
     (<path-absolute>	(path)))
    ((path-rootless)
     (<path-rootless>	(path)))
    ((path-noscheme)
     (<path-noscheme>	(path)))
    ((path-empty)
     (<path-empty>	()))
    (else
     (procedure-argument-violation __who__
       "invalid URI path type" path-type))))


;;;; auxiliary labels

(define-label <userinfo/unspecified>
  (predicate (lambda (O)
	       (or ((<userinfo>) O)
		   (unspecified? O)))))

(define-label <host/unspecified>
  (predicate (lambda (O)
	       (or ((<host>) O)
		   (unspecified? O)))))

(define-label <port-number/unspecified>
  (predicate (lambda (O)
	       (or ((<port-number>) O)
		   (unspecified?    O)))))

(define-label <path/unspecified>
  (predicate (lambda (O)
	       (or ((<path>) O)
		   (unspecified? O)))))

(define-label <query/unspecified>
  (predicate (lambda (O)
	       (or ((<query>) O)
		   (unspecified? O)))))

(define-label <fragment/unspecified>
  (predicate (lambda (O)
	       (or ((<fragment>) O)
		   (unspecified? O)))))


(define-class <uri>
  (nongenerative nausicaa:net:addresses:uri:<uri>)

  (maker (lambda (stx)
	   (syntax-case stx ()
	     ((_ (?clause ...))
	      #'(%make-uri ?clause ...)))))

  (protocol
   (lambda (make-top)
     (lambda ((scheme	<scheme>)
	 (userinfo	<userinfo/unspecified>)
	 (host		<host/unspecified>)
	 (port		<port-number/unspecified>)
	 (path		<path/unspecified>)
	 (query		<query/unspecified>)
	 (fragment	<fragment/unspecified>))
       (define who 'make-<uri>)
       (when (and (specified? userinfo)
		  (unspecified? host))
	 (procedure-argument-violation who
	   "invalid specification of \"userinfo\" component when the \"host\" component is unspecified"
	   userinfo))
       (when (and (specified? port)
		  (unspecified? host))
	 (procedure-argument-violation who
	   "invalid specification of \"port\" component when the \"host\" component is unspecified"
	   port))
       ((make-top)
	scheme userinfo host port
	(if (unspecified? path)
	    (<path-empty> ())
	  path)
	query fragment
	#f #;memoized-bytevector #f #;memoized-string ))))

  (fields
   (immutable (scheme <scheme>))
		;An  instance of  "<scheme>".  A  "scheme" component  is
		;mandatory for URI objects.

   (immutable userinfo-object)
		;Unspecified or an instance of "<userinfo>".

   (immutable host-object)
		;Unspecified or an instance of "<host>".

   (immutable port-object)
		;Unspecified or an instance of "<port-number>".

   (immutable (path <path>))
		;An  instance  of  "<path>".  The  "path"  component  is
		;mandatory for URI objects.   When no path is specified:
		;it defaults to an instance of "<path-empty>".

   (immutable query-object)
		;Unspecified or an instance of "<query>".

   (immutable fragment-object)
		;Unspecified or an instance of "<fragment>".

   (mutable memoized-bytevector)
		;Memoized URI bytevector representation.

   (mutable memoized-string)
		;Memoized URI string representation.

   #| end of fields |# )

  (virtual-fields
   (immutable (has-userinfo?	<boolean>)	(lambda ((O <uri>))
						  (specified? (O $userinfo-object))))
   (immutable (has-host?	<boolean>)	(lambda ((O <uri>))
						  (specified? (O $host-object))))
   (immutable (has-port?	<boolean>)	(lambda ((O <uri>))
						  (specified? (O $port-object))))
   (immutable (has-authority?	<boolean>)	(lambda ((O <uri>))
						  (O has-host?)))
   (immutable (has-query?	<boolean>)	(lambda ((O <uri>))
						  (specified? (O $query-object))))
   (immutable (has-fragment?	<boolean>)	(lambda ((O <uri>))
						  (specified? (O $fragment-object))))
   #| end of virtual-fields |# )

  (virtual-fields
   (immutable (userinfo	<userinfo>)	(lambda ((O <uri>))
					  (%return-if-specified (O $userinfo-object) O)))
   (immutable (host	<host>)		(lambda ((O <uri>))
					  (%return-if-specified (O $host-object)     O)))
   (immutable (port <port-number>)	(lambda ((O <uri>))
					  (%return-if-specified (O $port-object)     O)))
   (immutable (query	<query>)	(lambda ((O <uri>))
					  (%return-if-specified (O $query-object)    O)))
   (immutable (fragment	<fragment>)	(lambda ((O <uri>))
					  (%return-if-specified (O $fragment-object) O)))
   #| end of virtual-fields |# )

  (virtual-fields
   (immutable (bytevector <ascii-bytevector>)
	      (lambda ((O <uri>))
		(or (O $memoized-bytevector)
		    (receive-and-return (bv)
			(receive (port getter)
			    (open-bytevector-output-port)
			  (O put-bytevector port)
			  (getter))
		      (set! (O $memoized-bytevector) bv)))))

   (immutable (string <ascii-string>)
	      (lambda ((O <uri>))
		(or (O $memoized-string)
		    (receive-and-return (str)
			($ascii->string (O bytevector))
		      (set! (O $memoized-string) str)))))

   (immutable (authority <ascii-bytevector>)
	      (lambda ((O <uri>))
		(if (O has-authority?)
		    (receive (port getter)
			(open-bytevector-output-port)
		      (O put-authority-bytevector port)
		      (getter))
		  (assertion-violation #f
		    "attempt to access unspecified field \"authority\""
		    O))))

   #| end of virtual-fields |# )

  (method (put-bytevector (O <uri>) (port <binary-output-port>))
    ;;We  want  to  recompose  the  URI  as  described  in  section  5.3
    ;;"Component Recomposition" of RFC 3986.
    (define who '<uri>-bytevector)
    (O $scheme put-bytevector port)
    (when (O has-authority?)
      ;;47 = #\/
      (put-bytevector port '#vu8(47 47))
      (O put-authority-bytevector port))
    (O $path put-bytevector port)
    (when (O has-query?)
      (O query put-bytevector port))
    (when (O has-fragment?)
      (O fragment put-bytevector port)))

  (method (put-authority-bytevector (O <uri>) (port <binary-output-port>))
    (when (O has-authority?)
      (when (O has-userinfo?)
	(O userinfo put-bytevector port))
      ;;The authority is defined only when the host is defined.
      (O host put-bytevector port)
      (when (O has-port?)
	(O port put-bytevector port))))

  #| end of class |# )

(mk.define-maker %make-uri
    make-<uri>
  ((scheme		unspecified	(mk.mandatory))
   (userinfo		unspecified)
   (host		unspecified)
   (port-number		unspecified)
   (path		(<path-empty> ()))
   (query		unspecified)
   (fragment		unspecified)))


(define-class <relative-ref>
  (nongenerative nausicaa:net:addresses:uri:<relative-ref>)

  (maker (lambda (stx)
	   (syntax-case stx ()
	     ((_ (?clause ...))
	      #'(%make-relative-ref ?clause ...)))))

  (protocol
   (lambda (make-top)
     (lambda ((userinfo	<userinfo/unspecified>)
	 (host		<host/unspecified>)
	 (port		<port-number/unspecified>)
	 (path		<path>)
	 (query		<query/unspecified>)
	 (fragment	<fragment/unspecified>))
       (define who 'make-<relative-ref>)
       (when (and (specified? userinfo)
		  (unspecified? host))
	 (procedure-argument-violation who
	   "invalid specification of \"userinfo\" component when the \"host\" component is unspecified"
	   userinfo))
       (when (and (specified? port)
		  (unspecified? host))
	 (procedure-argument-violation who
	   "invalid specification of \"port\" component when the \"host\" component is unspecified"
	   port))
       ((make-top)
	userinfo host port
	path query fragment
	#f #;memoized-bytevector #f #;memoized-string ))))

  (fields
   (immutable userinfo-object)
		;Unspecified or an instance of "<userinfo>".

   (immutable host-object)
		;Unspecified or an instance of "<host>".

   (immutable port-object)
		;Unspecified or an instance of "<port-number>".

   (immutable (path <path>))
		;An  instance  of  "<path>".  The  "path"  component  is
		;mandatory for URI objects.   When no path is specified:
		;it defaults to an instance of "<path-empty>".

   (immutable query-object)
		;Unspecified or an instance of "<query>".

   (immutable fragment-object)
		;Unspecified or an instance of "<fragment>".

   (mutable memoized-bytevector)
		;Memoized URI bytevector representation.

   (mutable memoized-string)
		;Memoized URI string representation.

   #| end of fields |# )

  (virtual-fields
   (immutable (has-userinfo?	<boolean>)	(lambda ((O <relative-ref>))
						  (specified? (O $userinfo-object))))
   (immutable (has-host?	<boolean>)	(lambda ((O <relative-ref>))
						  (specified? (O $host-object))))
   (immutable (has-port?	<boolean>)	(lambda ((O <relative-ref>))
						  (specified? (O $port-object))))
   (immutable (has-authority?	<boolean>)	(lambda ((O <relative-ref>))
						  (O has-host?)))
   (immutable (has-query?	<boolean>)	(lambda ((O <relative-ref>))
						  (specified? (O $query-object))))
   (immutable (has-fragment?	<boolean>)	(lambda ((O <relative-ref>))
						  (specified? (O $fragment-object))))
   #| end of virtual-fields |# )

  (virtual-fields
   (immutable (userinfo	<userinfo>)	(lambda ((O <relative-ref>))
					  (%return-if-specified (O $userinfo-object) O)))
   (immutable (host	<host>)		(lambda ((O <relative-ref>))
					  (%return-if-specified (O $host-object)     O)))
   (immutable (port <port-number>)	(lambda ((O <relative-ref>))
					  (%return-if-specified (O $port-object)     O)))
   (immutable (query	<query>)	(lambda ((O <relative-ref>))
					  (%return-if-specified (O $query-object)    O)))
   (immutable (fragment	<fragment>)	(lambda ((O <relative-ref>))
					  (%return-if-specified (O $fragment-object) O)))
   #| end of virtual-fields |# )

  (virtual-fields
   (immutable (bytevector <ascii-bytevector>)
	      (lambda ((O <relative-ref>))
		(or (O $memoized-bytevector)
		    (receive-and-return (bv)
			(receive (port getter)
			    (open-bytevector-output-port)
			  (O put-bytevector port)
			  (getter))
		      (set! (O $memoized-bytevector) bv)))))

   (immutable (string <ascii-string>)
	      (lambda ((O <relative-ref>))
		(or (O $memoized-string)
		    (receive-and-return (str)
			($ascii->string (O bytevector))
		      (set! (O $memoized-string) str)))))

   (immutable (authority <ascii-bytevector>)
	      (lambda ((O <relative-ref>))
		(if (O has-authority?)
		    (receive (port getter)
			(open-bytevector-output-port)
		      (O put-authority-bytevector port)
		      (getter))
		  (assertion-violation #f
		    "attempt to access unspecified field \"authority\""
		    O))))

   #| end of virtual-fields |# )

  (method (put-bytevector (O <relative-ref>) (port <binary-output-port>))
    ;;We  want  to  recompose  the  URI  as  described  in  section  5.3
    ;;"Component Recomposition" of RFC 3986.
    (define who '<relative-ref>-bytevector)
    (O put-authority-bytevector port)
    (O $path put-bytevector port)
    (when (O has-query?)
      (O query put-bytevector port))
    (when (O has-fragment?)
      (O fragment put-bytevector port)))

  (method (put-authority-bytevector (O <relative-ref>) (port <binary-output-port>))
    (when (O has-authority?)
      ;;47 = #\/
      (put-bytevector port '#vu8(47 47))
      (when (O has-userinfo?)
	(O userinfo put-bytevector port))
      ;;The authority is defined only when the host is defined.
      (O host put-bytevector port)
      (when (O has-port?)
	(O port put-bytevector port))))

  #| end of class |# )

(mk.define-maker %make-relative-ref
    make-<relative-ref>
  ((userinfo		unspecified	(mk.mandatory))
   (host		unspecified)
   (port-number		unspecified)
   (path		unspecified)
   (query		unspecified)
   (fragment		unspecified)))


;;;; done

)

;;; end of file
