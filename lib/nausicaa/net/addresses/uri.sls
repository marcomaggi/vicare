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
(library (nausicaa net addresses uri)
  (export
    <uri> <relative-ref>

    ;; URI components
    <scheme> <userinfo> <host> <port-number>
    <query> <fragment>
    <path> <path-empty> <path-abempty> <path-absolute> <path-rootless>

    ;; auxiliary classes and labels
    <segment-bytevector>
    <list-of-segments>

    ;; multimethods
    uri-path->bytevector
    uri-path-put-bytevector

    ;; utility functions
    make-path-object

    ;; auxiliary syntaxes
    scheme		authority		userinfo
    host		port-number
    path		query			fragment)
  (import (nausicaa)
    (prefix (nausicaa net addresses ip) ip.)
    (prefix (vicare language-extensions makers) mk.)
    (vicare unsafe operations)
    (vicare language-extensions ascii-chars)
    ;;FIXME  To be  removed at  the  next boot  image rotation.   (Marco
    ;;Maggi; Mon Nov 4, 2013)
    (only (vicare system $bytevectors)
	  $uri-encoded-bytevector?)
    (only (vicare system $strings)
	  $ascii->string)
    ;;FIXME  To be  removed at  the  next boot  image rotation.   (Marco
    ;;Maggi; Fri Nov 8, 2013)
    (only (vicare system $lists)
	  $for-all1))


;;;; helpers

(define-auxiliary-syntaxes
  scheme
  authority
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
   (immutable (specified? <boolean>)
	      (lambda (bv)
		($bytevector-not-empty? bv)))

   (immutable (bytevector <ascii-bytevector>)
	      (lambda ((O <userinfo>))
		(if (O specified?)
		    (bytevector-append O #vu8(64)) ;64 = #\@
		  '#vu8())))

   (immutable (string <ascii-string>)
	      (lambda ((O <userinfo>))
		($ascii->string (O bytevector))))

   #| end of virtual-fields |# )

  (method (put-bytevector (O <userinfo>) port)
    (when (O specified?)
      (put-bytevector port O)
      ;;64 = #\@
      (put-u8         port 64)))

  #| end of class |# )


;;;; auxiliary labels and classes: host

(define-label <host>
  (parent ip.<ip-address>)

  (method (put-bytevector (O ip.<ip-address>) port)
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
     (immutable (specified? <boolean>)
		(lambda (fx)
		  (not ($fxzero? fx))))

     (immutable (bytevector <ascii-bytevector>)
		(lambda ((O <port-number>))
		  (if (O specified?)
		      ;;58 = #\:
		      (bytevector-append '#vu8(58) ($fixnum->bytevector O))
		    '#vu8())))

     (immutable (string <ascii-string>)
		(lambda ((O <port-number>))
		  ($ascii->string (O bytevector))))

     #| end of virtual-fields |# )

    (method (put-bytevector (O <port-number>) port)
      (when (O specified?)
	;;58 = #\:
	(put-u8 port 58)
	(put-bytevector port (string->ascii (number->string O)))))

    #| end of label |# )

  (define-inline ($fixnum->bytevector fx)
    (string->ascii (fixnum->string fx)))

  (define-inline (fixnum->string fx)
    (number->string fx))

  #| end of module |# )


;;;; auxiliary labels and classes: query

(define-label <query>
  (parent <percent-encoded-bytevector>)
  (virtual-fields

   (immutable (bytevector <ascii-bytevector>)
	      (lambda (O)
		;;63 = ?
		(bytevector-append '#vu8(63) O)))

   #| end of virtual-fields |# )

  (method (put-bytevector O port)
    ;;63 = ?
    (put-u8 port 63)
    (put-bytevector port O))

  #| end of label |# )


;;;; auxiliary labels and classes: fragment

(define-label <fragment>
  (parent <percent-encoded-bytevector>)
  (virtual-fields

   (immutable (bytevector <ascii-bytevector>)
	      (lambda (O)
		;;35 = #
		(bytevector-append '#vu8(35) O)))

   #| end of virtual-fields |# )

  (method (put-bytevector O port)
    ;;35 = #
    (put-u8 port 35)
    (put-bytevector port O))

  #| end of label |# )


;;;; path types

(define-generic uri-path->bytevector (uri))
(define-generic uri-path-put-bytevector (uri port))

(define-label <segment-bytevector>
  (parent <percent-encoded-bytevector>)
  (predicate (lambda (O)
	       ($bytevector-not-empty? O))))

(define-label <list-of-segments>
  (parent <list>)
  (predicate (lambda (O)
	       ($for-all1 (<segment-bytevector>) O))))

;;; --------------------------------------------------------------------

(define-class <path>
  (nongenerative nausicaa:net:addresses:uri:<path>)
  (fields (immutable (path <list-of-segments>))
	  (mutable   memoized-bytevector))
  (protocol (lambda (make-top)
	      (lambda (path)
		((make-top) (map uri-decode path) #f))))

  (virtual-fields
   (immutable (bytevector <bytevector>)
	      uri-path->bytevector)
   #| end of virtual-fields |# )

  (methods (put-bytevector uri-path-put-bytevector))

  #| end of class |# )

(define-method (uri-path->bytevector (O <path>))
  (or (O $memoized-bytevector)
      (receive-and-return (rep)
	  (receive (port getter)
	      (open-bytevector-output-port)
	    (put-bytevector port (O $path $car))
	    (for-each (lambda (bv)
			(put-u8 port 47) ;47 = (char->integer #\/)
			(put-bytevector port bv))
	      (O $path $cdr))
	    (getter))
	(set! (O $memoized-bytevector) rep))))

(define-method (uri-path-put-bytevector (O <path>) (port <binary-output-port>))
  (put-bytevector port (O bytevector)))

;;; --------------------------------------------------------------------

(define-class <path-empty>
  (nongenerative nausicaa:net:addresses:uri:<path-empty>)
  (parent <path>)
  (protocol (lambda (make-uri-path)
	      (lambda ()
		((make-uri-path '())))))
  #| end of class |# )

;;; --------------------------------------------------------------------

(define-class <path-abempty>
  (nongenerative nausicaa:net:addresses:uri:<path-abempty>)
  (parent <path>)
  (protocol (lambda (make-uri-path)
	      (lambda (path)
		((make-uri-path path)))))
  #| end of class |# )

(define-method (uri-path->bytevector (O <path-abempty>))
  (or (O $memoized-bytevector)
      (receive-and-return (bv)
	  (receive (port getter)
	      (open-bytevector-output-port)
	    (for-each (lambda (bv)
			(put-u8 port 47) ;47 = (char->integer #\/)
			(put-bytevector port bv))
	      (O $path))
	    (getter))
	(set! (O $memoized-bytevector) bv))))

;;; --------------------------------------------------------------------

(define-class <path-absolute>
  (nongenerative nausicaa:net:addresses:uri:<path-absolute>)
  (parent <path>)
  (protocol (lambda (make-uri-path)
	      (lambda (path)
		((make-uri-path path)))))
  #| end of class |# )

(define-method (uri-path->bytevector (O <path-absolute>))
  (or (O $memoized-bytevector)
      (receive-and-return (bv)
	  (receive (port getter)
	      (open-bytevector-output-port)
	    (for-each (lambda (bv)
			(put-u8 port 47) ;47 = (char->integer #\/)
			(put-bytevector port bv))
	      (O $path))
	    (getter))
	(set! (O $memoized-bytevector) bv))))

;;; --------------------------------------------------------------------

(define-class <path-rootless>
  (nongenerative nausicaa:net:addresses:uri:<path-rootless>)
  (parent <path>)
  (protocol (lambda (make-uri-path)
	      (lambda (path)
		((make-uri-path path)))))
  #| end of class |# )

;;; --------------------------------------------------------------------

(define (make-path-object (path-type <symbol>) path)
  (case path-type
    ((path-abempty)
     (<path-abempty> (path)))
    ((path-absolute)
     (<path-absolute> (path)))
    ((path-rootless)
     (<path-rootless> (path)))
    ((path-empty)
     (<path-empty> ()))
    (else
     (procedure-argument-violation __who__
       "invalid URI path type" path-type))))


(define-class <uri>
  (nongenerative nausicaa:net:addresses:uri:<uri>)

  (maker (lambda (stx)
	   (syntax-case stx ()
	     ((_ (?clause ...))
	      #'(%make-uri ?clause ...)))))

  (protocol
   (lambda (make-top)
     (lambda ((scheme <scheme>)
         (userinfo <userinfo>) (host ip.<ip-address>) (port <port-number>)
	 (path <path>) (query <query>) (fragment <fragment>))
       ((make-top) scheme userinfo host port path query fragment))))

  (fields (mutable (scheme	<scheme>))
	  (mutable (userinfo	<userinfo>))
	  (mutable (host	<host>))
	  (mutable (port	<port-number>))
	  (mutable (path	<path>))
	  (mutable (query	<query>))
	  (mutable (fragment	<fragment>)))

  (virtual-fields
   (immutable (string <string>)
	      (lambda ((O <uri>))
		(ascii->string (O bytevector))))

   (immutable (bytevector <bytevector>)
	      (lambda ((O <uri>))
		(define who '<uri>-bytevector)
		(receive (port getter)
		    (open-bytevector-output-port)
		  (O $scheme put-bytevector port)
		  (let ((authority (receive (authority-port authority-getter)
				       (open-bytevector-output-port)
				     (O $userinfo put-bytevector authority-port)
				     (O $host     put-bytevector authority-port)
				     (O $port     put-bytevector authority-port)
				     (authority-getter))))
		    (when (or ($bytevector-not-empty? authority)
			      ((<path-abempty>) O)
			      ((<path-empty>)   O))
		      (put-u8 47 port) ;47 = #\/
		      (put-u8 47 port) ;47 = #\/
		      (put-bytevector authority port)))
		  (O $path  put-bytevector port)
		  (O $query put-bytevector port)
		  (O $fragment put-bytevector port)
		  (getter))))

   #| end of virtual-fields |# )

  #| end of class |# )

(mk.define-maker %make-uri
    make-<uri>
  ((scheme		#f)
   (userinfo		#f)
   (host		#f)
   (port-number		#f)
   (path		#f)
   (query		#f)
   (fragment		#f)))


(define-class <relative-ref>
  (nongenerative nausicaa:net:addresses:uri:<relative-ref>)

  (maker (lambda (stx)
	   (syntax-case stx ()
	     ((_ (?clause ...))
	      #'(%make-relative-ref ?clause ...)))))

  (protocol
   (lambda (make-top)
     (lambda ((userinfo <userinfo>) (host ip.<ip-address>) (port <port-number>)
	 (path <path>) (query <query>) (fragment <fragment>))
       ((make-top) userinfo host port path query fragment))))

  (fields (mutable (userinfo	<userinfo>))
	  (mutable (host	<host>))
	  (mutable (port	<port-number>))
	  (mutable (path	<path>))
	  (mutable (query	<query>))
	  (mutable (fragment	<fragment>)))

  (virtual-fields
   (immutable (string <string>)
	      (lambda ((O <relative-ref>))
		(ascii->string (O bytevector))))

   (immutable (bytevector <bytevector>)
	      (lambda ((O <uri>))
		(define who '<uri>-bytevector)
		(receive (port getter)
		    (open-bytevector-output-port)
		  (let ((authority (receive (authority-port authority-getter)
				       (open-bytevector-output-port)
				     (O $userinfo put-bytevector authority-port)
				     (O $host     put-bytevector authority-port)
				     (O $port     put-bytevector authority-port)
				     (authority-getter))))
		    (when (or ($bytevector-not-empty? authority)
			      ((<path-abempty>) O)
			      ((<path-empty>)   O))
		      (put-u8 47 port) ;47 = #\/
		      (put-u8 47 port) ;47 = #\/
		      (put-bytevector authority port)))
		  (O $path  put-bytevector port)
		  (O $query put-bytevector port)
		  (O $fragment put-bytevector port)
		  (getter))))

   #| end of virtual-fields|# )

  #| end of class |# )

(mk.define-maker %make-relative-ref
    make-<relative-ref>
  ((userinfo		#f)
   (host		#f)
   (port-number		#f)
   (path		#f)
   (query		#f)
   (fragment		#f)))


;;;; done

)

;;; end of file
