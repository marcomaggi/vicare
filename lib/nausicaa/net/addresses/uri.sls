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
    uri-path->uri-representation
    uri-path-put-uri-representation

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

  (predicate (lambda (bv)
	       (and ($ascii-alphabetic? ($bytevector-u8-ref bv 0))
		    ($bytevector-for-all ($ascii-alpha-digit?
					  $ascii-chi-plus?
					  $ascii-chi-minus?
					  $ascii-chi-dot?)
					 1 bv))))

  (virtual-fields
   (immutable (uri-representation <ascii-bytevector>)
	      (lambda ((O <scheme>))
		;;58 = #\:
		(bytevector-append O '#vu8(58))))

   #| end of virtual-fields |# )

  (method (put-uri-representation (O <scheme>) port)
    (put-bytevector port O)
    ;;58 = #\:
    (put-u8         port 58))

  #| end of label |# )


;;;; auxiliary labels and classes: userinfo

(define-label <userinfo>
  (parent <percent-encoded-bytevector>)

  (virtual-fields
   (immutable (specified? <boolean>)
	      (lambda (bv)
		($bytevector-not-empty? bv)))

   (immutable (uri-representation <ascii-bytevector>)
	      (lambda ((O <userinfo>))
		(if (O specified?)
		    (bytevector-append O #vu8(64)) ;64 = #\@
		  '#vu8())))

   #| end of virtual-fields |# )

  (method (put-uri-representation (O <userinfo>) port)
    (when (O specified?)
      (put-bytevector port O)
      ;;64 = #\@
      (put-u8         port 64)))

  #| end of class |# )


;;;; auxiliary labels and classes: host

(define-label <host>
  (parent ip.<ip-address>)

  (virtual-fields

   (immutable (uri-representation <percent-encoded-bytevector>)
	      (lambda ((O ip.<ip-address>))
		(O percent-encoded)))

   #| end of virtual-fields |# )

  (method (put-uri-representation (O ip.<ip-address>) port)
    (put-bytevector port (O percent-encoded)))

  #| end of label |# )


;;;; auxiliary labels and classes: port number

(define-label <port-number>
  (parent <nonnegative-fixnum>)
  (predicate (lambda (fx)
	       ($fx<= fx 65535)))

  (virtual-fields
   (immutable (specified? <boolean>)
	      (lambda (fx)
		(not ($fxzero? fx))))

   (immutable (uri-representation <ascii-bytevector>)
	      (lambda ((O <port-number>))
		(if (O specified?)
		    (bytevector-append (string->ascii (number->string O)) '#vu8(58)) ;58 = #\:
		  '#vu8())))

   #| end of virtual-fields |# )

  (method (put-uri-representation (O <port-number>) port)
    (when (O specified?)
      (put-bytevector port (string->ascii (number->string O)))
      ;;58 = #\:
      (put-u8 port 58)))

  #| end of label |# )


;;;; auxiliary labels and classes: query

(define-label <query>
  (parent <percent-encoded-bytevector>)
  (virtual-fields

   (immutable (uri-representation <ascii-bytevector>)
	      (lambda (O)
		;;63 = ?
		(bytevector-append '#vu8(63) O)))

   #| end of virtual-fields |# )

  (method (put-uri-representation O port)
    ;;63 = ?
    (put-u8 port 63)
    (put-bytevector port O))

  #| end of label |# )


;;;; auxiliary labels and classes: fragment

(define-label <fragment>
  (parent <percent-encoded-bytevector>)
  (virtual-fields

   (immutable (uri-representation <ascii-bytevector>)
	      (lambda (O)
		;;35 = #
		(bytevector-append '#vu8(35) O)))

   #| end of virtual-fields |# )

  (method (put-uri-representation O port)
    ;;35 = #
    (put-u8 port 35)
    (put-bytevector port O))

  #| end of label |# )


;;;; path types

(define-generic uri-path->uri-representation (uri))
(define-generic uri-path-put-uri-representation (uri port))

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
   (immutable (uri-representation <bytevector>)
	      uri-path->uri-representation)
   #| end of virtual-fields |# )

  (methods (put-uri-representation uri-path-put-uri-representation))

  #| end of class |# )

(define-method (uri-path->uri-representation (O <path>))
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

(define-method (uri-path-put-uri-representation (O <path>) (port <binary-output-port>))
  (put-bytevector port (O uri-representation)))

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

(define-method (uri-path->uri-representation (O <path-abempty>))
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

(define-method (uri-path->uri-representation (O <path-absolute>))
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
		  (O $scheme put-uri-representation port)
		  (let ((authority (receive (authority-port authority-getter)
				       (open-bytevector-output-port)
				     (O $userinfo put-uri-representation authority-port)
				     (O $host percent-encoded)
				     (O $port put-uri-representation authority-port)
				     (authority-getter))))
		    (when (or ($bytevector-not-empty? authority)
			      ((<path-abempty>) O)
			      ((<path-empty>)   O))
		      (put-u8 47 port) ;47 = #\/
		      (put-u8 47 port) ;47 = #\/
		      (put-bytevector authority port)))
		  (O $path  put-uri-representation port)
		  (O $query put-uri-representation port)
		  (O $fragment put-uri-representation port)
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
				     (O $userinfo put-uri-representation authority-port)
				     (O $host percent-encoded)
				     (O $port put-uri-representation authority-port)
				     (authority-getter))))
		    (when (or ($bytevector-not-empty? authority)
			      ((<path-abempty>) O)
			      ((<path-empty>)   O))
		      (put-u8 47 port) ;47 = #\/
		      (put-u8 47 port) ;47 = #\/
		      (put-bytevector authority port)))
		  (O $path  put-uri-representation port)
		  (O $query put-uri-representation port)
		  (O $fragment put-uri-representation port)
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
