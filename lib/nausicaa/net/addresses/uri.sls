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
    ;;<uri> <relative-ref>

    ;; auxiliary syntaxes
    )
  (import (nausicaa)
    (prefix (nausicaa net addresses ip) ip.)
    (prefix (vicare language-extensions makers) mk.)
    (vicare unsafe operations)
    (vicare language-extensions ascii-chars)
    ;;FIXME  To be  removed at  the  next boot  image rotation.   (Marco
    ;;Maggi; Mon Nov 4, 2013)
    (only (vicare system $bytevectors)
	  $uri-encoded-bytevector?))


;;;; helpers

(define-auxiliary-syntaxes
  scheme
  authority
  userinfo
  host
  port
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

(define ($for-all1 func ell)
  (if (pair? ($cdr ell))
      (and (func ($car ell))
	   ($for-all1 func ($cdr ell)))
    ;;Last call in tail position.
    (func ($car ell))))


;;;; auxiliary labels and classes: scheme

(define-label <uri-scheme>
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
	      (lambda ((O <uri-scheme>))
		;;58 = #\:
		(bytevector-append O '#vu8(58))))

   #| end of virtual-fields |# )

  (method (put-uri-representation (O <uri-scheme>) port)
    (put-bytevector port O)
    ;;58 = #\:
    (put-u8         port 58))

  #| end of label |# )


;;;; auxiliary labels and classes: userinfo

(define-label <uri-userinfo>
  (parent <percent-encoded-bytevector>)

  (virtual-fields
   (immutable (specified? <boolean>)
	      (lambda (bv)
		($bytevector-not-empty? bv)))

   (immutable (uri-representation <ascii-bytevector>)
	      (lambda ((O <uri-userinfo>))
		(if (O specified?)
		    (bytevector-append O #vu8(64)) ;64 = #\@
		  '#vu8())))

   #| end of virtual-fields |# )

  (method (put-uri-representation (O <uri-userinfo>) port)
    (when (O specified?)
      (put-bytevector port O)
      ;;64 = #\@
      (put-u8         port 64)))

  #| end of class |# )


;;;; auxiliary labels and classes: host

(define-label <uri-host>
  (parent ip.<ip-address>)

  (virtual-fields

   (immutable (uri-representation <ascii-bytevector>)
	      (lambda ((O ip.<ip-address>))
		(O percent-encoded)))

   #| end of virtual-fields |# )

  (method (put-uri-representation (O ip.<ip-address>) port)
    (put-bytevector port (O percent-encoded)))

  #| end of label |# )


;;;; auxiliary labels and classes: port number

(define-label <uri-port>
  (parent <nonnegative-fixnum>)
  (predicate (lambda (fx)
	       ($fx<= fx 65535)))

  (virtual-fields
   (immutable (specified? <boolean>)
	      (lambda (fx)
		(not ($fxzero? fx))))

   (immutable (uri-representation <ascii-bytevector>)
	      (lambda ((O <uri-port>))
		(if (O specified?)
		    (bytevector-append (string->ascii (number->string O)) '#vu8(58)) ;58 = #\:
		  '#vu8())))

   #| end of virtual-fields |# )

  (method (put-uri-representation (O <uri-port>) port)
    (when (O specified?)
      (put-bytevector port (string->ascii (number->string O)))
      ;;58 = #\:
      (put-u8 port 58)))

  #| end of label |# )


;;;; auxiliary labels and classes: query

(define-label <uri-query>
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

(define-label <uri-fragment>
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

(define-class <uri-path>
  (nongenerative nausicaa:net:addresses:uri:<uri-path>)
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

(define-method (uri-path->uri-representation (O <uri-path>))
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

(define-method (uri-path-put-uri-representation (O <uri-path>) (port <binary-output-port>))
  (put-bytevector port (O uri-representation)))

;;; --------------------------------------------------------------------

(define-class <uri-path-empty>
  (nongenerative nausicaa:net:addresses:uri:<uri-path-empty>)
  (parent <uri-path>)
  (protocol (lambda (make-uri-path)
	      (lambda ()
		((make-uri-path '())))))
  #| end of class |# )

;;; --------------------------------------------------------------------

(define-class <uri-path-abempty>
  (nongenerative nausicaa:net:addresses:uri:<uri-path-abempty>)
  (parent <uri-path>)
  (protocol (lambda (make-uri-path)
	      (lambda (path)
		((make-uri-path path)))))
  #| end of class |# )

(define-method (uri-path->uri-representation (O <uri-path-abempty>))
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

(define-class <uri-path-absolute>
  (nongenerative nausicaa:net:addresses:uri:<uri-path-absolute>)
  (parent <uri-path>)
  (protocol (lambda (make-uri-path)
	      (lambda (path)
		((make-uri-path path)))))
  #| end of class |# )

(define-method (uri-path->uri-representation (O <uri-path-absolute>))
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

(define-class <uri-path-rootless>
  (nongenerative nausicaa:net:addresses:uri:<uri-path-rootless>)
  (parent <uri-path>)
  (protocol (lambda (make-uri-path)
	      (lambda (path)
		((make-uri-path path)))))
  #| end of class |# )

;;; --------------------------------------------------------------------

(define (make-uri-path (path-type <symbol>) path)
  (case path-type
    ((path-abempty)
     (<uri-path-abempty> (path)))
    ((path-absolute)
     (<uri-path-absolute> (path)))
    ((path-rootless)
     (<uri-path-rootless> (path)))
    ((path-empty)
     (<uri-path-empty> ()))
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
     (lambda ((scheme <uri-scheme>)
         (userinfo <uri-userinfo>) (host ip.<ip-address>) (port <uri-port>)
	 (path <uri-path>) (query <uri-query>) (fragment <uri-fragment>))
       ((make-top) scheme userinfo host port path query fragment))))

  (fields (mutable (scheme	<uri-scheme>))
	  (mutable (userinfo	<uri-userinfo>))
	  (mutable (host	<uri-host>))
	  (mutable (port	<uri-port>))
	  (mutable (path	<uri-path>))
	  (mutable (query	<uri-query>))
	  (mutable (fragment	<uri-fragment>)))

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
			      ((<uri-path-abempty>) O)
			      ((<uri-path-empty>)   O))
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
   (port		#f)
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
     (lambda ((userinfo <uri-userinfo>) (host ip.<ip-address>) (port <uri-port>)
	 (path <uri-path>) (query <uri-query>) (fragment <uri-fragment>))
       ((make-top) userinfo host port path query fragment))))

  (fields (mutable (userinfo	<uri-userinfo>))
	  (mutable (host	<uri-host>))
	  (mutable (port	<uri-port>))
	  (mutable (path	<uri-path>))
	  (mutable (query	<uri-query>))
	  (mutable (fragment	<uri-fragment>)))

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
			      ((<uri-path-abempty>) O)
			      ((<uri-path-empty>)   O))
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
   (port		#f)
   (path		#f)
   (query		#f)
   (fragment		#f)))


;;;; done

)

;;; end of file
