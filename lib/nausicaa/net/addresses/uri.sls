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

    ;; auxiliary syntaxes
    )
  (import (nausicaa)
    (nausicaa net addresses ip-address)
    (nausicaa net addresses ipv4)
    (nausicaa net addresses ipv6)
    (prefix (vicare language-extensions makers) mk.)
    (vicare unsafe operations))


;;;; helpers

(define-auxiliary-syntaxes
  scheme
  authority
  userinfo
  host-type
  host
  port
  path-type
  path
  query
  fragment)

(define-inline (integer->ascii-hex n)
  (if (<= 0 n 9)
      (+ 48 n)	      ;48 = #\0
    (+ 65 (- n 10)))) ;65 = #\A


;;;; auxiliary labels

(define-label <uri-scheme>
  (parent <nonempty-bytevector>))

(define-label <uri-authority>
  (parent <bytevector>))

(define-label <uri-userinfo>
  (parent <bytevector>))

(define-label <uri-host>
  (parent <bytevector>))

(define-label <uri-port>
  (parent <bytevector>))

(define-label <uri-query>
  (parent <bytevector>))

(define-label <uri-fragment>
  (parent <bytevector>))

;;; --------------------------------------------------------------------

(define-label <decoded-list-of-segments>
  (parent <list>)
  (predicate (lambda (self)
	       (for-all (<nonempty-bytevector>) self)))
  (protocol (lambda ()
	      (lambda (path)
		(map uri-decode path)))))


;;;; host types

(define (make-uri-host (host-type <symbol>) host-data)
  (case host-type
    ((reg-name)
     (<reg-name-address> (host-data)))
    ((ipv4-address)
     (let (((vec <vector>) (cdr host-data)))
       (<ipv4-address> (($vector-ref vec 0)
			($vector-ref vec 1)
			($vector-ref vec 2)
			($vector-ref vec 3)))))
    ((ipv6-address)
     (let (((vec <vector>) (cdr host-data)))
       (<ipv6-address> (($vector-ref vec 0)
			($vector-ref vec 1)
			($vector-ref vec 2)
			($vector-ref vec 3)
			($vector-ref vec 4)
			($vector-ref vec 5)
			($vector-ref vec 6)
			($vector-ref vec 7)))))
    ((ipvfuture)
     (<ipvfuture-address> (host-data)))
    (else
     (procedure-argument-violation __who__
       "invalid URI host type" host-type))))


;;;; path types

(define-generic uri-path->bytevector (uri))

;;; --------------------------------------------------------------------

(define-class <uri-path>
  (nongenerative nausicaa:net:addresses:uri:<uri-path>)
  (fields (immutable (path <decoded-list-of-segments>))
	  (mutable   memoized-bytevector))
  (protocol (lambda (make-top)
	      (lambda (path)
		((make-top) (map uri-decode path) #f))))

  (virtual-fields
   (immutable (bytevector <bytevector>)
	      uri-path->bytevector)
   #| end of virtual-fields |# )

  #| end of class |# )

(define-method (uri-path->bytevector (O <uri-path>))
  (or (O $memoized-bytevector)
      (receive-and-return (bv)
	  (receive (port getter)
	      (open-bytevector-output-port)
	    (put-bytevector port (O $path $car))
	    (for-each (lambda (bv)
			(put-u8 port 47) ;47 = (char->integer #\/)
			(put-bytevector port bv))
	      (O $path $cdr))
	    (getter))
	(set! (O $memoized-bytevector) bv))))

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
  (virtual-fields
   (immutable (bytevector <bytevector>)
	      uri-path->bytevector)
   #| end of virtual-fields |# )
  #| end of class |# )

(define-method (uri-path->bytevector (O <uri-path-abempty>))
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
  (virtual-fields
   (immutable (bytevector <bytevector>)
	      uri-path->bytevector)
   #| end of virtual-fields |# )
  #| end of class |# )

(define-method (uri-path->bytevector (O <uri-path-absolute>))
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
     (lambda ((scheme <uri-scheme>) (authority <uri-authority>) (userinfo <uri-userinfo>)
	 (host-type <symbol>) (host <uri-host>) (port <uri-port>)
	 (path-type <symbol>) (path <uri-path>)
	 (query <uri-query>) (fragment <uri-fragment>))
       ((make-top) scheme authority
	(and userinfo (uri-decode userinfo))
	host-type (if (eq? host-type 'reg-name)
		      (uri-decode host)
		    host)
	port
	(make-uri-path path-type path)
	(and query (uri-decode query))
	(and fragment (uri-decode fragment))))))

  (fields (mutable (scheme	<uri-scheme>))
	  (mutable (authority	<uri-authority>))
	  (mutable (userinfo	<uri-userinfo>))
	  (mutable (host-type	<symbol>))
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
		   (define-inline (%put-bv ?thing)
		     (put-bytevector port ?thing))
		   (define-inline (%put-u8 ?thing)
		     (put-u8 port ?thing))

		   (%put-bv (O $scheme))
		   (%put-u8 58) ;58 = #\:

		   (let ((authority (receive (authority-port authority-getter)
					(open-bytevector-output-port)
				      (define-inline (%put-bv ?thing)
					(put-bytevector authority-port ?thing))
				      (define-inline (%put-u8 ?thing)
					(put-u8 authority-port ?thing))
				      (when (O $userinfo)
					(%put-bv (uri-encode (O $userinfo)))
					(%put-u8 64)) ;64 = #\@
				      (case (O $host-type)
					((reg-name)
					 (%put-bv (uri-encode (O $host))))
					((ipv4-address)
					 (%put-bv (car (O $host))))
					((ipv6-address)
					 (%put-u8 91) ;91 = #\[
					 (%put-bv (car (O $host)))
					 (%put-u8 93)) ;93 = #\]
					((ipvfuture)
					 (%put-u8 91)  ;91 = #\[
					 (%put-u8 118) ;118 = #\v
					 (%put-u8 (integer->ascii-hex (car (O $host))))
					 (%put-bv (cdr (O $host)))
					 (%put-u8 93)) ;93 = #\]
					(else
					 (assertion-violation who "invalid host type" O (O $host-type))))
				      (when (O $port)
					(%put-u8 58) ;58 = #\:
					(%put-bv (O $port)))
				      (authority-getter))))
		     (when (or (not (zero? (bytevector-length authority)))
			       ((<uri-path-abempty>) O)
			       ((<uri-path-empty>)   O))
		       (%put-u8 47) ;47 = #\/
		       (%put-u8 47) ;47 = #\/
		       (%put-bv authority)))

		   (%put-bv (O $path bytevector))

		   (when (O $query)
		     (%put-u8 63) ;63 = ?
		     (%put-bv (uri-encode (O $query))))

		   (when (O $fragment)
		     (%put-u8 35) ;35 = #
		     (%put-bv (uri-encode (O $fragment))))

		   (getter))))

    #| end of virtual-fields |# )

  #| end of class |# )

(mk.define-maker %make-uri
    make-<uri>
  ((scheme	#f)
   (authority	#f)
   (userinfo	#f)
   (host-type	#f)
   (host	#f)
   (port	#f)
   (path-type	#f)
   (path	#f)
   (query	#f)
   (fragment	#f)))


(define-class <relative-ref>
  (nongenerative nausicaa:net:addresses:uri:<relative-ref>)

  (maker (lambda (stx)
	   (syntax-case stx ()
	     ((_ (?clause ...))
	      #'(%make-relative-ref ?clause ...)))))

  (protocol
   (lambda (make-top)
     (lambda ((authority <uri-authority>) (userinfo <uri-userinfo>)
	 (host-type <symbol>) (host <uri-host>) (port <uri-port>)
	 (path-type <symbol>) (path <uri-path>)
	 (query <uri-query>) (fragment <uri-fragment>))
       ((make-top) authority
	(and userinfo (uri-decode userinfo))
	host-type (if (eq? host-type 'reg-name)
		      (uri-decode host)
		    host)
	(make-uri-path path-type path)
	(and query (uri-decode query))
	(and fragment (uri-decode fragment))))))

  (fields (mutable (authority	<uri-authority>))
	  (mutable (userinfo	<uri-userinfo>))
	  (mutable (host-type	<symbol>))
	  (mutable (host	<uri-host>))
	  (mutable (port	<uri-port>))
	  (mutable (path-type	<symbol>))
	  (mutable (path	<uri-path>))
	  (mutable (query	<uri-query>))
	  (mutable (fragment	<uri-fragment>)))

  (virtual-fields
   (immutable (string <string>)
	      (lambda ((O <relative-ref>))
		(ascii->string (O bytevector))))

   (immutable (bytevector <bytevector>)
	      (lambda ((O <relative-ref>))
		(define who '<relative-ref>-bytevector)
		(receive (port getter)
		    (open-bytevector-output-port)
		  (define-inline (%put-bv ?thing)
		    (put-bytevector port ?thing))
		  (define-inline (%put-u8 ?thing)
		    (put-u8 port ?thing))

		  (let ((authority (receive (authority-port authority-getter)
				       (open-bytevector-output-port)
				     (define-inline (%put-bv ?thing)
				       (put-bytevector authority-port ?thing))
				     (define-inline (%put-u8 ?thing)
				       (put-u8 authority-port ?thing))
				     (when (O $userinfo)
				       (%put-bv (uri-encode (O $userinfo)))
				       (%put-u8 64)) ;64 = #\@
				     (case (O $host-type)
				       ((reg-name)
					(%put-bv (uri-encode (O $host))))
				       ((ipv4-address)
					(%put-bv (car (O $host))))
				       ((ipv6-address)
					(%put-u8 91) ;91 = #\[
					(%put-bv (car (O $host)))
					(%put-u8 93)) ;93 = #\]
				       ((ipvfuture)
					(%put-u8 91) ;91 = #\[
					(%put-u8 118) ;118 = #\v
					(%put-u8 (integer->ascii-hex (car (O $host))))
					(%put-bv (cdr (O $host)))
					(%put-u8 93)) ;93 = #\]
				       (else
					(assertion-violation who "invalid host type" O (O $host-type))))
				     (when (O $port)
				       (%put-u8 58) ;58 = #\:
				       (%put-bv (O $port)))
				     (authority-getter))))
		    (when (or (not (zero? (bytevector-length authority)))
			      ((<uri-path-abempty>) O)
			      ((<uri-path-empty>)   O))
		      (%put-u8 47) ;47 = #\/
		      (%put-u8 47) ;47 = #\/
		      (%put-bv authority)))

		  (%put-bv (O $path bytevector))

		  (when (O $query)
		    (%put-u8 63) ;63 = ?
		    (%put-bv (uri-encode (O $query))))

		  (when (O $fragment)
		    (%put-u8 35) ;35 = #
		    (%put-bv (uri-encode (O $fragment))))

		  (getter))))

   #| end of virtual-fields|# )

  #| end of class |# )

(mk.define-maker %make-relative-ref
    make-<relative-ref>
  ((authority		#f)
   (userinfo		#f)
   (host-type		#f)
   (host		#f)
   (port		#f)
   (path-type		#f)
   (path		#f)
   (query		#f)
   (fragment		#f)))


;;;; done

)

;;; end of file
