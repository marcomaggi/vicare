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
    source-bytevector)
  (import (nausicaa)
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

(define-label <nonempty-bytevector>
  (parent <bytevector>)
  (predicate (lambda (bv)
	       ($fxpositive? ($bytevector-length bv)))))

(define-label <uri-scheme>
  (parent <nonempty-bytevector>)
  (predicate (lambda (bv)
	       #f)))


(define-class <uri>
  (nongenerative nausicaa:net:addresses:uri:<uri>)

  (maker (lambda (stx)
	   (syntax-case stx ()
	     ((_ (?clause ...))
	      #'(%make-uri ?clause ...)))))

  (protocol
   (lambda (make-top)
     (lambda (scheme authority userinfo host-type host port path-type path query fragment)
       ((make-top) scheme authority
	(and userinfo (uri.percent-decode userinfo))
	host-type (if (eq? host-type 'reg-name)
		      (uri.percent-decode host)
		    host)
	port path-type (map (lambda (p)
			      (uri.percent-decode p))
			 path)
	(and query (uri.percent-decode query))
	(and fragment (uri.percent-decode fragment))))))

   (fields (mutable scheme)
	   (mutable authority)
	   (mutable userinfo)
	   (mutable host-type)
	   (mutable host)
	   (mutable port)
	   (mutable path-type)
	   (mutable path)
	   (mutable query)
	   (mutable fragment))

   (virtual-fields
    (immutable (string <string>)
	       (lambda ((O <uri>))
		 (ascii->string (O $bytevector))))

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
					(%put-bv (uri.percent-encode (O $userinfo)))
					(%put-u8 64)) ;64 = #\@
				      (case (O $host-type)
					((reg-name)
					 (%put-bv (uri.percent-encode (O $host))))
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
					 (assertion-violation who "invalid host type" o (O $host-type))))
				      (when (O $port)
					(%put-u8 58) ;58 = #\:
					(%put-bv (O $port)))
				      (authority-getter))))
		     (when (or (not (zero? (bytevector-length authority)))
			       (memq (O $path-type) '(path-abempty path-empty)))
		       (%put-u8 47) ;47 = #\/
		       (%put-u8 47) ;47 = #\/
		       (%put-bv authority)))

		   (unless (null? (O $path))
		     (let ((first	(car (O $path)))
			   (rest	(cdr (O $path))))
		       (case (O $path-type)
			 ((path-abempty path-absolute)
			  (%put-u8 47) ;47 = /
			  (%put-bv first))
			 ((path-rootless)
			  (%put-bv first))
			 (else
			  (assertion-violation who "invalid path type" o (O $path-type))))
		       (for-each (lambda (bv)
				   (%put-u8 47) ;47 = /
				   (%put-bv bv))
			 rest)))

		   (when (O $query)
		     (%put-u8 63) ;63 = ?
		     (%put-bv (uri.percent-encode (O $query))))

		   (when (O $fragment)
		     (%put-u8 35) ;35 = #
		     (%put-bv (uri.percent-encode (O $fragment))))

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

;;;(uri.parse-relative-ref (open-bytevector-input-port source-bytevector))

  (protocol
   (lambda (make-top)
     (lambda (authority userinfo host-type host port path-type path query fragment)
       ((make-top) authority
	(and userinfo (uri.percent-decode userinfo))
	host-type (if (eq? host-type 'reg-name)
		      (uri.percent-decode host)
		    host)
	port path-type (map (lambda (p)
			      (uri.percent-decode p))
			 path)
	(and query (uri.percent-decode query))
	(and fragment (uri.percent-decode fragment))))))

   (fields (mutable authority)
	   (mutable userinfo)
	   (mutable host-type)
	   (mutable host)
	   (mutable port)
	   (mutable path-type)
	   (mutable path)
	   (mutable query)
	   (mutable fragment))

  (virtual-fields
   (immutable (string <string>)
	      (lambda ((O <relative-ref>))
		(ascii->string (O $bytevector))))

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
				       (%put-bv (uri.percent-encode (O $userinfo)))
				       (%put-u8 64)) ;64 = #\@
				     (case (O $host-type)
				       ((reg-name)
					(%put-bv (uri.percent-encode (O $host))))
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
					(assertion-violation who "invalid host type" o (O $host-type))))
				     (when (O $port)
				       (%put-u8 58) ;58 = #\:
				       (%put-bv (O $port)))
				     (authority-getter))))
		    (when (or (not (zero? (bytevector-length authority)))
			      (memq (O $path-type) '(path-abempty path-empty)))
		      (%put-u8 47) ;47 = #\/
		      (%put-u8 47) ;47 = #\/
		      (%put-bv authority)))

		  (unless (null? (O $path))
		    (let ((first	(car (O $path)))
			  (rest	(cdr (O $path))))
		      (case (O $path-type)
			((path-abempty path-absolute)
			 (%put-u8 47) ;47 = /
			 (%put-bv first))
			((path-noscheme)
			 (%put-bv first))
			(else
			 (assertion-violation who "invalid path type" o (O $path-type))))
		      (for-each (lambda (bv)
				  (%put-u8 47) ;47 = /
				  (%put-bv bv))
			rest)))

		  (when (O $query)
		    (%put-u8 63) ;63 = ?
		    (%put-bv (uri.percent-encode (O $query))))

		  (when (O $fragment)
		    (%put-u8 35) ;35 = #
		    (%put-bv (uri.percent-encode (O $fragment))))

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
