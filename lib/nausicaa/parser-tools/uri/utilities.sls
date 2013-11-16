;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: utility functions for URI parsing
;;;Date: Sat Nov 16, 2013
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


#!r6rs
(library (nausicaa parser-tools uri utilities)
  (export
    string->uri		string->relative-ref
    bytevector->uri	bytevector->relative-ref
    read-uri		read-relative-ref)
  (import (nausicaa)
    (prefix (nausicaa parser-tools uri) uri.)
    (prefix (nausicaa uri) uri.))


;;;; "URI" parser functions

(define (string->uri str)
  (bytevector->uri (string->ascii str)))

(define (bytevector->uri bv)
  (read-uri (open-bytevector-input-port bv)))

(define (read-uri port)
  (receive (scheme authority userinfo host.type host.bv host.data port path.type path query fragment)
      (uri.parse-uri port)
    (uri.<uri>
     ((uri.scheme	(uri.<scheme> (scheme)))
      (uri.userinfo	(if userinfo
			    (uri.<userinfo> (userinfo))
			  unspecified))
      (uri.host		(uri.make-host-object host.type host.bv host.data))
      (uri.port-number	(if port
			    (uri.<port-number> ((string->number (ascii->string port))))
			  unspecified))
      (uri.path		(uri.make-path-object path.type path))
      (uri.query	(if query
			    (uri.<query> (query))
			  unspecified))
      (uri.fragment	(if fragment
			    (uri.<fragment> (fragment))
			  unspecified))))))


;;;; "relative-ref" parser functions

(define (string->relative-ref str)
  (bytevector->relative-ref (string->ascii str)))

(define (bytevector->relative-ref bv)
  (read-relative-ref (open-bytevector-input-port bv)))

(define (read-relative-ref port)
  (receive (authority userinfo host.type host.bv host.data port path.type path query fragment)
      (uri.parse-relative-ref port)
    (uri.<relative-ref>
     ((uri.userinfo	(if userinfo
			    (uri.<userinfo> (userinfo))
			  unspecified))
      (uri.host		(uri.make-host-object host.type host.bv host.data))
      (uri.port-number	(if port
			    (uri.<port-number> ((string->number (ascii->string port))))
			  unspecified))
      (uri.path		(uri.make-path-object path.type path))
      (uri.query	(if query
			    (uri.<query> (query))
			  unspecified))
      (uri.fragment	(if fragment
			    (uri.<fragment> (fragment))
			  unspecified))))))


;;;; done

)

;;; end of file
