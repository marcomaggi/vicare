;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for URI objects
;;;Date: Fri Nov  8, 2013
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
(import (nausicaa)
  (prefix (nausicaa net addresses ip)  ip.)
  (prefix (nausicaa net addresses uri) uri.)
  (prefix (nausicaa parser-tools uri) uri.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa libraries: URI objects\n")


;;;; helpers

(define (mkport obj)
  (cond ((string? obj)
	 (open-bytevector-input-port (string->ascii obj)))
	((bytevector? obj)
	 (open-bytevector-input-port obj))
	(else
	 (assertion-violation __who__ "expecting string or bytevector" obj))))

(define (string->host-object (str <string>))
  (let ((port (open-bytevector-input-port (string->ascii str))))
    (receive (host.type host.ascii host.data)
	(uri.parse-host port)
      (ip.make-host-object host.type host.ascii host.data))))

(define (percent-encoded->host-object (bv <percent-encoded-bytevector>))
  (let ((port (open-bytevector-input-port bv)))
    (receive (host.type host.ascii host.data)
	(uri.parse-host port)
      (ip.make-host-object host.type host.ascii host.data))))


(parametrise ((check-test-name	'scheme))

  (check
      (let (((O uri.<scheme>) '#ve(ascii "http")))
        (O uri-representation))
    => '#ve(ascii "http:"))

  (check
      (let (((O uri.<scheme>) '#ve(ascii "http")))
	(receive (port getter)
	    (open-bytevector-output-port)
	  (O put-uri-representation port)
	  (getter)))
    => '#ve(ascii "http:"))

  (check-for-true
   (let (((O uri.<scheme>) '#ve(ascii "http")))
     (fixnum? (O hash))))

;;; --------------------------------------------------------------------

  (check	;empty "scheme" is invalid
      (try
	  (let (((O uri.<scheme>) '#vu8()))
	    #f)
	(catch E
	  (&tagged-binding-violation
	   #t)
	  (else E)))
    => #t)

  #t)


(parametrise ((check-test-name	'userinfo))

  (check
      (let (((O uri.<userinfo>) '#vu8()))
        (O uri-representation))
    => '#vu8())

  (check
      (let (((O uri.<userinfo>) '#ve(ascii "marco")))
        (O uri-representation))
    => '#ve(ascii "marco@"))

  (check
      (let (((O uri.<userinfo>) '#ve(ascii "ci%3Fa%3Do")))
        (O uri-representation))
    => '#ve(ascii "ci%3Fa%3Do@"))

  (check
      (let (((O uri.<userinfo>) '#ve(ascii "ci%3Fa%3Do")))
        (O percent-decoded))
    => '#ve(ascii "ci?a=o"))

  (check
      (let (((O uri.<userinfo>) '#ve(ascii "marco")))
	(receive (port getter)
	    (open-bytevector-output-port)
	  (O put-uri-representation port)
	  (getter)))
    => '#ve(ascii "marco@"))

  (check-for-true
   (let (((O uri.<userinfo>) '#ve(ascii "marco")))
     (fixnum? (O hash))))

;;; --------------------------------------------------------------------

  (check
      (try
	  (let (((O uri.<userinfo>) '#vu8(0)))
	    #f)
	(catch E
	  (&tagged-binding-violation
	   #t)
	  (else E)))
    => #t)

  #t)


(parametrise ((check-test-name	'host/registered-name))

  (check	;empty host is fine
      (let (((O uri.<host>) (string->host-object "")))
	(O percent-encoded-string))
    => "")

  (check	;member of <ip-address>
      (let (((O uri.<host>) (string->host-object "github.io")))
	(O percent-encoded-string))
    => "github.io")

  (check	;member of <ip-address>
      (let (((O uri.<host>) (string->host-object "ci%3Fa%3Do")))
	(O percent-encoded-string))
    => "ci%3Fa%3Do")

  (check	;member of <ip-address>
      (let (((O uri.<host>) (string->host-object "ci%3Fa%3Do")))
	(O percent-encoded-bytevector))
    => '#ve(ascii "ci%3Fa%3Do"))

  (check	;member of <bytevector> through <ip-address>
      (let (((O uri.<host>) (string->host-object "ci%3Fa%3Do")))
        (O percent-encoded-bytevector percent-decoded))
    => '#ve(ascii "ci?a=o"))

  (check	;member of <host>
      (let (((O uri.<host>) (string->host-object "ci%3Fa%3Do")))
        (O uri-representation))
    => '#ve(ascii "ci%3Fa%3Do"))

  (check	;member of <host>
      (let (((O uri.<host>) (string->host-object "ci%3Fa%3Do")))
	(receive (port getter)
	    (open-bytevector-output-port)
	  (O put-uri-representation port)
	  (getter)))
    => '#ve(ascii "ci%3Fa%3Do"))

  #t)


(parametrise ((check-test-name	'host/ipv4-address))

  (check
      (let* ((port (mkport "1.2.3.4"))
	     ((host uri.<host>) (receive (host.type host.ascii host.data)
				    (uri.parse-host port)
				  (ip.make-host-object host.type host.ascii host.data))))
	(host percent-encoded-string))
    => "1.2.3.4")

  #t)


(parametrise ((check-test-name	'host/ipv6-address))

  (check
      (let* ((port (mkport "[1:2:3:4:5:6:7:8]"))
	     ((host uri.<host>) (receive (host.type host.ascii host.data)
				    (uri.parse-host port)
				  (ip.make-host-object host.type host.ascii host.data))))
	(host percent-encoded-string))
    => "[1:2:3:4:5:6:7:8]")

  #t)


(parametrise ((check-test-name	'host/ipvfuture))

  (check
      (let* ((port (mkport "[v9.ciao]"))
	     ((host uri.<host>) (receive (host.type host.ascii host.data)
				    (uri.parse-host port)
				  (ip.make-host-object host.type host.ascii host.data))))
	(host percent-encoded-string))
    => "[v9.ciao]")

  #t)


;;;; done

(check-report)

;;; end of file
