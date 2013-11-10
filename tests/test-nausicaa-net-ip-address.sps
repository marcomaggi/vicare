;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for generic IP address classes
;;;Date: Tue Nov  5, 2013
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
  (prefix (nausicaa net addresses ip) ip.)
  (prefix (nausicaa parser-tools uri) uri.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa libraries: generic IP address classes\n")


;;;; helpers

(define (%make-lexer-port obj)
  (define who '%make-lexer-port)
  (cond ((string? obj)
	 (open-bytevector-input-port (string->ascii obj)))
	((bytevector? obj)
	 (open-bytevector-input-port obj))
	(else
	 (assertion-violation who "expecting string or bytevector" obj))))

(define mkport %make-lexer-port)


(parametrise ((check-test-name	'ip-address))

;;;"<ip-address>" is an abstract class.

  (check
      (let ()
	(define-class <one-address>
	  (parent ip.<ip-address>)
	  (protocol (lambda (make-ip-address)
		      (lambda (num)
			((make-ip-address) num))))
	  (fields (immutable (num <number>))))

	(define-method (ip.ip-address->percent-encoded-string (O <one-address>))
	  (O num string))

	(define-method (ip.ip-address->percent-encoded-bytevector (O <one-address>))
	  (O num string percent-encoding))

	(<one-address> O (<> (123)))

	(values (O num)
		(O percent-encoded-string) (O percent-encoded-string)
		(O percent-encoded-bytevector)  (O percent-encoded-bytevector)))
    => 123 "123" "123" '#ve(ascii "123") '#ve(ascii "123"))

  ;;This  makes  use of  the  ascii  representation method  defined  for
  ;;"<ip-address>".
  ;;
  (check
      (let ()
	(define-class <two-address>
	  (parent ip.<ip-address>)
	  (protocol (lambda (make-ip-address)
		      (lambda (num)
			((make-ip-address) num))))
	  (fields (immutable (num <number>))))

	(define-method (ip.ip-address->percent-encoded-string (O <two-address>))
	  (O num string))

	(<two-address> O (<> (123)))

	(values (O num)
		(O percent-encoded-string)
		(O percent-encoded-bytevector)))
    => 123 "123" '#ve(ascii "123"))

  #t)


(parametrise ((check-test-name	'ip-numeric-address))

;;;"<ip-numeric-address>" is an abstract class.

  (check
      (let ()
	(define-class <one-address>
	  (parent ip.<ip-numeric-address>)
	  (protocol (lambda (make-ip-numeric-address)
		      (lambda (num)
			((make-ip-numeric-address) num))))
	  (fields (immutable (num <number>))))

	(define-method (ip.ip-address->percent-encoded-string (O <one-address>))
	  (O num string))

	(define-method (ip.ip-address->percent-encoded-bytevector (O <one-address>))
	  (O num string percent-encoding))

	(define-method (ip.ip-address->bignum (O <one-address>))
	  (O num))

	(<one-address> O (<> (123)))

	(values (O num)
		(O percent-encoded-string)
		(O percent-encoded-string)
		(O percent-encoded-bytevector)
		(O percent-encoded-bytevector)
		(O bignum)
		(O bignum)))
    => 123 "123" "123" '#ve(ascii "123") '#ve(ascii "123") 123 123)

  ;;This  makes  use of  the  ascii  representation method  defined  for
  ;;"<ip-numeric-address>".
  ;;
  (check
      (let ()
	(define-class <two-address>
	  (parent ip.<ip-numeric-address>)
	  (protocol (lambda (make-ip-numeric-address)
		      (lambda (num)
			((make-ip-numeric-address) num))))
	  (fields (immutable (num <number>))))

	(define-method (ip.ip-address->percent-encoded-string (O <two-address>))
	  (O num string))

	(define-method (ip.ip-address->bignum (O <two-address>))
	  (O num))

	(<two-address> O (<> (123)))

	(values (O num)
		(O percent-encoded-string)
		(O percent-encoded-bytevector)
		(O bignum)))
    => 123 "123" '#ve(ascii "123") 123)

  #t)


(parametrise ((check-test-name	'reg-name-address))

  (check
      (let ()
	(ip.<reg-name-address> O (<> ('#ve(ascii "ci%3Fa%3Do"))))

	(values (O percent-encoded-string) (O percent-encoded-string)
		(O percent-encoded-bytevector)  (O percent-encoded-bytevector)
		(O percent-encoded-bytevector percent-decoded)))
    => "ci%3Fa%3Do" "ci%3Fa%3Do"
    '#ve(ascii "ci%3Fa%3Do") '#ve(ascii "ci%3Fa%3Do")
    '#ve(ascii "ci?a=o"))

  #t)


(parametrise ((check-test-name	'ipvfuture-address))

  (check
      (let ()
	(ip.<ipvfuture-address> O (<> (10 '#ve(ascii "ci%3Fa%3Do"))))

	(values (O percent-encoded-string) (O percent-encoded-string)
		(O percent-encoded-bytevector) (O percent-encoded-bytevector)
		(O literal percent-decoded)))
    => "[vA.ci%3Fa%3Do]" "[vA.ci%3Fa%3Do]"
    '#ve(ascii "[vA.ci%3Fa%3Do]") '#ve(ascii "[vA.ci%3Fa%3Do]")
    '#ve(ascii "ci?a=o"))

  #t)


(parametrise ((check-test-name	'host-class))

;;; registered name

  (check
      (let* ((port (mkport "github.io"))
	     ((host ip.<reg-name-address>) (receive (host.type host.ascii host.data)
					       (uri.parse-host port)
					     (ip.make-host-object host.type host.ascii host.data))))
	(host percent-encoded-string))
    => "github.io")

  (check
      (let* ((port (mkport "github.io"))
	     ((host ip.<ip-address>) (receive (host.type host.ascii host.data)
					 (uri.parse-host port)
				       (ip.make-host-object host.type host.ascii host.data))))
	(host percent-encoded-string))
    => "github.io")

;;; --------------------------------------------------------------------
;;; IPv4 address

  (check
      (let* ((port (mkport "1.2.3.4"))
	     ((host ip.<ipv4-address>) (receive (host.type host.ascii host.data)
					   (uri.parse-host port)
					 (ip.make-host-object host.type host.ascii host.data))))
	(host percent-encoded-string))
    => "1.2.3.4")

  (check
      (let* ((port (mkport "1.2.3.4"))
	     ((host ip.<ip-address>) (receive (host.type host.ascii host.data)
					 (uri.parse-host port)
				       (ip.make-host-object host.type host.ascii host.data))))
	(host percent-encoded-string))
    => "1.2.3.4")

;;; --------------------------------------------------------------------
;;; IPv6 address

  (check
      (let* ((port (mkport "[1:2:3:4:5:6:7:8]"))
	     ((host ip.<ipv6-address>) (receive (host.type host.ascii host.data)
					   (uri.parse-host port)
					 (ip.make-host-object host.type host.ascii host.data))))
	(host percent-encoded-string))
    => "[1:2:3:4:5:6:7:8]")

  (check
      (let* ((port (mkport "[1:2:3:4:5:6:7:8]"))
	     ((host ip.<ip-address>) (receive (host.type host.ascii host.data)
					 (uri.parse-host port)
				       (ip.make-host-object host.type host.ascii host.data))))
	(host percent-encoded-string))
    => "[1:2:3:4:5:6:7:8]")

;;; --------------------------------------------------------------------
;;; IPvFuture address

  (check
      (let* ((port (mkport "[v9.ciao]"))
	     ((host ip.<ipvfuture-address>) (receive (host.type host.ascii host.data)
						(uri.parse-host port)
					      (ip.make-host-object host.type host.ascii host.data))))
	(host percent-encoded-string))
    => "[v9.ciao]")

  (check
      (let* ((port (mkport "[v9.ciao]"))
	     ((host ip.<ip-address>) (receive (host.type host.ascii host.data)
					 (uri.parse-host port)
				       (ip.make-host-object host.type host.ascii host.data))))
	(host percent-encoded-string))
    => "[v9.ciao]")

  #t)


;;;; done

(check-report)

;;; end of file
