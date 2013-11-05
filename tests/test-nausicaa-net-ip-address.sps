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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa libraries: generic IP address classes\n")


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

	(define-method (ip.ip-address-representation-string (O <one-address>))
	  (O num string))

	(define-method (ip.ip-address-representation-ascii (O <one-address>))
	  (O num string percent-encoding))

	(<one-address> O (<> (123)))

	(values (O num)
		(O string)
		(O ascii)))
    => 123 "123" #ve(ascii "123"))

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

	(define-method (ip.ip-address-representation-string (O <two-address>))
	  (O num string))

	(<two-address> O (<> (123)))

	(values (O num)
		(O string)
		(O ascii)))
    => 123 "123" #ve(ascii "123"))

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

	(define-method (ip.ip-address-representation-string (O <one-address>))
	  (O num string))

	(define-method (ip.ip-address-representation-ascii (O <one-address>))
	  (O num string percent-encoding))

	(define-method (ip.ip-address-representation-bignum (O <one-address>))
	  (O num))

	(<one-address> O (<> (123)))

	(values (O num)
		(O string)
		(O ascii)
		(O bignum)))
    => 123 "123" #ve(ascii "123") 123)

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

	(define-method (ip.ip-address-representation-string (O <two-address>))
	  (O num string))

	(define-method (ip.ip-address-representation-bignum (O <two-address>))
	  (O num))

	(<two-address> O (<> (123)))

	(values (O num)
		(O string)
		(O ascii)
		(O bignum)))
    => 123 "123" #ve(ascii "123") 123)

  #t)


;;;; done

(check-report)

;;; end of file
