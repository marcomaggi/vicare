;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for "ikarus.io.ss"
;;;Date: Mon Aug 29, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (rename (ikarus)
		(parameterize	parametrise))
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing Ikarus input/output functions\n")


(parametrise ((check-test-name	'output-bytevector))

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
        (getter))
    => '#vu8())

;;; --------------------------------------------------------------------
;;; output bytes

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-u8 port 65)
        (getter))
    => '#vu8(65))

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-u8 port 1)
	(put-u8 port 2)
	(put-u8 port 3)
        (getter))
    => '#vu8(1 2 3))

;;; --------------------------------------------------------------------
;;; output bytevectors

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(1 2 3))
        (getter))
    => '#vu8(1 2 3))

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(1 2 3))
	(put-bytevector port '#vu8(4 5 6))
	(put-bytevector port '#vu8(7 8 9))
        (getter))
    => '#vu8(1 2 3 4 5 6 7 8 9))

;;; --------------------------------------------------------------------
;;; multiple getter invocation

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(1 2 3))
        (let ((result-0 (getter)))
	  (put-bytevector port '#vu8(10 20 30))
	  (list result-0 (getter))))
    => '(#vu8(1 2 3) #vu8(10 20 30)))

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(1 2 3))
	(put-bytevector port '#vu8(4 5 6))
	(put-bytevector port '#vu8(7 8 9))
        (let ((result-0 (getter)))
	  (put-bytevector port '#vu8(10 20 30))
	  (put-bytevector port '#vu8(40 50 60))
	  (put-bytevector port '#vu8(70 80 90))
	  (list result-0 (getter))))
    => '(#vu8(1 2 3 4 5 6 7 8 9) #vu8(10 20 30 40 50 60 70 80 90)))

;;; --------------------------------------------------------------------
;;; getting port position

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(port-position port))
    => 0)

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-u8 port 1)
	(port-position port))
    => 1)

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-u8 port 1)
	(put-u8 port 2)
	(put-u8 port 3)
	(port-position port))
    => 3)

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(1 2 3))
        (port-position port))
    => 3)

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(1 2 3))
	(put-bytevector port '#vu8(4 5 6))
	(put-bytevector port '#vu8(7 8 9))
	(getter)
        (port-position port))
    => 0)

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(1 2 3))
	(put-bytevector port '#vu8(4 5 6))
	(put-bytevector port '#vu8(7 8 9))
	(getter)
	(put-bytevector port '#vu8(10 20 30))
	(put-bytevector port '#vu8(40 50 60))
        (port-position port))
    => 6)

;;; --------------------------------------------------------------------
;;; setting port position

  (check	;has position?
      (let-values (((port getter) (open-bytevector-output-port)))
	(port-has-set-port-position!? port))
    => #t)

  (check	;invalid position, empty port
      (let-values (((port getter) (open-bytevector-output-port)))
	(guard (E ((i/o-invalid-position-error? E)
		   (list (condition-who E)
			 (i/o-error-position E)))
		  (else E))
	  (set-port-position! port 3)))
    => '(open-bytevector-output-port 3))

  (check	;invalid position, beyond limit
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(guard (E ((i/o-invalid-position-error? E)
		   (list (condition-who E)
			 (i/o-error-position E)))
		  (else E))
	  (set-port-position! port 20)))
    => '(open-bytevector-output-port 20))

  (check	;set and get position, no write
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(set-port-position! port 3)
	(port-position port))
    => 3)

  (check	;set and get position, with "short" write
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(set-port-position! port 3)
	(put-bytevector port '#vu8(30 40 50))
	(let ((pos (port-position port)))
	  (list pos (getter))))
    => '(6 #vu8(0 1 2 30 40 50 6 7 8 9)))

  (check	;set and get position, with "short" write
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(set-port-position! port 7)
	(put-bytevector port '#vu8(70 80 90))
	(let ((pos (port-position port)))
	  (list pos (getter))))
    => '(10 #vu8(0 1 2 3 4 5 6 70 80 90)))

  (check	;set and get position, with "long" write
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(set-port-position! port 7)
	(put-bytevector port '#vu8(70 80 90 100))
	(let ((pos (port-position port)))
	  (list pos (getter))))
    => '(11 #vu8(0 1 2 3 4 5 6 70 80 90 100)))

  (check	;set and get position, with "long" write
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(set-port-position! port 7)
	(put-bytevector port '#vu8(70 80 90 100 110 120))
	(let ((pos (port-position port)))
	  (list pos (getter))))
    => '(13 #vu8(0 1 2 3 4 5 6 70 80 90 100 110 120)))

  (check	;set and get position, with "long" write, then write again
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(set-port-position! port 7)
	(put-bytevector port '#vu8(70 80 90 100 110 120))
	(put-bytevector port '#vu8(130 140 150))
	(let ((pos (port-position port)))
	  (list pos (getter))))
    => '(16 #vu8(0 1 2 3 4 5 6 70 80 90 100 110 120 130 140 150)))

  #t)


;;;; done

(check-report)

;;; end of file
