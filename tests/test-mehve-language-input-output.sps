;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for mehve input output functions
;;;Date: Mon Jun 20, 2011
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
(import (nausicaa mehve)
  (prefix (nausicaa mehve input-output) mehve.)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing mehve, input output functions\n")


(parametrise ((check-test-name	'display))

  (define-method (mehve.display-2 (o <vector>) (p <port>))
    (o.for-each (lambda (item)
		  (display item p))))

  (define-method (mehve.write-2 (o <vector>) (p <port>))
    (o.for-each (lambda (item)
		  (write item p))))

;;; --------------------------------------------------------------------

  (check
      (call-with-string-output-port
       (lambda (port)
	 (display "ciao" port)))
    => "ciao")

  (check
      (call-with-string-output-port
       (lambda (port)
	 (display '#(1 2 3) port)))
    => "123")

  (check
      (call-with-string-output-port
       (lambda (port)
	 (apply display (list "ciao" port))))
    => "ciao")

;;; --------------------------------------------------------------------

  (check
      (call-with-string-output-port
       (lambda (port)
	 (write "ciao" port)))
    => "\"ciao\"")

  (check
      (call-with-string-output-port
       (lambda (port)
	 (write '#(1 2 3) port)))
    => "123")

  (check
      (call-with-string-output-port
       (lambda (port)
	 (apply write (list "ciao" port))))
    => "\"ciao\"")

  #t)


;;;; done

(check-report)

;;; end of file
