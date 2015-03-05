;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: input/output functions for Mehve
;;;Date: Fri Nov 29, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013, 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa mehve language input-output (0 4))
  (options visit-upon-loading)
  (export
    display		write

    display-1		write-1
    display-2		write-2)
  (import (except (nausicaa (0 4))
		  display
		  write)
    (prefix (only (nausicaa (0 4))
		  display
		  write)
	    nau.))


;;;; input/output

(define-syntax (display stx)
  (syntax-case stx ()
    (?id
     (identifier? #'?id)
     #'display-N)
    ((_ ?obj)
     #'(display-1 ?obj))
    ((_ ?obj ?port)
     #'(display-2 ?obj ?port))
    ))

(define-syntax (write stx)
  (syntax-case stx ()
    (?id
     (identifier? #'?id)
     #'write-N)
    ((_ ?obj)
     #'(write-1 ?obj))
    ((_ ?obj ?port)
     #'(write-2 ?obj ?port))
    ))

(define display-N
  (case-lambda
   ((obj)
    (display-1 obj))
   ((obj port)
    (display-2 obj port))
   ))

(define write-N
  (case-lambda
   ((obj)
    (write-1 obj))
   ((obj port)
    (write-2 obj port))
   ))

(define-generic display-1	(obj))
(define-generic display-2	(obj port))
(define-generic write-1		(obj))
(define-generic write-2		(obj port))


;;;; initialisation

(add-method display-1	(<top>)		nau.display)
(add-method display-2	(<top> <port>)	nau.display)
(add-method write-1	(<top>)		nau.write)
(add-method write-2	(<top> <port>)	nau.write)


;;;; done

)

;;; end of file
