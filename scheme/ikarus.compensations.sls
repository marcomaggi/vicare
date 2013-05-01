;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: stacks of compensations for asynchronous resources
;;;Date: Tue Apr 30, 2013
;;;
;;;Abstract
;;;
;;;	This library was originally part of Nausicaa/Scheme.
;;;
;;;Copyright (c) 2009-2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (ikarus compensations)
  (export
    compensations
    run-compensations
    push-compensation-thunk)
  (import (except (ikarus)
		  compensations
		  run-compensations
		  push-compensation-thunk))

  (define compensations
    (make-parameter '()))

  (define (run-compensations)
    (for-each
	(lambda (closure)
	  (guard (E (else #f))
	    (closure)))
      (compensations))
    (compensations '())
    (void))

  (define (push-compensation-thunk thunk)
    (define who 'push-compensation-thunk)
    (assert (procedure? thunk))
    (if (compensations)
	(compensations (cons thunk (compensations)))
      (error who "attempt to push compensation thunk out of context" thunk)))

  #| end of library |# )

;;; end of file
