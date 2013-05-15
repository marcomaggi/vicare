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
    run-compensations-store
    push-compensation-thunk)
  (import (except (ikarus)
		  compensations
		  run-compensations
		  push-compensation-thunk))

  (define %invalid-compensations-store
    (case-lambda
     (()
      (assertion-violation 'compensations
	"attempt to retrieve compensations outside of compensations environment"))
     ((false/compensation-thunk)
      (assertion-violation 'compensations
	"attempt to register compensation outside of compensations environment"))))

  ;;References a function which:
  ;;
  ;;*  When  called  with  no  arguments returns  the  list  of  current
  ;;compensation thunks.
  ;;
  ;;* When called with a #f argument: resets to null the list of current
  ;;compensation thunks.
  ;;
  ;;*  When called  with a  thunk  argument: pushes  it on  the list  of
  ;;compensation thunks.
  ;;
  (define compensations
    (make-parameter %invalid-compensations-store))

  (define (run-compensations)
    ;;Run the compensations in the current compensations store.
    ;;
    (run-compensations-store (compensations)))

  (define (run-compensations-store store)
    ;;Run  the  compensations  in  the  STORE, which  must  be  a  store
    ;;function.  After running reset the compensations stack to empty.
    ;;
    (guard (E (else
	       (assertion-violation 'run-compensations-store
		 "expected null or proper list in compensations parameter"
		 (store))))
      (for-each-in-order
	  (lambda (closure)
	    (guard (E (else #f))
	      (closure)))
	(store)))
    (store #f)
    (void))

  (define (push-compensation-thunk compensation-thunk)
    ;;Push a compensation thunk on the current store.
    ;;
    (assert (procedure? compensation-thunk))
    ((compensations) compensation-thunk))

  #| end of library |# )

;;; end of file
