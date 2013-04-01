;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: coroutines library
;;;Date: Mon Apr  1, 2013
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
(library (vicare coroutines)
  (export coroutine yield finish-coroutines)
  (import (vicare)
    (prefix (vicare unsafe-operations)
	    $)
    (only (vicare syntactic-extensions)
	  begin0))


(module (empty-queue? queue-append! queue-pop!)

  ;;The value of this parameter is #f  or a pair representing a queue of
  ;;escape functions.
  ;;
  ;;The car of  the queue-pair is the  first pair of the  list of escape
  ;;functions.  The cdr  of the queue-pair is the last  pair of the list
  ;;of escape functions.
  ;;
  ;;NOTE At present Vicare has no native threads, so this QUEUE variable
  ;;could  well  be a  simple  binding.   For show  off:  we  make it  a
  ;;parameter because that would be needed in a multithreading process.
  ;;
  (define queue
    (make-parameter #f))

  (define (empty-queue?)
    (not (queue)))

  (define (queue-append! escape-func)
    (if (queue)
	(let ((old-last-pair ($cdr (queue)))
	      (new-last-pair (list escape-func)))
	  ($set-cdr! old-last-pair new-last-pair)
	  ($set-cdr! (queue) new-last-pair))
      (let ((Q (list escape-func)))
	(queue (cons Q Q)))))

  (define (queue-pop!)
    (let ((Q (queue)))
      (if Q
	  (let ((head ($car Q)))
	    (begin0
		($car head)
	      (let ((head ($cdr head)))
		(if (null? head)
		    (queue #f)
		  ($set-car! Q head)))))
	(error 'queue-pop! "no more coroutines"))))

  #| end of module |# )


(define (coroutine thunk)
  ;;Create  a new  coroutine  having  THUNK as  function  and enter  it.
  ;;Return unspecified values.
  ;;
  (call/cc
      (lambda (reenter)
	(queue-append! reenter)
	(thunk)
	((queue-pop!)))))

(define (yield)
  ;;Register the  current continuation as  coroutine, then run  the next
  ;;coroutine.  Return unspecified values.
  ;;
  (coroutine values))

(define (finish-coroutines)
  ;;Loop running  the next  coroutine until there  are no  more.  Return
  ;;unspecified values.
  ;;
  (unless (empty-queue?)
    (yield)
    (finish-coroutines)))


;;;; done

)

;;; end of file
