;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for bytevector-compound objects
;;;Date: Tue Apr 16, 2013
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
(import (vicare)
  (vicare bytevector-compounds)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: bytevector-compounds\n")


(parametrise ((check-test-name	'base))

  (define (doit . args)
    (let ((bvcom (apply make-bytevector-compound args)))
      (list (bytevector-compound-length bvcom)
	    (bytevector-compound-total-length bvcom)
	    (bytevector-compound-empty? bvcom)
	    (bytevector-compound-filled? bvcom))))

  (check
      (doit)
    => '(0 0 #t #f))

  (check
      (doit '#vu8(1 2 3))
    => '(3 3 #f #t))

  (check
      (doit '#vu8(1 2 3) '#vu8(4 5 6) '#vu8(7 8 9))
    => '(9 9 #f #t))

  #t)


(parametrise ((check-test-name	'base))

  (define (doit bvcom)
    (list (bytevector-compound-length bvcom)
	  (bytevector-compound-total-length bvcom)
	  (bytevector-compound-empty? bvcom)
	  (bytevector-compound-filled? bvcom)))

;;; --------------------------------------------------------------------

  (check	;enqueueing
      (let ((bvcom (make-bytevector-compound)))
	(bytevector-compound-enqueue! bvcom '#vu8(1 2 3))
	(bytevector-compound-enqueue! bvcom '#vu8(4 5 6))
	(bytevector-compound-enqueue! bvcom '#vu8(7 8))
	(list (bytevector-compound-data bvcom)
	      (doit bvcom)))
    => '((#vu8(1 2 3) #vu8(4 5 6) #vu8(7 8))
	 (8 8 #f #t)))

  (check	;enqueueing and dequeueing
      (let ((bvcom (make-bytevector-compound)))
	(bytevector-compound-enqueue! bvcom '#vu8(1 2 3))
	(bytevector-compound-enqueue! bvcom '#vu8(4 5 6))
	(bytevector-compound-enqueue! bvcom '#vu8(7 8))
	(let ((rv (bytevector-compound-dequeue! bvcom)))
	  (list rv (doit bvcom))))
    => '(#vu8(1 2 3) (5 8 #f #t)))

  (check	;enqueueing and dequeueing
      (let ((bvcom (make-bytevector-compound)))
	(bytevector-compound-enqueue! bvcom '#vu8(1 2 3))
	(let ((rv (bytevector-compound-dequeue! bvcom)))
	  (list rv (doit bvcom))))
    => '(#vu8(1 2 3) (0 3 #t #f)))

  (check	;enqueueing and dequeueing
      (let ((bvcom (make-bytevector-compound)))
	(bytevector-compound-enqueue! bvcom '#vu8(1 2 3))
	(let* ((rv0 (bytevector-compound-dequeue! bvcom))
	       (rv1 (bytevector-compound-dequeue! bvcom)))
	  (list rv0 rv1 (doit bvcom))))
    => '(#vu8(1 2 3) #f (0 3 #t #f)))

  (check	;enqueueing and dequeueing till empty
      (let ((bvcom (make-bytevector-compound)))
	(bytevector-compound-enqueue! bvcom '#vu8(1 2 3))
	(bytevector-compound-enqueue! bvcom '#vu8(4 5 6))
	(bytevector-compound-enqueue! bvcom '#vu8(7 8))
	(let* ((rv0 (bytevector-compound-dequeue! bvcom))
	       (rv1 (bytevector-compound-dequeue! bvcom))
	       (rv2 (bytevector-compound-dequeue! bvcom))
	       (rv3 (bytevector-compound-dequeue! bvcom)))
	  (list rv0 rv1 rv2 rv3 (doit bvcom))))
    => '(#vu8(1 2 3) #vu8(4 5 6) #vu8(7 8) #f (0 8 #t #f)))


  #t)


;;;; done

(check-report)

;;; end of file
