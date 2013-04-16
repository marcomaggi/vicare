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


#!vicare
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


(parametrise ((check-test-name	'queue))

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


(parametrise ((check-test-name	'access-u8))

  (check
      (with-result
       (let ((bvcom (make-bytevector-compound '#vu8(0 1 2 3) '#vu8(4) '#vu8(5 6))))
	 (add-result (bytevector-compound-u8-ref bvcom 0))
	 (add-result (bytevector-compound-u8-ref bvcom 1))
	 (add-result (bytevector-compound-u8-ref bvcom 2))
	 (add-result (bytevector-compound-u8-ref bvcom 3))
	 (add-result (bytevector-compound-u8-ref bvcom 4))
	 (add-result (bytevector-compound-u8-ref bvcom 5))
	 (add-result (bytevector-compound-u8-ref bvcom 6))
	 #t))
    => '(#t (0 1 2 3 4 5 6)))

  (check
      (with-result
       (let ((bvcom (make-bytevector-compound '#vu8(0 1 2 3) '#vu8(4) '#vu8(5 6))))
	 (bytevector-compound-u8-set! bvcom 0 10)
	 (bytevector-compound-u8-set! bvcom 1 20)
	 (bytevector-compound-u8-set! bvcom 2 30)
	 (bytevector-compound-u8-set! bvcom 3 40)
	 (bytevector-compound-u8-set! bvcom 4 50)
	 (bytevector-compound-u8-set! bvcom 5 60)
	 (bytevector-compound-u8-set! bvcom 6 70)
	 (add-result (bytevector-compound-u8-ref bvcom 0))
	 (add-result (bytevector-compound-u8-ref bvcom 1))
	 (add-result (bytevector-compound-u8-ref bvcom 2))
	 (add-result (bytevector-compound-u8-ref bvcom 3))
	 (add-result (bytevector-compound-u8-ref bvcom 4))
	 (add-result (bytevector-compound-u8-ref bvcom 5))
	 (add-result (bytevector-compound-u8-ref bvcom 6))
	 #t))
    => '(#t (10 20 30 40 50 60 70)))

  #t)


(parametrise ((check-test-name	'access-s8))

  (check
      (with-result
       (let ((bvcom (make-bytevector-compound '#vs8(0 1 -2 -3) '#vs8(4) '#vs8(5 -6))))
	 (add-result (bytevector-compound-s8-ref bvcom 0))
	 (add-result (bytevector-compound-s8-ref bvcom 1))
	 (add-result (bytevector-compound-s8-ref bvcom 2))
	 (add-result (bytevector-compound-s8-ref bvcom 3))
	 (add-result (bytevector-compound-s8-ref bvcom 4))
	 (add-result (bytevector-compound-s8-ref bvcom 5))
	 (add-result (bytevector-compound-s8-ref bvcom 6))
	 #t))
    => '(#t (0 1 -2 -3 4 5 -6)))

  (check
      (with-result
       (let ((bvcom (make-bytevector-compound '#vs8(0 1 -2 -3) '#vs8(4) '#vs8(5 -6))))
	 (bytevector-compound-s8-set! bvcom 0 -10)
	 (bytevector-compound-s8-set! bvcom 1 -20)
	 (bytevector-compound-s8-set! bvcom 2 30)
	 (bytevector-compound-s8-set! bvcom 3 40)
	 (bytevector-compound-s8-set! bvcom 4 -50)
	 (bytevector-compound-s8-set! bvcom 5 60)
	 (bytevector-compound-s8-set! bvcom 6 -70)
	 (add-result (bytevector-compound-s8-ref bvcom 0))
	 (add-result (bytevector-compound-s8-ref bvcom 1))
	 (add-result (bytevector-compound-s8-ref bvcom 2))
	 (add-result (bytevector-compound-s8-ref bvcom 3))
	 (add-result (bytevector-compound-s8-ref bvcom 4))
	 (add-result (bytevector-compound-s8-ref bvcom 5))
	 (add-result (bytevector-compound-s8-ref bvcom 6))
	 #t))
    => '(#t (-10 -20 30 40 -50 60 -70)))

  #t)


;;;; done

(check-report)

;;; end of file
