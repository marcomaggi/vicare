;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for basic time functions
;;;Date: Thu Nov 15, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012, 2013, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare basic time functions\n")


(parametrise ((check-test-name	'base))

  (when #t
    (let ((T (current-time)))
      (check-pretty-print (list T
				(time-seconds     T)
				(time-nanoseconds T)
				(epoch-time-gmt-offset T)
				(date-string)))
      (check-pretty-print T)))

  (check
      (time? (current-time))
    => #t)

  (check
      (let ((s (time-seconds (current-time))))
	(or (fixnum? s) (bignum? s)))
    => #t)

  (check
      (let ((s (time-nanoseconds (current-time))))
	(or (fixnum? s) (bignum? s)))
    => #t)

  (check
      (fixnum? (epoch-time-gmt-offset (current-time)))
    => #t)

  (check
      (string? (date-string))
    => #t)

  #t)


(parametrise ((check-test-name	'operations))

;;; addition

  (check
      (time-addition (make-time 100 200)
		     (make-time  20  40))
    (=> time=?)
    (make-time 120 240))

  (check
      (time-addition (make-time 100100100 200)
		     (make-time  20020020 40))
    (=> time=?)
    (make-time 120120120 240))

;;; --------------------------------------------------------------------
;;; difference

  (check
      (time-difference (make-time 100 200)
		       (make-time  20  40))
    (=> time=?)
    (make-time 80 160))

;;; --------------------------------------------------------------------
;;; hash

  (check
      (hash (make-time 123 456))
    => 507)

  (check
      (hash (make-time 123 897))
    => 1019)

  (check-for-true
   (non-negative-fixnum? (hash (current-time))))

;;; --------------------------------------------------------------------
;;; equality predicate

  (check-for-true	(equal? (make-time 1 2) (make-time 1 2)))
  (check-for-false	(equal? (make-time 1 2) (make-time 9 2)))
  (check-for-false	(equal? (make-time 1 2) (make-time 1 9)))

  (check-for-true	(let ((T (current-time)))
			  (equal? T T)))
  (check-for-false	(equal? (current-time) (current-time)))

;;; --------------------------------------------------------------------
;;; comparison procedure

  (let ((compar-time (comparison-procedure <time>)))

    (check (compar-time (make-time 1 2) (make-time 1 2))	=> 0)

    (check (compar-time (make-time 1 2) (make-time 1 3))	=> -1)
    (check (compar-time (make-time 1 3) (make-time 1 2))	=> +1)

    (check (compar-time (make-time 2 2) (make-time 1 2))	=> +1)
    (check (compar-time (make-time 1 2) (make-time 2 2))	=> -1)

    (void))

  (let ((compar-time (comparison-procedure <epoch-time>)))

    (check
	(let ((T (current-time)))
	  (compar-time T T))
      => 0)

    (check
	(let* ((T1 (current-time))
	       (T2 (current-time)))
	  (compar-time T1 T2))
      => -1)

    (check
	(let* ((T1 (current-time))
	       (T2 (current-time)))
	  (compar-time T2 T1))
      => +1)

    (void))

  #t)


(parametrise ((check-test-name	'comparison))

;;; equal

  (check
      (time=? (make-time 100 200000)
	      (make-time 100 200000))
    => #t)

  (check
      (time=? (make-time 100 201000)
	      (make-time 100 200000))
    => #f)

  (check
      (time=? (make-time 100100100 200000)
	      (make-time 100100100 200000))
    => #t)

  (check
      (time=? (make-time 100100109 200000)
	      (make-time 100100100 200000))
    => #f)

  (check
      (time=? (make-time 190100100 200000)
	      (make-time 100100100 200000))
    => #f)

;;; --------------------------------------------------------------------
;;; less than

  (check
      (time<? (make-time 100 200000)
	      (make-time 100 200000))
    => #f)

  (check
      (time<? (make-time 100 201000)
	      (make-time 100 200000))
    => #f)

  (check
      (time<? (make-time 100 200000)
	      (make-time 100 201000))
    => #t)

  (check
      (time<? (make-time 100100100 200000)
	      (make-time 100100100 200000))
    => #f)

  (check
      (time<? (make-time 100100109 200000)
	      (make-time 100100100 200000))
    => #f)

  (check
      (time<? (make-time 100100100 200000)
	      (make-time 100100109 200000))
    => #t)

  (check
      (time<? (make-time 190100100 200000)
	      (make-time 100100100 200000))
    => #f)

  (check
      (time<? (make-time 100100100 200000)
	      (make-time 190100100 200000))
    => #t)

;;; --------------------------------------------------------------------
;;; greater than

  (check
      (time>? (make-time 100 200000)
	      (make-time 100 200000))
    => #f)

  (check
      (time>? (make-time 100 209000)
	      (make-time 100 200000))
    => #t)

  (check
      (time>? (make-time 100 200000)
	      (make-time 100 209000))
    => #f)

  (check
      (time>? (make-time 100100100 200000)
	      (make-time 100100100 200000))
    => #f)

  (check
      (time>? (make-time 100100109 200000)
	      (make-time 100100100 200000))
    => #t)

  (check
      (time>? (make-time 100100100 200000)
	      (make-time 100100109 200000))
    => #f)

  (check
      (time>? (make-time 190100100 200000)
	      (make-time 100100100 200000))
    => #t)

  (check
      (time>? (make-time 100100100 200000)
	      (make-time 190100100 200000))
    => #f)

;;; --------------------------------------------------------------------
;;; less than or equal to

  (check
      (time<=? (make-time 100 200000)
	       (make-time 100 200000))
    => #t)

  (check
      (time<=? (make-time 100 201000)
	       (make-time 100 200000))
    => #f)

  (check
      (time<=? (make-time 100 200000)
	       (make-time 100 201000))
    => #t)

  (check
      (time<=? (make-time 100100100 200000)
	       (make-time 100100100 200000))
    => #t)

  (check
      (time<=? (make-time 100100109 200000)
	       (make-time 100100100 200000))
    => #f)

  (check
      (time<=? (make-time 100100100 200000)
	       (make-time 100100109 200000))
    => #t)

  (check
      (time<=? (make-time 190100100 200000)
	       (make-time 100100100 200000))
    => #f)

  (check
      (time<=? (make-time 100100100 200000)
	       (make-time 190100100 200000))
    => #t)

;;; --------------------------------------------------------------------
;;; greater than or equal to

  (check
      (time>=? (make-time 100 200000)
	       (make-time 100 200000))
    => #t)

  (check
      (time>=? (make-time 100 209000)
	       (make-time 100 200000))
    => #t)

  (check
      (time>=? (make-time 100 200000)
	       (make-time 100 209000))
    => #f)

  (check
      (time>=? (make-time 100100100 200000)
	       (make-time 100100100 200000))
    => #t)

  (check
      (time>=? (make-time 100100109 200000)
	       (make-time 100100100 200000))
    => #t)

  (check
      (time>=? (make-time 100100100 200000)
	       (make-time 100100109 200000))
    => #f)

  (check
      (time>=? (make-time 190100100 200000)
	       (make-time 100100100 200000))
    => #t)

  (check
      (time>=? (make-time 100100100 200000)
	       (make-time 190100100 200000))
    => #f)

  #t)


;;;; done

(check-report)

;;; end of file
