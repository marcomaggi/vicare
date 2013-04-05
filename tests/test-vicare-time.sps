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
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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

  (when #f
    (let ((T (current-time))
	  (N (time-from-now (make-time 123 123000))))
      (check-pretty-print (list T
				(time-second     T)
				(time-nanosecond T)
				(time-gmt-offset T)
				(date-string)))
      (check-pretty-print T)
      (check-pretty-print N)))

  (check
      (time? (current-time))
    => #t)

  (check
      (time? (time-from-now (make-time 123 123000)))
    => #t)

  (check
      (let ((s (time-second (current-time))))
	(or (fixnum? s) (bignum? s)))
    => #t)

  (check
      (let ((s (time-nanosecond (current-time))))
	(or (fixnum? s) (bignum? s)))
    => #t)

  (check
      (fixnum? (time-gmt-offset (current-time)))
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
