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
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(program (test-vicare-time)
  (import (vicare)
    (vicare checks))

  (check-set-mode! 'report-failed)
  (check-display "*** testing Vicare basic time functions\n")


(parametrise ((check-test-name	'time-base))

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

  #t)


(parametrise ((check-test-name	'epoch-time-base))

  (when #t
    (let ((T (current-time)))
      (check-pretty-print (list T
				(time-seconds     T)
				(time-nanoseconds T)
				(time-ratnum T)
				(time-flonum T)
				(time-gmt-offset)
				(date-string)))
      (check-pretty-print T)))

;;; --------------------------------------------------------------------

  (check-for-true	(time? (current-time)))
  (check-for-true	(time? (make-epoch-time (greatest-fixnum) 123)))
  (check-for-true	(epoch-time? (make-epoch-time (greatest-fixnum) 123)))

  (check
      (let ((s (time-seconds (current-time))))
	(or (fixnum? s) (bignum? s)))
    => #t)

  (check
      (let ((s (time-nanoseconds (current-time))))
	(or (fixnum? s) (bignum? s)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((ET (make-epoch-time 0 0)))
	(epoch-time-addition ET (make-time 1 2)))
    => (make-epoch-time 1 2))

  (check
      (let ((ET (make-epoch-time 0 0)))
	(epoch-time-subtraction ET (make-time 1 2)))
    => (make-epoch-time -1 -2))

;;; --------------------------------------------------------------------

  (check
      (fixnum? (time-gmt-offset))
    => #t)

  (void))


(parametrise ((check-test-name	'operations))

;;; type predicate

  (check-for-true	(is-a? (new <time> 1 2) <time>))
  (check-for-false	(is-a? (new <string> #\A) <time>))

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

;;; --------------------------------------------------------------------
;;; hash function

  (check
      (hash (make-time 123 456))
    => 507)

  (check
      (hash (make-time 123 897))
    => 1019)

  (check-for-true
   (non-negative-fixnum? (hash (current-time))))

  #t)


(parametrise ((check-test-name	'arithmetics))

;;; addition

  (check
      (time-addition (make-time 100 200)
		     (make-time  20  40))
    => (make-time 120 240))

  (check
      (time-addition (make-time 100100100 200)
		     (make-time  20020020 40))
    => (make-time 120120120 240))

  (check
      (time-addition (make-time 100 200)
		     (make-time -20 -40))
    => (make-time 80 160))

;;;

  (check
      (time-addition (make-time 100 200))
    => (make-time 100 200))

  (check
      (time-addition (make-time 100 200)
		     (make-time  20  40)
		     (make-time   4   8))
    => (make-time 124 248))

  (check
      (time-addition (make-time 100 200)
		     (make-time  20  40)
		     (make-time   4   8)
		     (make-time 9000 9000))
    => (make-time 9124 9248))

;;; --------------------------------------------------------------------
;;; difference

  (check
      (time-difference (make-time 100 200)
		       (make-time  20  40))
    (=> time=?)
    (make-time 80 160))

;;;

  (check
      (time-difference (make-time 100 200))
    => (make-time -101 999999800))

  (check
      (time-difference (make-time 100 200)
		       (make-time  20  40)
		       (make-time   4   8))
    => (make-time (- 100 20 4) (- 200 40 8)))

  (check
      (time-difference (make-time 100 200)
		       (make-time  20  40)
		       (make-time   4   8)
		       (make-time 9000 9000))
    => (make-time (- 100 20 4 9000) (- 200 40 8 9000)))

  (void))


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

;;; --------------------------------------------------------------------

  (check
      (time-max (make-time 1 2))
    => (make-time 1 2))

  (check
      (time-max (make-time 1 2) (make-time 3 4))
    => (make-time 3 4))

  (check
      (time-max (make-time 1 2) (make-time 3 4) (make-time 5 6))
    => (make-time 5 6))

;;;

  (check
      (time-min (make-time 1 2))
    => (make-time 1 2))

  (check
      (time-min (make-time 1 2) (make-time 3 4))
    => (make-time 1 2))

  (check
      (time-min (make-time 1 2) (make-time 3 4) (make-time 5 6))
    => (make-time 1 2))

  #t)


(parametrise ((check-test-name	'oop))

;;; accessors

  (check
      (let ((T (make-time 1 2)))
	(values (.seconds T)
		(.nanoseconds T)
		(.ratnum T)
		(.flonum T)))
    => 1 2 500000001/500000000 1.000000002)

;;; --------------------------------------------------------------------
;;; arithmetics

  (check
      (.+ (new <time> 100 200) (new <time> 20 40))
    => (new <time> 120 240))

  (check
      (.+ (new <time> 1 2) (new <time> 10 20) (new <time> 100 200))
    => (new <time> 111 222))

;;;

  (check
      (.- (new <time> 100 200))
    => (new <time> -100 -200))

  (check
      (.- (new <time> 1 1))
    => (new <time> -1 -1))

  (check
      (.- (new <time> 100 200) (new <time> 20 40))
    => (new <time> 80 160))

;;; --------------------------------------------------------------------
;;; comparison

  (check-for-true	(.=	(new <time> 100 200000) (new <time> 100 200000)))
  (check-for-true	(.!=	(new <time> 1 2) (new <time> 1 3)))
  (check-for-true	(.<	(new <time> 1 2) (new <time> 1 3)))
  (check-for-false	(.>	(new <time> 1 2) (new <time> 1 3)))
  (check-for-true	(.<=	(new <time> 1 2) (new <time> 1 3)))
  (check-for-false	(.>=	(new <time> 1 2) (new <time> 1 3)))

;;;

  (check
      (.max (new <time> 1 2) (new <time> 1 3))
    => (new <time> 1 3))

  (check
      (.min (new <time> 1 2) (new <time> 1 3))
    => (new <time> 1 2))

  (void))


(parametrise ((check-test-name	'date))

  (check
      (string? (date-string))
    => #t)

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
