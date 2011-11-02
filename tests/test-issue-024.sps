;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for issue 24
;;;Date: Thu Jun 16, 2011
;;;
;;;Abstract
;;;
;;;	The two following operations must return the same result:
;;;
;;;	  (+ 1 -1.1-0.0i)	=> -0.10000000000000009+0.0i
;;;	  (- 1 +1.1+0.0i)	=> -0.10000000000000009+0.0i
;;;                                                    ^
;;;                                                    !
;;;
;;;	but instead they returned:
;;;
;;;	  (+ 1 -1.1-0.0i)	=> -0.10000000000000009-0.0i
;;;	  (- 1 +1.1+0.0i)	=> -0.10000000000000009+0.0i
;;;                                                    ^
;;;                                                    !
;;;
;;;	this is wrong  because R6RS specifies the following  for the "-"
;;;	function:
;;;
;;;	   Implementations that  distinguish -0.0 should  adopt behavior
;;;	   consistent with the following examples:
;;;
;;;		(+  0.0 -0.0)           =>  0.0
;;;		(+ -0.0  0.0)           =>  0.0
;;;		(+  0.0  0.0)           =>  0.0
;;;		(+ -0.0 -0.0)           => -0.0
;;;
;;;	so the "+" operation must be implemented as:
;;;
;;;	   (+ 1 -1.1-0.0i)
;;;	   == (make-rectangular (+ 1 -1.1) (+   0 -0.0))
;;;	   == (make-rectangular (+ 1 -1.1) (+ 0.0 -0.0))
;;;	   == (make-rectangular (+ 1 -1.1) (+ 0.0 -0.0))
;;;	   == (make-rectangular -0.100...  0.0)
;;;
;;;     notice that the step:
;;;
;;;	   (+ 0 -0.0) == (+ 0.0 -0.0)
;;;
;;;	is not  compliant with  R6RS but we  accept it because  it makes
;;;	other  formulas work  correctly, for  example the  following two
;;;	versions of ATAN which should return the same result:
;;;
;;;        (define (atan-one x)
;;;          (* +0.5i
;;;             (- (log (+ 1 (* -1i x)))
;;;                (log (+ 1 (* +1i x))))))
;;;
;;;        (define (atan-two x)
;;;          (* +0.5i
;;;             (- (log (- 1 (* +1i x)))
;;;                (log (+ 1 (* +1i x))))))
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
(import (rnrs)
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing issue 24, wrong unary minus sign handling for complex numbers\n")


;;;; helpers

(define (quasi=? X Y)
  (define (sgn x)
    (cond ((zero? x)		0)
	  ((positive? x)	+1)
	  ((negative? x)	-1)
	  (else			#f)))
  (define epsilon 1e-9)
  (define (%zero? x)
    (and (exact? x) (zero? x)))
  (cond ((and (real? X) (real? Y))
	 (or (and (nan? X) (nan? Y))
	     (and (infinite? X) (infinite? Y) (= (sgn X) (sgn Y)))
	     (and (eqv? (sgn X) (sgn Y))
		  (= X Y)
		  #;(< (abs (- X Y)) epsilon)
		  )))
	((and (complex? X) (complex? Y))
	 (and (quasi=? (real-part X) (real-part Y))
	      (quasi=? (imag-part X) (imag-part Y))))
	((complex? X)
	 (and (%zero? (imag-part X))
	      (quasi=? (real-part X) Y)))
	((complex? Y)
	 (and (%zero? (imag-part Y))
	      (quasi=? (real-part Y) X)))
	(else
	 (assertion-violation 'quasi=? "invalid arguments" X Y))))


;;;; tests

(check (- +0.0)			(=> quasi=?) -0.0)
(check (- -0.0)			(=> quasi=?) +0.0)

(check (- +0.0i)		(=> quasi=?) -0.0i)
(check (- -0.0i)		(=> quasi=?) +0.0i)

(check (- +0.0+1.0i)		(=> quasi=?) -0.0-1.0i)
(check (- +0.0-1.0i)		(=> quasi=?) -0.0+1.0i)
(check (- -0.0+1.0i)		(=> quasi=?) +0.0-1.0i)
(check (- -0.0-1.0i)		(=> quasi=?) +0.0+1.0i)

(check (- +0.0+13.1i)		(=> quasi=?) -0.0-13.1i)
(check (- +0.0-13.1i)		(=> quasi=?) +0.0+13.1i)
(check (- -0.0+13.1i)		(=> quasi=?) +0.0-13.1i)
(check (- -0.0-13.1i)		(=> quasi=?) +0.0+13.1i)

(check (- +1.0+0.0i)		(=> quasi=?) -1.0-0.0i)
(check (- +1.0-0.0i)		(=> quasi=?) -1.0+0.0i)
(check (- -1.0+0.0i)		(=> quasi=?) +1.0-0.0i)
(check (- -1.0-0.0i)		(=> quasi=?) +1.0+0.0i)

(check (- +13.1+0.0i)		(=> quasi=?) -13.1-0.0i)
(check (- +13.1-0.0i)		(=> quasi=?) -13.1+0.0i)
(check (- -13.1+0.0i)		(=> quasi=?) +13.1-0.0i)
(check (- -13.1-0.0i)		(=> quasi=?) +13.1+0.0i)

;;; --------------------------------------------------------------------

(check (- +2.3+1.0i)		(=> quasi=?) -2.3-1.0i)
(check (- +2.3-1.0i)		(=> quasi=?) -2.3+1.0i)
(check (- -2.3+1.0i)		(=> quasi=?) +2.3-1.0i)
(check (- -2.3-1.0i)		(=> quasi=?) +2.3+1.0i)

(check (- +2.3+13.1i)		(=> quasi=?) -2.3-13.1i)
(check (- +2.3-13.1i)		(=> quasi=?) -2.3+13.1i)
(check (- -2.3+13.1i)		(=> quasi=?) +2.3-13.1i)
(check (- -2.3-13.1i)		(=> quasi=?) +2.3+13.1i)

(check (- +1.0+2.3i)		(=> quasi=?) -1.0-2.3i)
(check (- +1.0-2.3i)		(=> quasi=?) -1.0+2.3i)
(check (- -1.0+2.3i)		(=> quasi=?) +1.0-2.3i)
(check (- -1.0-2.3i)		(=> quasi=?) +1.0+2.3i)

(check (- +13.1+2.3i)		(=> quasi=?) -13.1-2.3i)
(check (- +13.1-2.3i)		(=> quasi=?) -13.1+2.3i)
(check (- -13.1+2.3i)		(=> quasi=?) +13.1-2.3i)
(check (- -13.1-2.3i)		(=> quasi=?) +13.1+2.3i)


(let ()
  (define (one x)
    (* +0.5i
       (- (log (+ 1 (* -1i x)))
	  (log (+ 1 (* +1i x))))))

  (define (two x)
    (* +0.5i
       (- (log (- 1 (* +1i x)))
	  (log (+ 1 (* +1i x))))))

  (check
      (one 0.0-1.1i)
    (=> quasi=?) (two 0.0-1.1i))

  (check
      (+ 1 (* -1i 0.0-1.1i))
    (=> quasi=?) (- 1 (* +1i 0.0-1.1i)))

  (check
      (* -1i 0.0-1.1i)
    (=> quasi=?) (- (* +1i 0.0-1.1i)))


  (let ((x 0.0-1.1i))
    (check
	(log (- 1 (* +1i x)))
      (=> quasi=?) (log (+ 1 (* -1i x)))))

  (check
      (log (+ 1 -1.1-0.0i))
    (=> quasi=?) (log (- 1 +1.1+0.0i)))

  (check
      (+ 1 -1.1-0.0i)
    (=> quasi=?) (- 1 +1.1+0.0i))

  (let ((A (+ 1 -1.1-0.0i))
	(B (- 1 +1.1+0.0i)))
    (check
	A
      (=> quasi=?) B)
    (check
	(log A)
      (=> quasi=?) (log B)))

  #t)


;;;; done

(check-report)

;;; end of file
