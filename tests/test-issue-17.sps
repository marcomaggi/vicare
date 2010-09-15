;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for issue 17
;;;Date: Thu Sep  9, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (rnrs)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing issue 17\n")

(define epsilon 1e-6)

(define (eq=? a b)
  ;;This is not  the best definition of equality  between floating point
  ;;numbers.
  ;;
  (let ((ra (real-part a))
	(rb (real-part b))
	(ia (imag-part a))
	(ib (imag-part b)))
    (and (or (and (nan? ra) (nan? rb))
	     (< (abs (- ra rb)) epsilon))
	 (or (and (nan? ia) (nan? ib))
	     (< (abs (- ia ib)) epsilon)))))

(define bignum0 (expt 2 32))


;;;; fixnum exponent

;;; fixnum base

(check (expt 2 3)		=> 8)
(check (expt 2 -3)		=> 1/8)

(check (expt -2 3)		=> -8)
(check (expt -2 -3)		=> -1/8)

(check (expt 0 3)		=> 0)
(check (expt 0 -3)		=> 0)

(check (expt 3 0)		=> 1)
(check (expt -3 0)		=> 1)

;;; --------------------------------------------------------------------
;;; rational base

(check (expt 2/3 3)		=> 8/27)
(check (expt 2/3 -3)		=> 27/8)

(check (expt -2/3 3)		=> -8/27)
(check (expt -2/3 -3)		=> -27/8)

(check (expt 3/4 0)		=> 1)
(check (expt -3/4 0)		=> 1)

;;; --------------------------------------------------------------------
;;; bignum base

(check (expt bignum0 3)		=> 79228162514264337593543950336)
(check (expt bignum0 -3)	=> 1/79228162514264337593543950336)

(check (expt (- bignum0) 3)	=> -79228162514264337593543950336)
(check (expt (- bignum0) -3)	=> -1/79228162514264337593543950336)

(check (expt bignum0 0)		=> 1)
(check (expt (- bignum0) 0)	=> 1)

;;; --------------------------------------------------------------------
;;; flonum base

(check (expt 2. 3)		=> 8.)
(check (expt 2. -3)		=> 0.125)

(check (expt -2. 3)		=> -8.0)
(check (expt -2. -3)		=> -0.125)

(check (expt 0. 3)		=> 0.)
(check (expt 0. -3)		=> +inf.0)

(check (expt 3. 0)		=> 1.)
(check (expt -3. 0)		=> 1.)

(check (expt 2.3 3)		(=> eq=?) 12.166999999999998)
(check (expt 2.3 -3)		(=> eq=?) 0.08218952905399854)

(check (expt -2.3 3)		(=> eq=?) -12.166999999999998)
(check (expt -2.3 -3)		(=> eq=?) -0.08218952905399854)

(check (expt 3.4 0)		=> 1.)
(check (expt -3.4 0)		=> 1.)

(check (expt +inf.0 0)		=> 1.)
(check (expt +inf.0 1)		=> +inf.0)
(check (expt +inf.0 2)		=> +inf.0)
(check (expt +inf.0 3)		=> +inf.0)
(check (expt +inf.0 -1)		=> 0.0)
(check (expt +inf.0 -2)		=> 0.0)
(check (expt +inf.0 -3)		=> 0.0)

(check (expt -inf.0 0)		=> 1.)
(check (expt -inf.0 1)		=> -inf.0)
(check (expt -inf.0 2)		=> +inf.0)
(check (expt -inf.0 3)		=> -inf.0)
(check (expt -inf.0 4)		=> +inf.0)
(check (expt -inf.0 -1)		=> -0.0)
(check (expt -inf.0 -2)		=> 0.0)
(check (expt -inf.0 -3)		=> -0.0)
(check (expt -inf.0 -4)		=> 0.0)

(check (nan? (expt +nan.0 2))	=> #t)
(check (nan? (expt +nan.0 -2))	=> #t)
(check (nan? (expt +nan.0 0))	=> #t)

;;; --------------------------------------------------------------------
;;; complex fixnum base

(check (expt 1+2i 2)		=> -3+4i)
(check (expt 3+4i 5)		=> -237-3116i)

(check (expt +2i 2)		=> -4)
(check (expt +4i 5)		=> +1024i)
(check (expt -2i 2)		=> -4)
(check (expt -4i 5)		=> -1024i)

(check (expt +2i -2)		=> -1/4)
(check (expt +4i -5)		=> -1/1024i)
(check (expt -2i -2)		=> -1/4)
(check (expt -4i -5)		=> +1/1024i)

;;; --------------------------------------------------------------------
;;; complex rational base

(check (expt 1/2+2/3i 2)	=> -7/36+2/3i)
(check (expt 3/4+4/5i 5)	=> -118617/128000-257339/200000i)

(check (expt +2/3i 2)		=> -4/9)
(check (expt +4/5i 5)		=> 0+1024/3125i)
(check (expt -2/3i 2)		=> -4/9)
(check (expt -4/5i 5)		=> 0-1024/3125i)

(check (expt +2/3i -2)		=> -9/4)
(check (expt +4/5i -5)		=> 0-3125/1024i)
(check (expt -2/3i -2)		=> -9/4)
(check (expt -4/5i -5)		=> 0+3125/1024i)

;;; --------------------------------------------------------------------
;;; complex bignum base

(check
    (expt (make-rectangular bignum0 bignum0) 3)
  => -158456325028528675187087900672+158456325028528675187087900672i)

(check
    (expt (make-rectangular (- bignum0) bignum0) 3)
  => 158456325028528675187087900672+158456325028528675187087900672i)

(check
    (expt (make-rectangular bignum0 (- bignum0)) 3)
  => -158456325028528675187087900672-158456325028528675187087900672i)

(check
    (expt (make-rectangular (- bignum0) (- bignum0)) 3)
  => 158456325028528675187087900672-158456325028528675187087900672i)

;;

(check
    (expt (make-rectangular bignum0 bignum0) -3)
  => -1/316912650057057350374175801344-1/316912650057057350374175801344i)

(check
    (expt (make-rectangular (- bignum0) bignum0) -3)
  => 1/316912650057057350374175801344-1/316912650057057350374175801344i)

(check
    (expt (make-rectangular bignum0 (- bignum0)) -3)
  => -1/316912650057057350374175801344+1/316912650057057350374175801344i)

(check
    (expt (make-rectangular (- bignum0) (- bignum0)) -3)
  => 1/316912650057057350374175801344+1/316912650057057350374175801344i)

;;

(check
    (expt (make-rectangular 0 bignum0) 3)
  => -79228162514264337593543950336i)

(check
    (expt (make-rectangular 0 bignum0) -3)
  => +1/79228162514264337593543950336i)

(check
    (expt (make-rectangular 0 (- bignum0)) 3)
  => +79228162514264337593543950336i)

(check
    (expt (make-rectangular 0 (- bignum0)) -3)
  => -1/79228162514264337593543950336i)

;;; --------------------------------------------------------------------
;;; complex flonum base

(check (expt 1.+2.i 2)		(=> eq=?) -3.+4.i)
(check (expt 3.+4.i 5)		(=> eq=?) -237.-3116.i)

(check (expt +2.i 2)		(=> eq=?) -4.+0.i)
(check (expt +4.i 5)		(=> eq=?) +1024.i)
(check (expt -2.i 2)		(=> eq=?) -4.+0.i)
(check (expt -4.i 5)		(=> eq=?) -1024.i)

(check (expt +2.i -2)		(=> eq=?) (inexact -1/4))
(check (expt +4.i -5)		(=> eq=?) (inexact -1/1024i))
(check (expt -2.i -2)		(=> eq=?) (inexact -1/4))
(check (expt -4.i -5)		(=> eq=?) (inexact +1/1024i))

(check (expt +inf.0+2.i 2)	=> +inf.0+0.0i)
(check (expt +inf.0-2.i 2)	=> +inf.0+0.0i)

(check (expt +inf.0+2.i 0)	=> 1.)
(check (expt +2.+inf.0i 0)	=> 1.)

(check (nan? (expt +nan.0+2.i 2))	=> #t)
(check (nan? (expt +2.+nan.0i 2))	=> #t)
(check (nan? (expt +nan.0+2.i 0))	=> #t)
(check (nan? (expt +2.+nan.0i 0))	=> #t)

;;; --------------------------------------------------------------------
;;; related checks

(check (magnitude +inf.0+2.i) => +inf.0)
(check (log +inf.0) => +inf.0)
(check (angle +inf.0+2.i) => 0.0)
(check (log +inf.0+2.i) => +inf.0+0.0i)


;;;; rational exponent




;;;; bignum exponent

;;; fixnum base

(check (expt  0 (+ 3 bignum0))	=> 0)
(check (expt  1 (+ 3 bignum0))	=> 1)
(check (expt  1 (- bignum0))	=> 1)
(check (expt -1 (+ 3 bignum0))	=> -1) ;odd positive exponent
(check (expt -1 (+ 4 bignum0))	=> +1) ;even positive exponent
(check (expt -1 (- -1 bignum0))	=> -1) ;odd negative exponent
(check (expt -1 (- bignum0))	=> 1) ;even negative exponent

(check
    (guard (E ((assertion-violation? E)
	       #t)
	      (else #f))
      ;;result is too big to compute
      (expt 2 bignum0))
  => #t)

;;When the  exponent is a bignum:  EXPT computes the result  only if the
;;base is 0, +1, -1 or NaN.
;;
(check
    (expt +nan.0 (expt 2 32))
  => +nan.0)


;;;; flonum exponent

;;; fixnum base

(check (expt 2 +nan.0)		=> +nan.0)
(check (expt 2 +inf.0)		=> +inf.0)
(check (expt 2 -inf.0)		=> 0.)

(check (expt 2 3.)		=> 8.)
(check (expt 2 -3.)		=> (inexact 1/8))

(check (expt -2 3.)		=> -8.)
(check (expt -2 -3.)		=> (inexact -1/8))

(check (expt 0 3.)		=> 0.)
(check (expt 0 -3.)		=> +inf.0)

(check (expt 3 0.)		=> 1.)
(check (expt -3 0.)		=> 1.)

;;; --------------------------------------------------------------------
;;; rational base

(check (expt 2/3 3.)		=> (inexact 8/27))
(check (expt 2/3 -3.)		=> (inexact 27/8))

(check (expt -2/3 3.)		=> (inexact -8/27))
(check (expt -2/3 -3.)		=> (inexact -27/8))

(check (expt 3/4 0.)		=> 1.)
(check (expt -3/4 0.)		=> 1.)

;;; --------------------------------------------------------------------
;;; bignum base

(check (expt bignum0 3.)	=> (inexact 79228162514264337593543950336))
(check (expt bignum0 -3.)	=> (inexact 1/79228162514264337593543950336))

(check (expt (- bignum0) 3.)	=> (inexact -79228162514264337593543950336))
(check (expt (- bignum0) -3.)	=> (inexact -1/79228162514264337593543950336))

(check (expt bignum0 0.)	=> 1.)
(check (expt (- bignum0) 0.)	=> 1.)

;;; --------------------------------------------------------------------
;;; flonum base

(check (expt 0.0 0.0)		=> 1.0)
(check (expt 0.0 1.0)		=> 0.0)

(check (expt 2. 3.)		=> 8.)
(check (expt 2. -3.)		=> 0.125)

(check (expt -2. 3.)		=> -8.0)
(check (expt -2. -3.)		=> -0.125)

(check (expt 0. 3.)		=> 0.)
(check (expt 0. -3.)		=> +inf.0)

(check (expt 3. 0.)		=> 1.)
(check (expt -3. 0.)		=> 1.)

(check (expt 2.3 3.)		(=> eq=?) 12.166999999999998)
(check (expt 2.3 -3.)		(=> eq=?) 0.08218952905399854)

(check (expt -2.3 3.)		(=> eq=?) -12.166999999999998)
(check (expt -2.3 -3.)		(=> eq=?) -0.08218952905399854)

(check (expt 3.4 0.)		=> 1.)
(check (expt -3.4 0.)		=> 1.)

(check (expt +inf.0 0.)		=> 1.)
(check (expt +inf.0 1.)		=> +inf.0)
(check (expt +inf.0 2.)		=> +inf.0)
(check (expt +inf.0 3.)		=> +inf.0)
(check (expt +inf.0 -1.)	=> 0.0)
(check (expt +inf.0 -2.)	=> 0.0)
(check (expt +inf.0 -3.)	=> 0.0)

(check (expt -inf.0 0.)		=> 1.)
(check (expt -inf.0 1.)		=> -inf.0)
(check (expt -inf.0 2.)		=> +inf.0)
(check (expt -inf.0 3.)		=> -inf.0)
(check (expt -inf.0 4.)		=> +inf.0)
(check (expt -inf.0 -1.)	=> -0.0)
(check (expt -inf.0 -2.)	=> 0.0)
(check (expt -inf.0 -3.)	=> -0.0)
(check (expt -inf.0 -4.)	=> 0.0)

(check (nan? (expt +nan.0 2.))	=> #t)
(check (nan? (expt +nan.0 -2.))	=> #t)
(check (nan? (expt +nan.0 0.))	=> #t)

;;; --------------------------------------------------------------------
;;; complex fixnum base

(check (expt 1+2i 2.)		(=> eq=?) (inexact -3+4i))
(check (expt 3+4i 5.)		(=> eq=?) (inexact -237-3116i))

(check (expt +2i 2.)		(=> eq=?) (inexact -4))
(check (expt +4i 5.)		(=> eq=?) (inexact +1024i))
(check (expt -2i 2.)		(=> eq=?) (inexact -4))
(check (expt -4i 5.)		(=> eq=?) (inexact -1024i))

(check (expt +2i -2.)		(=> eq=?) (inexact -1/4))
(check (expt +4i -5.)		(=> eq=?) (inexact -1/1024i))
(check (expt -2i -2.)		(=> eq=?) (inexact -1/4))
(check (expt -4i -5.)		(=> eq=?) (inexact +1/1024i))

;;; --------------------------------------------------------------------
;;; complex rational base

(check (expt 1/2+2/3i 2.)	(=> eq=?) (inexact -7/36+2/3i))
(check (expt 3/4+4/5i 5.)	(=> eq=?) (inexact -118617/128000-257339/200000i))

(check (expt +2/3i 2.)		(=> eq=?) (inexact -4/9))
(check (expt +4/5i 5.)		(=> eq=?) (inexact 0+1024/3125i))
(check (expt -2/3i 2.)		(=> eq=?) (inexact -4/9))
(check (expt -4/5i 5.)		(=> eq=?) (inexact 0-1024/3125i))

(check (expt +2/3i -2.)		(=> eq=?) (inexact -9/4))
(check (expt +4/5i -5.)		(=> eq=?) (inexact 0-3125/1024i))
(check (expt -2/3i -2.)		(=> eq=?) (inexact -9/4))
(check (expt -4/5i -5.)		(=> eq=?) (inexact 0+3125/1024i))

;;; --------------------------------------------------------------------
;;; complex bignum base

(check
    (expt (make-rectangular bignum0 bignum0) 3)
  (=> eq=?) (inexact -158456325028528675187087900672+158456325028528675187087900672i))

(check
    (expt (make-rectangular (- bignum0) bignum0) 3)
  (=> eq=?) (inexact 158456325028528675187087900672+158456325028528675187087900672i))

(check
    (expt (make-rectangular bignum0 (- bignum0)) 3)
  (=> eq=?) (inexact -158456325028528675187087900672-158456325028528675187087900672i))

(check
    (expt (make-rectangular (- bignum0) (- bignum0)) 3)
  (=> eq=?) (inexact 158456325028528675187087900672-158456325028528675187087900672i))

;;

(check
    (expt (make-rectangular bignum0 bignum0) -3)
  (=> eq=?) (inexact -1/316912650057057350374175801344-1/316912650057057350374175801344i))

(check
    (expt (make-rectangular (- bignum0) bignum0) -3)
  (=> eq=?) (inexact 1/316912650057057350374175801344-1/316912650057057350374175801344i))

(check
    (expt (make-rectangular bignum0 (- bignum0)) -3)
  (=> eq=?) (inexact -1/316912650057057350374175801344+1/316912650057057350374175801344i))

(check
    (expt (make-rectangular (- bignum0) (- bignum0)) -3)
  (=> eq=?) (inexact 1/316912650057057350374175801344+1/316912650057057350374175801344i))

;;

(check
    (expt (make-rectangular 0 bignum0) 3)
  (=> eq=?) (inexact -79228162514264337593543950336i))

(check
    (expt (make-rectangular 0 bignum0) -3)
  (=> eq=?) (inexact +1/79228162514264337593543950336i))

(check
    (expt (make-rectangular 0 (- bignum0)) 3)
  (=> eq=?) (inexact +79228162514264337593543950336i))

(check
    (expt (make-rectangular 0 (- bignum0)) -3)
  (=> eq=?) (inexact -1/79228162514264337593543950336i))

;;; --------------------------------------------------------------------
;;; complex flonum base

(check (expt 1.+2.i 2.)		(=> eq=?) -3.+4.i)
(check (expt 3.+4.i 5.)		(=> eq=?) -237.-3116.i)

(check (expt +2.i 2.)		(=> eq=?) -4.+0.i)
(check (expt +4.i 5.)		(=> eq=?) +1024.i)
(check (expt -2.i 2.)		(=> eq=?) -4.+0.i)
(check (expt -4.i 5.)		(=> eq=?) -1024.i)

(check (expt +2.i -2.)		(=> eq=?) (inexact -1/4))
(check (expt +4.i -5.)		(=> eq=?) (inexact -1/1024i))
(check (expt -2.i -2.)		(=> eq=?) (inexact -1/4))
(check (expt -4.i -5.)		(=> eq=?) (inexact +1/1024i))

(check (expt +inf.0+2.i 2.)	=> +inf.0+0.0i)
(check (expt +inf.0-2.i 2.)	=> +inf.0+0.0i)

(check (expt +inf.0+2.i 0.)	(=> eq=?) +nan.0+0.0i)
(check (expt +2.+inf.0i 0.)	(=> eq=?) +nan.0+0.0i)

(check (expt +nan.0+2.i 2.)	(=> eq=?) +nan.0+nan.0i)
(check (expt +nan.0+2.i 2.)	(=> eq=?) +nan.0+nan.0i)
(check (expt +2.+nan.0i 2.)	(=> eq=?) +nan.0+nan.0i)
(check (expt +nan.0+2.i 0.)	(=> eq=?) +nan.0+nan.0i)
(check (expt +2.+nan.0i 0.)	(=> eq=?) +nan.0+nan.0i)

(check
    (let ((r (expt +i +inf.0)))
      (list (nan? (real-part r))
	    (nan? (imag-part r))))
  => '(#t #t))


;;;; complex exponent

;;; fixnum base

(check (expt 2 +3+4i)		(=> eq=?) -7.461496614688567+2.8854927255134477i)
(check (expt 2 +3-4i)		(=> eq=?) -7.461496614688567-2.8854927255134477i)
(check (expt 2 -3+4i)		(=> eq=?) -0.11658588460450892+0.045085823836147634i)
(check (expt 2 -3-4i)		(=> eq=?) -0.11658588460450892-0.045085823836147634i)

(check (expt -2 +3+4i)		(=> eq=?) 2.6020793185113498e-5-1.0062701000215989e-5i)
(check (expt -2 +3-4i)		(=> eq=?) 2139593.9522266407+827418.82808724i)
(check (expt -2 -3+4i)		(=> eq=?) 4.0657489351739836e-7-1.5722970312837445e-7i)
(check (expt -2 -3-4i)		(=> eq=?) 33431.15550354123+12928.419188863143i)

(check (expt 0 +3+4i)		=> 0)
(check (expt 0 +3-4i)		=> 0)
(check (expt 0 -3+4i)		=> 0)
(check (expt 0 -3-4i)		=> 0)

(check
    (expt 2 (make-rectangular bignum0 bignum0))
  => -inf.0+inf.0i)

;;; --------------------------------------------------------------------
;;; rational base

(check (expt 2/3 3+4i)		(=> eq=?) -0.015123530831611505-0.29591007758115956i)
(check (expt 2/3 -3+4i)		(=> eq=?) -0.1722664683788249-3.370600727447897i)

(check (expt -2/3 3+4i)		(=> eq=?) 5.274092964451166e-8+1.0319397471778688e-6i)
(check (expt -2/3 -3+4i)	(=> eq=?) 6.007521517320171e-7+1.1754438682697904e-5i)

(check (expt +3/4 0+0i)		(=> eq=?) 1)
(check (expt -3/4 0+0i)		(=> eq=?) 1)
(check (expt +3/4 0.+0.i)	(=> eq=?) 1.+0.i)
(check (expt -3/4 0.+0.i)	(=> eq=?) 1.+0.i)

;;; --------------------------------------------------------------------
;;; bignum base

(check (expt bignum0 3+4i)	(=> eq=?) 5.752313843061938e28+5.448110021354569e28i)
(check (expt bignum0 -3+4i)	(=> eq=?) 9.16396465367731e-30+8.679340005979716e-30i)

(check (expt (- bignum0) 3+4i)	(=> eq=?) -2.0060287711117325e23-1.899942483875641e23i)
(check (expt (- bignum0) -3+4i)	(=> eq=?) 0.0+0.0i)

(check (expt    bignum0  0+0i)		=> 1)
(check (expt (- bignum0) 0+0i)		=> 1)
(check (expt    bignum0  0.+0.i)	=> 1.+0.i)
(check (expt (- bignum0) 0.+0.i)	=> 1.+0.i)

;;; --------------------------------------------------------------------
;;; flonum base

(check (expt 2. 3+4i)		(=> eq=?) -7.461496614688567+2.8854927255134477i)
(check (expt 2. -3+4i)		(=> eq=?) -0.11658588460450892+0.045085823836147634i)

(check (expt -2. 3+4i)		(=> eq=?) 2.6020793185113498e-5-1.0062701000215989e-5i)
(check (expt -2. -3+4i)		(=> eq=?) 4.0657489351739836e-7-1.5722970312837445e-7i)

(check (expt 0. 3+4i)		(=> eq=?) 0.0)
(check (expt 0. -3+4i)		(=> eq=?) 0.0)

(check (expt  3. 0+0i)		(=> eq=?) 1)
(check (expt -3. 0+0i)		(=> eq=?) 1)
(check (expt  3. 0.+0.i)	(=> eq=?) 1.)
(check (expt -3. 0.+0.i)	(=> eq=?) 1.)

(check (expt 2.3 3+4i)		(=> eq=?) -11.947944783864344-2.298369953190461i)
(check (expt 2.3 -3+4i)		(=> eq=?) -0.08070978507018896-0.015525761818409197i)

(check (expt -2.3 3+4i)		(=> eq=?) 4.16665739144165e-5+8.015202887999153e-6i)
(check (expt -2.3 -3+4i)	(=> eq=?) 2.814626520357944e-7+5.414364680175077e-8i)

(check (expt +inf.0 0.+0.i)	(=> eq=?) +nan.0+nan.0i)
(check (expt +inf.0 1+4i)	(=> eq=?) +nan.0+nan.0i)
(check (expt +inf.0 2+4i)	(=> eq=?) +nan.0+nan.0i)
(check (expt +inf.0 3+4i)	(=> eq=?) +nan.0+nan.0i)
(check (expt +inf.0 -1+4i)	(=> eq=?) +nan.0+nan.0i)
(check (expt +inf.0 -2+4i)	(=> eq=?) +nan.0+nan.0i)
(check (expt +inf.0 -3+4i)	(=> eq=?) +nan.0+nan.0i)

(check (expt -inf.0 0.+0.i)	(=> eq=?) +nan.0+nan.0i)
(check (expt -inf.0 1+4i)	(=> eq=?) +nan.0+nan.0i)
(check (expt -inf.0 2+4i)	(=> eq=?) +nan.0+nan.0i)
(check (expt -inf.0 3+4i)	(=> eq=?) +nan.0+nan.0i)
(check (expt -inf.0 -1+4i)	(=> eq=?) +nan.0+nan.0i)
(check (expt -inf.0 -2+4i)	(=> eq=?) +nan.0+nan.0i)
(check (expt -inf.0 -3+4i)	(=> eq=?) +nan.0+nan.0i)

(check (expt +nan.0 2+4i)	(=> eq=?) +nan.0+nan.0i)
(check (expt +nan.0 -2+4i)	(=> eq=?) +nan.0+nan.0i)
(check (expt +nan.0 0.+0.i)	(=> eq=?) +nan.0+nan.0i)

(check (expt 1. +nan.0+nan.0i)	(=> eq=?) +nan.0+nan.0i)

(check (expt 1. +inf.0+2.0i)	(=> eq=?) +nan.0+0.0i)
(check (expt 1. -inf.0+2.0i)	(=> eq=?) +nan.0+0.0i)
(check (expt 1. +2.0+inf.0i)	(=> eq=?) +nan.0+nan.0i)
(check (expt 1. -2.0+inf.0i)	(=> eq=?) +nan.0+nan.0i)

(check (expt 0. +inf.0+2.0i)	(=> eq=?) 0.0+0.0i)
(check (expt 0. -inf.0+2.0i)	(=> eq=?) 0.0+0.0i)

(check (expt 0. 2.0+inf.0i)	(=> eq=?) 0.0+0.0i)
(check (expt 0. 2.0-inf.0i)	(=> eq=?) 0.0+0.0i)
(check (expt 0. -2.0+inf.0i)	(=> eq=?) 0.0+0.0i)
(check (expt 0. -2.0-inf.0i)	(=> eq=?) 0.0+0.0i)

;;; --------------------------------------------------------------------
;;; complex fixnum base

(check (expt 1+2i 2+4i)		(=> eq=?) 0.03937155597696145-0.04481901904875275i)
(check (expt 3+4i 5+4i)		(=> eq=?) 6.015104118252687-76.31826646606693i)

(check (expt +2i 2+4i)		(=> eq=?) 0.0069669588103819695-0.0026942462088281804i)
(check (expt +4i 5+4i)		(=> eq=?) 1.2865989738437773+1.4147107757447617i)
(check (expt -2i 2+4i)		(=> eq=?) 1997.7845874460047-772.5786382949307i)
(check (expt -4i 5+4i)		(=> eq=?) -368933.94522997335-405670.1726533845i)

(check (expt +2i -2+4i)		(=> eq=?) 4.354349256488729e-4-1.683903880517613e-4i)
(check (expt +4i -5+4i)		(=> eq=?) -1.2269963968694439e-6-1.3491733319709417e-6i)
(check (expt -2i -2+4i)		(=> eq=?) 124.86153671537525-48.28616489343312i)
(check (expt -4i -5+4i)		(=> eq=?) 0.351842828016256+0.38687722459162105i)

;;; --------------------------------------------------------------------
;;; complex rational base

(check (expt 1/2+2/3i 2+4i)	(=> eq=?) 0.0073305954317033455+0.015351796096997978i)
(check (expt 3/4+4/5i 5+4i)	(=> eq=?) -0.015213795324594232-0.058277573555822575i)

(check (expt +2/3i 2+4i)	(=> eq=?) 4.2363491593881786e-5+8.288926854271252e-4i)
(check (expt +4/5i 5+4i)	(=> eq=?) 4.7649845184515857e-4+3.83926763375514e-4i)
(check (expt -2/3i 2+4i)	(=> eq=?) 12.147786843599112+237.68606599559493i)
(check (expt -4/5i 5+4i)	(=> eq=?) -136.63655677418146-110.09150354623384i)

(check (expt +2/3i -2+4i)	(=> eq=?) 2.144651761940257e-4+0.0041962692199748255i)
(check (expt +4/5i -5+4i)	(=> eq=?) -0.004437737649727223-0.003575596617306714i)
(check (expt -2/3i -2+4i)	(=> eq=?) 61.498170895720854+1203.2857091027004i)
(check (expt -4/5i -5+4i)	(=> eq=?) 1272.5270984152467+1025.3070252596747i)

;;; --------------------------------------------------------------------
;;; complex bignum base

(check
    (expt (make-rectangular bignum0 bignum0) 3+4i)
  (=> eq=?) -2.0343701122857706e27-9.467753817128583e27i)

(check
    (expt (make-rectangular (- bignum0) bignum0) 3+4i)
  (=> eq=?) -1.768048805139746e25+3.79906967979209e24i)

(check
    (expt (make-rectangular bignum0 (- bignum0)) 3+4i)
  (=> eq=?) 5.069903165635117e30-1.0893882193780784e30i)

(check
    (expt (make-rectangular (- bignum0) (- bignum0)) 3+4i)
  (=> eq=?) 5.833583011039331e32+2.7148908395161755e33i)

;;

(check
    (expt (make-rectangular bignum0 bignum0) -3+4i)
  (=> eq=?) 1.8853752528326245e-31-4.051173212665355e-32i)

(check
    (expt (make-rectangular (- bignum0) bignum0) -3+4i)
  (=> eq=?) 7.565333970882006e-35+3.5208303124444235e-34i)

(check
    (expt (make-rectangular bignum0 (- bignum0)) -3+4i)
  (=> eq=?) -2.1693694504676422e-29-1.0096027154247735e-28i)

(check
    (expt (make-rectangular (- bignum0) (- bignum0)) -3+4i)
  (=> eq=?) -5.406338295051056e-26+1.1616792384757687e-26i)

;;

(check
    (expt (make-rectangular 0 bignum0) 3+4i)
  (=> eq=?) 1.0174033460924027e26-1.0742116676729287e26i)

(check
    (expt (make-rectangular 0 bignum0) -3+4i)
  (=> eq=?) -1.6208170410189345e-32+1.7113179186138704e-32i)

(check
    (expt (make-rectangular 0 (- bignum0)) 3+4i)
  (=> eq=?) -2.9174174548162036e31+3.0803160629192304e31i)

(check
    (expt (make-rectangular 0 (- bignum0)) -3+4i)
  (=> eq=?) 4.647714148664389e-27-4.907226603568098e-27i)


;;; --------------------------------------------------------------------
;;; complex flonum base

(check (expt 1.+2.i 2.+4.i)	(=> eq=?) 0.03937155597696145-0.04481901904875275i)
(check (expt 3.+4.i 5.+4.i)	(=> eq=?) 6.015104118252687-76.31826646606693i)

(check (expt +2.i 2.+4.i)	(=> eq=?) 0.0069669588103819695-0.0026942462088281804i)
(check (expt +4.i 5.+4.i)	(=> eq=?) 1.2865989738437773+1.4147107757447617i)
(check (expt -2.i 2.+4.i)	(=> eq=?) 1997.7845874460047-772.5786382949307i)
(check (expt -4.i 5.+4.i)	(=> eq=?) -368933.94522997335-405670.1726533845i)

(check (expt +2.i -2.+4.i)	(=> eq=?) 4.354349256488729e-4-1.683903880517613e-4i)
(check (expt +4.i -5.+4.i)	(=> eq=?) -1.2269963968694439e-6-1.3491733319709417e-6i)
(check (expt -2.i -2.+4.i)	(=> eq=?) 124.86153671537525-48.28616489343312i)
(check (expt -4.i -5.+4.i)	(=> eq=?) 0.351842828016256+0.38687722459162105i)

(check (expt +inf.0+2.i 2.+4.i)	(=> eq=?) +nan.0+nan.0i)
(check (expt +inf.0-2.i 2.+4.i)	(=> eq=?) +nan.0+nan.0i)

(check (expt +inf.0+2.i 0.+0.i)	(=> eq=?) +nan.0+nan.0i)
(check (expt +2.+inf.0i 0.+0.i)	(=> eq=?) +nan.0+nan.0i)

(check (expt +nan.0+2.i 2.+4.i)	(=> eq=?) +nan.0+nan.0i)
(check (expt +2.+nan.0i 2.+4.i)	(=> eq=?) +nan.0+nan.0i)

(check (expt +nan.0+2.i 0.+0.i)	(=> eq=?) +nan.0+nan.0i)
(check (expt +2.+nan.0i 0.+0.i)	(=> eq=?) +nan.0+nan.0i)


;;;; done

(check-report)

;;; end of file
