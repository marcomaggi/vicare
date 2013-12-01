;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for infix library
;;;Date: Sat Aug 15, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(import (vicare)
  (vicare language-extensions infix)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: infix to prefix syntax\n")


;;;; numbers

(check (begin (infix) #f)	=> #f)

;;; integers

(check (infix 1)	=> 1)
(check (infix -1)	=> -1)
(check (infix +1)	=> 1)

;;; reals

(check (infix 1.1)		=> 1.1)
(check (infix -1.1)		=> -1.1)
(check (infix +1.1)		=> +1.1)
(check (infix 1.1e10)		=> 1.1e10)
(check (infix 1.1E10)		=> 1.1e10)
(check (infix 1.1e-10)		=> 1.1e-10)
(check (infix 1.1E-10)		=> 1.1e-10)
(check (infix 1e10)		=> 1e10)
(check (infix 1E10)		=> 1e10)
(check (infix 1e-10)		=> 1e-10)
(check (infix 1E-10)		=> 1e-10)

(check (infix .0)		=> 0.0)
(check (infix -.0)		=> -0.0)
(check (infix 0.)		=> 0.0)

;;; complexes

(check (infix +1i)		=> +1i)
(check (infix -1i)		=> -1i)
(check (infix +1.1i)		=> +1.1i)
(check (infix -1.1i)		=> -1.1i)
(check (infix +.1i)		=> +0.1i)
(check (infix -.1i)		=> -0.1i)

;;; nan and infinity

(check (nan? (infix +nan.0))	=> #t)
(check (nan? (infix -nan.0))	=> #t)
(check (infix +inf.0)		=> +inf.0)
(check (infix -inf.0)		=> -inf.0)


;;;; arithmetic operators

(check (infix 1 + 2)		=> (+ 1 2))
(check (infix 1 + 2 + 3)	=> (+ (+ 1 2) 3))
(check (infix 1 + 2 - 3)	=> (- (+ 1 2) 3))
(check (infix 1 + (2 + 3))	=> (+ 1 (+ 2 3)))
(check (infix 1 + (2 - 3))	=> (+ 1 (- 2 3)))

(check (infix 1 * 1)		=> (* 1 1))
(check (infix 1 * 2 * 3)	=> (* (* 1 2) 3))
(check (infix 1 * 2 / 3)	=> (/ (* 1 2) 3))
(check (infix 1 * (2 * 3))	=> (* 1 (* 2 3)))
(check (infix 1 * (2 / 3))	=> (* 1 (/ 2 3)))

(check (infix 1 + 2 * 3)	=> (+ 1 (* 2 3)))
(check (infix 1 - 2 * 3)	=> (- 1 (* 2 3)))
(check (infix 1 + 2 / 3)	=> (+ 1 (/ 2 3)))
(check (infix 1 - 2 / 3)	=> (- 1 (/ 2 3)))

(check (infix 1 * 2 + 3)	=> (+ (* 1 2) 3))
(check (infix 1 * 2 - 3)	=> (- (* 1 2) 3))
(check (infix 1 / 2 + 3)	=> (+ (/ 1 2) 3))
(check (infix 1 / 2 - 3)	=> (- (/ 1 2) 3))

(check (infix - 2)		=> (- 2))
(check (infix (- 2))		=> (- 2))
(check (infix (1 + (- 2)))	=> (+ 1 (- 2)))
(let ((a 2))
  (check (infix (- a))		=> (- 2))
  (check (infix (1 + (- a)))	=> (+ 1 (- 2)))
  #f)

(check (infix 1 % 3)		=> (mod 1 3))
(check (infix 10 mod 3)		=> (mod 10 3))
(check (infix 1 div 3)		=> (div 1 3))
(check (infix 1 expt 3)		=> (expt 1 3))
(check (infix 10 expt 3)	=> (expt 10 3))

;; EXPT is right-associative
(check (infix 10 expt 5 expt 3)	=> (expt 10 (expt 5 3)))

;;; --------------------------------------------------------------------

(check (infix incr! 10)	=> 11)
(check (infix 10 incr!)	=> 11)
(check (infix decr! 10)	=> 9)
(check (infix 10 decr!)	=> 9)

(check
    (let ((x 10))
      (let ((r (infix incr! x)))
	(list r x)))
  => '(11 11))

(check
    (let ((x 10))
      (let ((r (infix 2 + incr! x)))
	(list r x)))
  => '(13 11))

(check
    (let ((x 10))
      (let ((r (infix 2 + (incr! x))))
	(list r x)))
  => '(13 11))

(check
    (let ((x 10))
      (let ((r (infix x incr!)))
	(list r x)))
  => '(10 11))

(check
    (let ((x 10))
      (let ((r (infix decr! x)))
	(list r x)))
  => '(9 9))

(check
    (let ((x 10))
      (let ((r (infix x decr!)))
	(list r x)))
  => '(10 9))

;;; --------------------------------------------------------------------

(check (infix ++ 10)	=> 11)
(check (infix 10 ++)	=> 11)
(check (infix -- 10)	=> 9)
(check (infix 10 --)	=> 9)

(check
    (let ((x 10))
      (let ((r (infix ++ x)))
	(list r x)))
  => '(11 11))

(check
    (let ((x 10))
      (let ((r (infix 2 + ++ x)))
	(list r x)))
  => '(13 11))

(check
    (let ((x 10))
      (let ((r (infix 2 + (++ x))))
	(list r x)))
  => '(13 11))

(check
    (let ((x 10))
      (let ((r (infix x ++)))
	(list r x)))
  => '(10 11))

(check
    (let ((x 10))
      (let ((r (infix -- x)))
	(list r x)))
  => '(9 9))

(check
    (let ((x 10))
      (let ((r (infix x --)))
	(list r x)))
  => '(10 9))


;;;; comparison operators

(check (infix 1 < 3)		=> (<  1 3))
(check (infix 1 > 3)		=> (>  1 3))
(check (infix 1 <= 3)		=> (<= 1 3))
(check (infix 1 >= 3)		=> (>= 1 3))
(check (infix 1 = 3)		=> (=  1 3))

(check
    (infix 'a eq? 'a)
  => (eq? 'a 'a))

(check
    (infix 'a eq? 'b)
  => (eq? 'a 'b))

(check (infix 123 eqv? 123)	=> (eqv? 123 123))
(check (infix 123 eqv? 456)	=> (eqv? 123 456))

(check (infix 123 equal? 123)	=> (equal? 123 123))
(check (infix 123 equal? 456)	=> (equal? 123 456))


;;;; functions

(let ()

  (define (fun a b c)
    (+ a b c))

  (check
      (infix fun (1 2 3))
    => (fun 1 2 3))

  (check
      (infix fun(1 2 3))
    => (fun 1 2 3))

  (check
      (infix (fun (1 2 3)))
    => (fun 1 2 3))

  #f)

(check (infix sin (1.1))		=> (sin 1.1))

(check (infix cos (sin (1.1)))		=> (cos (sin 1.1)))
(check (infix cos (sin (1.1) + 4))	=> (cos (+ (sin 1.1) 4)))

(check
    (infix 1 + 23e-45 + 0.006789e2 * (4.113 + +23i) / sin (0.5) + atan (0.1 0.2))
  => (+ (+ (+ 1 23e-45) (/ (* 0.006789e2 (+ 4.113 +23i)) (sin 0.5))) (atan 0.1 0.2)))


;;;; variables

(let ((a 1) (b 2) (c 3))
  (check (infix a * 1.1)	=> (* a 1.1))
  (check (infix (a * b) / c)	=> (/ (* a b) c))
  (check (infix a * (b / c))	=> (* a (/ b c)))

  (check (infix cos (a) * tan (b) / c)
    => (/ (* (cos a) (tan b)) c))

  (check (infix (cos (a) * tan (b) / c))
    => (/ (* (cos a) (tan b)) c))

  #f)


;;;; if-then-else

(let ((a 1) (b 2) (c 3))

  (check (infix a ? b : c)	=> (if a b c))
  (check (infix (1 ? b : c))	=> (if 1 b c))
  (check (infix (a ? 1 : c))	=> (if a 1 c))
  (check (infix (a ? b : 1))	=> (if a b 1))
  (check (infix (1 ? 2 : 3))	=> (if 1 2 3))
  (check (infix #f ? 2 : 3)	=> (if #f 2 3))
  (check (infix (#f ? 2 : 3))	=> (if #f 2 3))

  (check (infix (a * (b / a ? b : c)))
    => (* a (if (/ b a) b c)))

  (check (infix (1 + a ? 2 + b : 3 + c - 4))
    => (if (+ 1 a) (+ 2 b) (- (+ 3 c) 4)))

  (check (infix a ? b : c)	=> (if a b c))

  #f)


;;;; nested prefix expressions

(check
    (infix (begin
	     (+ 1 2)))
  => (+ 1 2))

(check
    (infix (begin
	     (+ 1 2)
	     (+ 3 4)))
  => (+ 3 4))

(check
    (infix (begin
	     (let ((a 3))
	       (/ a 4))))
  => (/ 3 4))

(let ((a 3))
  (check
      (infix (begin
	       (/ a 4)))
    => (/ a 4)))

(check (infix (begin 1) + 2 * 3)	=> (+ 1 (* 2 3)))
(check (infix 1 - (begin 2) * 3)	=> (- 1 (* 2 3)))
(check (infix 1 + 2 / (begin 3))	=> (+ 1 (/ 2 3)))

(let ((a 1) (b 2) (c 3))
  (check (infix (1 + a ? (begin
			   (+ 2 b))
		   : 3 + c - 4))
    => (if (+ 1 a) (+ 2 b) (- (+ 3 c) 4)))
  #f)


;;;; logic operators

(check (infix 1 and 3)		=> 3)
(check (infix #f and 3)		=> #f)
(check (infix 1 and #f)		=> #f)
(check (infix 1 or 3)		=> 1)
(check (infix 1 or #f)		=> 1)
(check (infix #f or 1)		=> 1)
(check (infix 1 xor 3)		=> #f)
(check (infix 1 xor #f)		=> 1)
(check (infix #f xor 1)		=> 1)
(check (infix not 3)		=> #f)
(check (infix not #f)		=> #t)

(check (infix 1 && 3)		=> 3)
(check (infix 1 !! 3)		=> 1)
(check (infix 1 ^^ 3)		=> #f)

(check (infix ~~ 3)		=> #f)
(check (infix ~~ #f)		=> #t)


;;;; bitwise operators

(let ((a #b0101) (b #b1111))
  (check
      (infix a & b)
    => (bitwise-and a b)))

(let ((a #b0101) (b #b1101))
  (check
      (infix a ! b)
    => (bitwise-ior a b)))

(let ((a #b0111) (b #b1101))
  (check
      (infix a ^ b)
    => (bitwise-xor a b)))

(let ((a #b0101))
  (check
      (infix ~ a)
    => (bitwise-not a)))

(let ((a #b0111) (b 3))
  (check
      (infix a << b)
    => (bitwise-arithmetic-shift-left a b)))

(let ((a #b01110000) (b 3))
  (check
      (infix a >> b)
    => (bitwise-arithmetic-shift-right a b)))


;;;; fixnums

;;; arithmetic operators

(check (infix 1 fx+ 2)		=> (fx+ 1 2))
(check (infix 1 fx+ 2 fx+ 3)		=> (fx+ (fx+ 1 2) 3))
(check (infix 1 fx+ 2 fx- 3)		=> (fx- (fx+ 1 2) 3))
(check (infix 1 fx+ (2 fx+ 3))	=> (fx+ 1 (fx+ 2 3)))
(check (infix 1 fx+ (2 fx- 3))	=> (fx+ 1 (fx- 2 3)))

(check (infix 1 fx* 1)		=> (fx* 1 1))
(check (infix 1 fx* 2 fx* 3)		=> (fx* (fx* 1 2) 3))
(check (infix 1 fx* (2 fx* 3))	=> (fx* 1 (fx* 2 3)))

(check (infix 1 fx+ 2 fx* 3)		=> (fx+ 1 (fx* 2 3)))
(check (infix 1 fx- 2 fx* 3)		=> (fx- 1 (fx* 2 3)))

(check (infix 1 fx* 2 fx+ 3)		=> (fx+ (fx* 1 2) 3))
(check (infix 1 fx* 2 fx- 3)		=> (fx- (fx* 1 2) 3))

(check (infix fx- 2)			=> (fx- 2))
(check (infix (fx- 2))		=> (fx- 2))
(check (infix (1 fx+ (fx- 2)))	=> (fx+ 1 (fx- 2)))
(let ((a 2))
  (check (infix (fx- a))		=> (fx- 2))
  (check (infix (1 fx+ (fx- a)))	=> (fx+ 1 (fx- 2)))
  #f)

(check (infix 1 fxdiv 3)		=> (fxdiv 1 3))
(check (infix 1 fxmod 3)		=> (fxmod 1 3))
(check (infix 10 fxmod 3)		=> (fxmod 10 3))

;;; associativity

(check (infix 10 fx- 5 fx- 3)		=> (fx- (fx- 10 5) 3))
(check (infix 10 fxdiv 5 fxdiv 3)	=> (fxdiv (fxdiv 10 5) 3))
(check (infix 10 fxdiv0 5 fxdiv0 3)	=> (fxdiv0 (fxdiv0 10 5) 3))

;;; comparison operators

(check (infix 1 fx<? 3)		=> (fx<?  1 3))
(check (infix 1 fx>? 3)		=> (fx>?  1 3))
(check (infix 1 fx<=? 3)		=> (fx<=? 1 3))
(check (infix 1 fx>=? 3)		=> (fx>=? 1 3))
(check (infix 1 fx=? 3)		=> (fx=?  1 3))

;;; bitwise operators

(let ((a #b0101) (b #b1111))
  (check (infix a fx& b)	=> (fxand a b)))

(let ((a #b0101) (b #b1101))
  (check (infix a fx! b)	=> (fxior a b)))

(let ((a #b0111) (b #b1101))
  (check (infix a fx^ b)	=> (fxxor a b)))

(let ((a #b0101))
  (check (infix fx~ a)	=> (fxnot a)))

(let ((a #b0111) (b 3))
  (check (infix a fx<< b)	=> (fxarithmetic-shift-left a b)))

(let ((a #b01110000) (b 3))
  (check (infix a fx>> b)	=> (fxarithmetic-shift-right a b)))


;;;; flonums

;;; arithmetic operators

(check (infix 1. fl+ 2.)		=> (fl+ 1. 2.))
(check (infix 1. fl+ 2. fl+ 3.)		=> (fl+ (fl+ 1. 2.) 3.))
(check (infix 1. fl+ 2. fl- 3.)		=> (fl- (fl+ 1. 2.) 3.))
(check (infix 1. fl+ (2. fl+ 3.))	=> (fl+ 1. (fl+ 2. 3.)))
(check (infix 1. fl+ (2. fl- 3.))	=> (fl+ 1. (fl- 2. 3.)))

(check (infix 1. fl* 1.)		=> (fl* 1. 1.))
(check (infix 1. fl* 2. fl* 3.)		=> (fl* (fl* 1. 2.) 3.))
(check (infix 1. fl* 2. fl/ 3.)		=> (fl/ (fl* 1. 2.) 3.))
(check (infix 1. fl* (2. fl* 3.))	=> (fl* 1. (fl* 2. 3.)))
(check (infix 1. fl* (2. fl/ 3.))	=> (fl* 1. (fl/ 2. 3.)))

(check (infix 1. fl+ 2. fl* 3.)		=> (fl+ 1. (fl* 2. 3.)))
(check (infix 1. fl- 2. fl* 3.)		=> (fl- 1. (fl* 2. 3.)))
(check (infix 1. fl+ 2. fl/ 3.)		=> (fl+ 1. (fl/ 2. 3.)))
(check (infix 1. fl- 2. fl/ 3.)		=> (fl- 1. (fl/ 2. 3.)))

(check (infix 1. fl* 2. fl+ 3.)		=> (fl+ (fl* 1. 2.) 3.))
(check (infix 1. fl* 2. fl- 3.)		=> (fl- (fl* 1. 2.) 3.))
(check (infix 1. fl/ 2. fl+ 3.)		=> (fl+ (fl/ 1. 2.) 3.))
(check (infix 1. fl/ 2. fl- 3.)		=> (fl- (fl/ 1. 2.) 3.))

;; left associativity
(check (infix 10. fl- 5. fl- 3.)	=> (fl- (fl- 10. 5.) 3.))
(check (infix 10. fl- 5. fl- 3.)	=> (fl- 10. 5. 3.))
(check (infix 10. fl/ 5. fl/ 3.)	=> (fl/ (fl/ 10. 5.) 3.))
(check (infix 10. fl/ 5. fl/ 3.)	=> (fl/ 10. 5. 3.))

(check (infix fl- 2.)			=> (fl- 2.))
(check (infix (fl- 2.))			=> (fl- 2.))
(check (infix (1. fl+ (fl- 2.)))	=> (fl+ 1. (fl- 2.)))
(let ((a 2.))
  (check (infix (fl- a))		=> (fl- 2.))
  (check (infix (1. fl+ (fl- a)))	=> (fl+ 1. (fl- 2.)))
  #f)

(check (infix 1.0 flexpt 3.)		=> (flexpt 1.0 3.))
;; FLEXPT is right-associative
(check (infix 10. flexpt 5. flexpt 3.)	=> (flexpt 10. (flexpt 5. 3.)))

;;; comparison operators

(check (infix 1. fl<? 3.)		=> (fl<?  1. 3.))
(check (infix 1. fl>? 3.)		=> (fl>?  1. 3.))
(check (infix 1. fl<=? 3.)		=> (fl<=? 1. 3.))
(check (infix 1. fl>=? 3.)		=> (fl>=? 1. 3.))
(check (infix 1. fl=? 3.)		=> (fl=?  1. 3.))


;;;; precedence

;;;In order of increasing precedence from left to right:
;;;
;;; + - * / div div0 mod mod0 expt < > <= >= =


(check (infix 11 + 22 * 33)		=> (+ 11 (* 22 33)))
(check (infix 11 - 22 * 33)		=> (- 11 (* 22 33)))
(check (infix 22 * 33 + 11)		=> (+ (* 22 33) 11))
(check (infix 22 * 33 - 11)		=> (- (* 22 33) 11))

(check (infix 11 + 22 / 33)		=> (+ 11 (/ 22 33)))
(check (infix 11 - 22 / 33)		=> (- 11 (/ 22 33)))
(check (infix 22 / 33 + 11)		=> (+ (/ 22 33) 11))
(check (infix 22 / 33 - 11)		=> (- (/ 22 33) 11))

;;; --------------------------------------------------------------------

(check (infix 11 + 22 div 33)		=> (+ 11 (div 22 33)))
(check (infix 11 - 22 div 33)		=> (- 11 (div 22 33)))
(check (infix 22 div 33 + 11)		=> (+ (div 22 33) 11))
(check (infix 22 div 33 - 11)		=> (- (div 22 33) 11))

(check (infix 11 + 22 mod 33)		=> (+ 11 (mod 22 33)))
(check (infix 11 - 22 mod 33)		=> (- 11 (mod 22 33)))
(check (infix 22 mod 33 + 11)		=> (+ (mod 22 33) 11))
(check (infix 22 mod 33 - 11)		=> (- (mod 22 33) 11))

;;; --------------------------------------------------------------------

(check (infix 11 + 22 div0 33)		=> (+ 11 (div0 22 33)))
(check (infix 11 - 22 div0 33)		=> (- 11 (div0 22 33)))
(check (infix 22 div0 33 + 11)		=> (+ (div0 22 33) 11))
(check (infix 22 div0 33 - 11)		=> (- (div0 22 33) 11))

(check (infix 11 + 22 mod0 33)		=> (+ 11 (mod0 22 33)))
(check (infix 11 - 22 mod0 33)		=> (- 11 (mod0 22 33)))
(check (infix 22 mod0 33 + 11)		=> (+ (mod0 22 33) 11))
(check (infix 22 mod0 33 - 11)		=> (- (mod0 22 33) 11))

;;MUL and  DIV categories have the  same precedence, so it  is left to
;;right here:
(check (infix 11 * 22 div0 33)		=> (div0 (* 11 22) 33))
(check (infix 11 / 22 div0 33)		=> (div0 (/ 11 22) 33))
(check (infix 22 div0 33 * 11)		=> (* (div0 22 33) 11))
(check (infix 22 div0 33 / 11)		=> (/ (div0 22 33) 11))

(check (infix 11 * 22 mod0 33)		=> (* 11 (mod0 22 33)))
(check (infix 11 / 22 mod0 33)		=> (/ 11 (mod0 22 33)))
(check (infix 22 mod0 33 * 11)		=> (* (mod0 22 33) 11))
(check (infix 22 mod0 33 / 11)		=> (/ (mod0 22 33) 11))

;;; --------------------------------------------------------------------

(check (infix 11 + 22 expt 33)	=> (+ 11 (expt 22 33)))
(check (infix 11 - 22 expt 33)	=> (- 11 (expt 22 33)))
(check (infix 22 expt 33 + 11)	=> (+ (expt 22 33) 11))
(check (infix 22 expt 33 - 11)	=> (- (expt 22 33) 11))

(check (infix 11 * 22 expt 33)	=> (* 11 (expt 22 33)))
(check (infix 11 / 22 expt 33)	=> (/ 11 (expt 22 33)))
(check (infix 22 expt 33 * 11)	=> (* (expt 22 33) 11))
(check (infix 22 expt 33 / 11)	=> (/ (expt 22 33) 11))

(check (infix 11 div 22 expt 33)	=> (div 11 (expt 22 33)))
(check (infix 11 mod 22 expt 33)	=> (mod 11 (expt 22 33)))
(check (infix 22 expt 33 div 11)	=> (div (expt 22 33) 11))
(check (infix 22 expt 33 mod 11)	=> (mod (expt 22 33) 11))

;;; --------------------------------------------------------------------

(check (infix 11 + 22 & 33)	=> (+ 11 (& 22 33)))
(check (infix 11 - 22 & 33)	=> (- 11 (& 22 33)))
(check (infix 22 & 33 + 11)	=> (+ (& 22 33) 11))
(check (infix 22 & 33 - 11)	=> (- (& 22 33) 11))

(check (infix 11 + 22 ! 33)	=> (+ 11 (! 22 33)))
(check (infix 11 - 22 ! 33)	=> (- 11 (! 22 33)))
(check (infix 22 ! 33 + 11)	=> (+ (! 22 33) 11))
(check (infix 22 ! 33 - 11)	=> (- (! 22 33) 11))

(check (infix 11 + 22 ^ 33)	=> (+ 11 (^ 22 33)))
(check (infix 11 - 22 ^ 33)	=> (- 11 (^ 22 33)))
(check (infix 22 ^ 33 + 11)	=> (+ (^ 22 33) 11))
(check (infix 22 ^ 33 - 11)	=> (- (^ 22 33) 11))

(check (infix 1 & 2 << 3)	=> (& 1 (<< 2 3)))
(check (infix 1 ! 2 << 3)	=> (! 1 (<< 2 3)))
(check (infix 2 << 3 & 1)	=> (& (<< 2 3) 1))
(check (infix 2 << 3 ! 1)	=> (! (<< 2 3) 1))


;;;; done

(check-report)

;;; end of file
